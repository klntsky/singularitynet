module UnbondedStaking.DepositPool
  ( depositUnbondedPoolContract
  , updateEntryTx
  , updateEntriesList
  , updateOutdatedEntriesList
  ) where

import Contract.Prelude

import Contract.Address
  ( getNetworkId
  , getWalletAddress
  , ownPaymentPubKeyHash
  , scriptHashAddress
  )
import Contract.Log (logInfo', logWarn')
import Contract.Monad
  ( Contract
  , liftContractM
  , liftedE'
  , liftedM
  , throwContractError
  )
import Contract.Numeric.Natural (Natural, fromBigInt')
import Contract.Numeric.Rational (Rational)
import Contract.PlutusData
  ( PlutusData
  , Datum(Datum)
  , Redeemer(Redeemer)
  , fromData
  , getDatumByHash
  , toData
  )
import Contract.Prim.ByteArray (ByteArray)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (ValidatorHash, validatorHash)
import Contract.Transaction (TransactionInput, TransactionOutputWithRefScript)
import Contract.TxConstraints
  ( TxConstraints
  , mustBeSignedBy
  , mustSpendScriptOutput
  , mustValidateIn
  )
import Contract.Utxos (getWalletUtxos, utxosAt)
import Contract.Value (mkTokenName, singleton)
import Control.Applicative (unless)
import Ctl.Internal.Plutus.Conversion (fromPlutusAddress)
import Data.Array ((:))
import Data.Array as Array
import Data.Array.NonEmpty as NonEmpty
import Data.BigInt (BigInt)
import Data.Unfoldable (none)
import Partial.Unsafe (unsafePartial)
import Scripts.PoolValidator (mkUnbondedPoolValidator)
import Settings
  ( unbondedStakingTokenName
  , confirmationTimeout
  , submissionAttempts
  )
import Types (ScriptVersion)
import UnbondedStaking.Types
  ( Entry(Entry)
  , IncompleteDeposit(..)
  , UnbondedPoolParams(UnbondedPoolParams)
  , UnbondedStakingAction(AdminAct)
  , UnbondedStakingDatum(AssetDatum, EntryDatum, StateDatum)
  )
import UnbondedStaking.Utils (calculateRewards, getAdminTime, getListDatums)
import Utils
  ( getUtxoDatumHash
  , getUtxoWithNFT
  , logInfo_
  , mkOnchainAssocList
  , mustPayToScript
  , roundUp
  , splitByLength
  , submitBatchesSequentially
  , submitTransaction
  , toIntUnsafe
  , toRational
  )

-- | Deposits a certain amount in the pool
-- If the `batchSize` is zero, then funds will be deposited to all users.
-- Otherwise the transactions will be made in batches
-- If `IncompleteDeposit` is provided, then reward deposits will be made only
-- users whose entries are outdated since the last cycle.
depositUnbondedPoolContract
  :: BigInt
  -> UnbondedPoolParams
  -> ScriptVersion
  -> Natural
  -> Maybe IncompleteDeposit
  -> Contract () (Maybe IncompleteDeposit)
depositUnbondedPoolContract
  depositAmt
  params@
    ( UnbondedPoolParams
        { admin
        , nftCs
        , assocListCs
        }
    )
  scriptVersion
  batchSize
  incompleteDeposit' = do
  -- Fetch information related to the pool
  -- Get network ID and check admin's PKH
  networkId <- getNetworkId
  userPkh <- liftedM "depositUnbondedPoolContract: Cannot get user's pkh"
    ownPaymentPubKeyHash
  unless (userPkh == admin) $ throwContractError
    "depositUnbondedPoolContract: Admin is not current user"
  logInfo_ "depositUnbondedPoolContract: Admin PaymentPubKeyHash" userPkh
  -- Get utxos at the wallet address
  adminUtxos <-
    liftedM "depositUnbondedPoolContract: Could not get wallet's utxos" $
      getWalletUtxos
  -- Get the unbonded pool validator and hash
  validator <- liftedE' "depositUnbondedPoolContract: Cannot create validator"
    $ mkUnbondedPoolValidator params scriptVersion
  let valHash = validatorHash validator
  logInfo_ "depositUnbondedPoolContract: validatorHash" valHash
  let poolAddr = scriptHashAddress valHash Nothing
  logInfo_ "depositUnbondedPoolContract: Pool address"
    $ fromPlutusAddress networkId poolAddr
  -- Get the unbonded pool's utxo
  unbondedPoolUtxos <- utxosAt poolAddr
  logInfo_ "depositUnbondedPoolContract: Pool UTXOs" unbondedPoolUtxos
  tokenName <- liftContractM
    "depositUnbondedPoolContract: Cannot create TokenName"
    unbondedStakingTokenName
  poolTxInput /\ poolTxOutput <-
    liftContractM "depositUnbondedPoolContract: Cannot get state utxo"
      $ getUtxoWithNFT unbondedPoolUtxos nftCs tokenName
  logInfo_ "depositUnbondedPoolContract: Pool's UTXO" poolTxInput
  poolDatumHash <-
    liftContractM
      "depositUnbondedPoolContract: Could not get Pool UTXO's Datum Hash"
      $ getUtxoDatumHash poolTxOutput
  logInfo_ "depositUnbondedPoolContract: Pool's UTXO DatumHash" poolDatumHash
  poolDatum <- liftedM "depositUnbondedPoolContract: Cannot get datum"
    $ getDatumByHash poolDatumHash
  unbondedStakingDatum :: UnbondedStakingDatum <-
    liftContractM
      "depositUnbondedPoolContract: Cannot extract NFT State datum"
      $ fromData (unwrap poolDatum)
  -- Get the validitiy range to use
  logInfo' "depositUnbondedPoolContract: Getting admin range..."
  { currTime, range } <- getAdminTime params scriptVersion
  logInfo_ "depositUnbondedPoolContract: Current time: " $ show currTime
  logInfo_ "depositUnbondedPoolContract: TX Range" range
  -- Update the association list
  case unbondedStakingDatum of
    -- Non-empty user list
    StateDatum { maybeEntryName: Just _, open: true } -> do
      logInfo'
        "depositUnbondedPoolContract: STAKE TYPE - StateDatum is \
        \StateDatum { maybeEntryName: Just ..., open: true }"

      -- Obtain the on-chain association list. Note that if a previous deposit
      -- failed, some of these entries might be up to date while others are not.
      let
        assocList
          :: Array
               (ByteArray /\ TransactionInput /\ TransactionOutputWithRefScript)
        assocList = mkOnchainAssocList assocListCs unbondedPoolUtxos

      -- Obtain the entries' inputs and datums and generate the updated versions
      -- of the datums.
      -- If an `IncompleteDeposit` value is provided, only the given
      -- subset of the entries is updated.
      (entriesInputs /\ entriesDatums /\ updatedEntriesDatums) <-
        case incompleteDeposit' of
          Nothing -> do
            -- Use all entries
            let
              entriesInputs :: Array TransactionInput
              entriesInputs = map (fst <<< snd) assocList
            entriesDatums :: Array Entry <- getListDatums assocList
            -- Assign rewards and update fields for next cycle
            let
              updatedEntriesDatums :: Array Entry
              updatedEntriesDatums = updateEntriesList depositAmt entriesDatums
            pure $ entriesInputs /\ entriesDatums /\ updatedEntriesDatums
          Just (IncompleteDeposit incompDeposit) -> do
            -- Use only the entries with the given keys
            let
              assocList'
                :: Array
                     ( ByteArray /\ TransactionInput /\
                         TransactionOutputWithRefScript
                     )
              assocList' = Array.filter
                (\(ba /\ _) -> Array.elem ba incompDeposit.failedKeys)
                assocList
              entriesInputs = map (fst <<< snd) assocList'
            entriesDatums <- getListDatums assocList'
            -- Print warning if some of the failed keys were not found in the pool
            let
              keysNotFound :: Array ByteArray
              keysNotFound =
                Array.difference
                  (fst <$> assocList)
                  incompDeposit.failedKeys
            when (Array.length keysNotFound /= 0)
              $ logWarn'
              $ show (Array.length keysNotFound)
              <> " of the outdated entries were not found in the pool: "
              <> show keysNotFound
            -- Assign rewards and update fields to outdated entries
            let
              updatedEntriesDatums =
                updateOutdatedEntriesList
                  incompDeposit.nextDepositAmt
                  incompDeposit.totalDeposited
                  entriesDatums
            pure $ entriesInputs /\ entriesDatums /\ updatedEntriesDatums

      -- Generate constraints/lookups for updating each entry
      entryUpdates
        :: Array ((TxConstraints Unit Unit) /\ (ScriptLookups PlutusData)) <-
        traverse (updateEntryTx params valHash)
          ( Array.zip entriesInputs
              (Array.zip entriesDatums updatedEntriesDatums)
          )

      let
        constraints :: TxConstraints Unit Unit
        constraints =
          mustBeSignedBy admin
            <> mustValidateIn range

        lookups :: ScriptLookups.ScriptLookups PlutusData
        lookups =
          ScriptLookups.validator validator
            <> ScriptLookups.unspentOutputs unbondedPoolUtxos
      -- We deliberately omit the admin utxos, since batching includes
      -- them automatically before every batch.

      -- Submit transaction with possible batching
      failedDeposits <-
        if batchSize == zero then
          submitTransaction
            constraints
            (lookups <> ScriptLookups.unspentOutputs adminUtxos)
            confirmationTimeout
            submissionAttempts
            entryUpdates
        else do
          let
            entryUpdateBatches = splitByLength (toIntUnsafe batchSize)
              entryUpdates
          submitBatchesSequentially
            constraints
            lookups
            confirmationTimeout
            submissionAttempts
            entryUpdateBatches
      logInfo_
        "depositUnbondedPoolContract: Finished updating pool entries. /\
        \Entries with failed updates"
        failedDeposits
      -- We obtain the failed entries and return them as part of a
      -- `IncompleteDeposit` (if there are any)
      pure do
        indices <- NonEmpty.fromArray =<< traverse
          (flip Array.elemIndex entryUpdates)
          failedDeposits
        let
          entries = foldl
            ( \acc i -> unsafePartial $ Array.unsafeIndex updatedEntriesDatums i
                : acc
            )
            mempty
            indices
        firstEntry <- Array.head entries
        Just $ IncompleteDeposit
          { failedKeys: map (unwrap >>> _.key) entries
          , totalDeposited: unwrap >>> _.totalDeposited $ firstEntry
          , nextDepositAmt: depositAmt
          }
    -- Other error cases:
    StateDatum { maybeEntryName: Nothing, open: true } ->
      throwContractError
        "depositUnbondedPoolContract: There are no users in the pool to \
        \deposit rewards for"
    StateDatum { maybeEntryName: _, open: false } ->
      throwContractError
        "depositUnbondedPoolContract: Cannot deposit to a closed pool"
    _ ->
      throwContractError "depositUnbondedPoolContract: Datum incorrect type"

-- | Updates all entries in two passes. We need two passes because all
-- entries must contain the `totalDeposited` amount in the pool, which requires
-- traversing the array at least once.
updateEntriesList
  :: BigInt
  -> Array Entry
  -> Array Entry
updateEntriesList nextDepositAmt entries =
  uncurry updateTotalDeposited $ updateRewards nextDepositAmt entries

-- | Updates a list of outdated entries. It uses the given `totalDeposited`
-- to update the entries. This is in contrast to
-- `updateEntriesList`, which assumes the array of entries to conform the
-- entirety of the pool and calculates the `totalDeposited` itself.
updateOutdatedEntriesList
  :: BigInt
  -> BigInt
  -> Array Entry
  -> Array Entry
updateOutdatedEntriesList nextDepositAmt totalDeposited =
  map \entry@(Entry e) ->
    Entry $ e
      { newDeposit = zero
      , rewards = calculateRewards entry
      , totalRewards = nextDepositAmt
      , totalDeposited = totalDeposited
      }

-- | Assign rewards to each entry, set `newDeposit` to zero, set next
-- cycle rewards. Sum all deposits/rewards.
updateRewards :: BigInt -> Array Entry -> (Rational /\ Array Entry)
updateRewards nextDepositAmt entries = foldl update (zero /\ none) entries
  where
  update :: (Rational /\ Array Entry) -> Entry -> (Rational /\ Array Entry)
  update (accDeposits /\ arr) entry@(Entry e) =
    let
      rewards' :: Rational
      rewards' = calculateRewards entry

      entry' :: Entry
      entry' = Entry $ e
        { newDeposit = zero, rewards = rewards', totalRewards = nextDepositAmt }
    in
      (accDeposits + toRational e.deposited + rewards') /\ Array.snoc arr entry'

-- | Set `totalDeposited` in all entries.
updateTotalDeposited :: Rational -> Array Entry -> Array Entry
updateTotalDeposited totalDeposited =
  map (\(Entry e) -> Entry $ e { totalDeposited = roundUp totalDeposited })

-- | Creates the constraints and lookups for submitting an entry update. It
-- takes both the old and updated version of the `Entry`.
updateEntryTx
  :: UnbondedPoolParams
  -> ValidatorHash
  -> (TransactionInput /\ Entry /\ Entry)
  -> Contract ()
       ( Tuple (TxConstraints Unit Unit)
           (ScriptLookups.ScriptLookups PlutusData)
       )
updateEntryTx
  (UnbondedPoolParams ubp)
  valHash
  (txInput /\ (Entry e) /\ (Entry e')) = do
  -- Get the token name for the user by hashing the key
  assocListTn <-
    liftContractM
      "updateEntryTx: Could not create token name for user"
      $ mkTokenName e.key
  -- Build datums and redeemers
  let
    assetDatum = Datum $ toData AssetDatum
    assetParams = unwrap ubp.unbondedAssetClass
    assetCs = assetParams.currencySymbol
    assetTn = assetParams.tokenName
    -- The difference between the old and new `rewards` is what the user
    -- has earnt in the previous cycle and what we need to deposit now.
    rewardsDifference = e'.rewards - e.rewards
    assetValue = singleton assetCs assetTn $ roundUp rewardsDifference
    entryValue = singleton ubp.assocListCs assocListTn one
    entryDatum = Datum $ toData $ EntryDatum { entry: Entry e' }
    valRedeemer = Redeemer $ toData $ AdminAct
      { totalRewards: fromBigInt' $ e'.totalRewards
      , totalDeposited: fromBigInt' $ e'.totalDeposited
      }

    -- Build constraints and lookups
    constraints :: TxConstraints Unit Unit
    constraints =
      mconcat
        [
          -- We don't generate an Asset UTxO if not necessary. This is the case
          -- when a user has just staked in the pool and now rewards have been
          -- accrued.
          if rewardsDifference == zero then mempty
          else mustPayToScript valHash assetDatum assetValue
        , mustPayToScript valHash entryDatum entryValue
        , mustSpendScriptOutput txInput valRedeemer
        ]
  let entryDatumLookup = ScriptLookups.datum entryDatum
  pure (constraints /\ entryDatumLookup)

