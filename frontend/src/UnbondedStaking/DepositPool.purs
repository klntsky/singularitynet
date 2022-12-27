module UnbondedStaking.DepositPool (depositUnbondedPoolContract) where

import Contract.Prelude

import Contract.Address
  ( getNetworkId
  , getWalletAddress
  , ownPaymentPubKeyHash
  , scriptHashAddress
  )
import Contract.Log (logInfo')
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
import Contract.Utxos (utxosAt)
import Contract.Value (mkTokenName, singleton)
import Control.Applicative (unless)
import Data.Array (elemIndex, zip)
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.Unfoldable (none)
import Ctl.Internal.Plutus.Conversion (fromPlutusAddress)
import Scripts.PoolValidator (mkUnbondedPoolValidator)
import Settings
  ( unbondedStakingTokenName
  , confirmationTimeout
  , submissionAttempts
  )
import UnbondedStaking.Types
  ( Entry(Entry)
  , UnbondedPoolParams(UnbondedPoolParams)
  , UnbondedStakingAction(AdminAct)
  , UnbondedStakingDatum(AssetDatum, EntryDatum, StateDatum)
  )
import UnbondedStaking.Utils (calculateRewards, getAdminTime)
import Utils
  ( getUtxoWithNFT
  , mkOnchainAssocList
  , logInfo_
  , roundUp
  , splitByLength
  , submitTransaction
  , toIntUnsafe
  , mustPayToScript
  , getUtxoDatumHash
  , toRational
  )

-- | Deposits a certain amount in the pool
-- | If the `batchSize` is zero, then funds will be deposited to all users.
-- | Otherwise the transactions will be made in batches
-- | If the `depositList` is empty, then reward deposits will be made to all
-- | users. Otherwise only the users within in the list will have rewards
-- | deposited.
depositUnbondedPoolContract
  :: BigInt
  -> UnbondedPoolParams
  -> Natural
  -> Array Int
  -> Contract () (Array Int)
depositUnbondedPoolContract
  depositAmt
  params@
    ( UnbondedPoolParams
        { admin
        , nftCs
        , assocListCs
        }
    )
  batchSize
  _depositList = do
  -- Fetch information related to the pool
  -- Get network ID and check admin's PKH
  networkId <- getNetworkId
  userPkh <- liftedM "depositUnbondedPoolContract: Cannot get user's pkh"
    ownPaymentPubKeyHash
  unless (userPkh == admin) $ throwContractError
    "depositUnbondedPoolContract: Admin is not current user"
  logInfo_ "depositUnbondedPoolContract: Admin PaymentPubKeyHash" userPkh
  -- Get the (Nami) wallet address
  adminAddr <-
    liftedM "depositUnbondedPoolContract: Cannot get wallet Address"
      getWalletAddress
  -- Get utxos at the wallet address
  adminUtxos <- utxosAt adminAddr
  -- Get the unbonded pool validator and hash
  validator <- liftedE' "depositUnbondedPoolContract: Cannot create validator"
    $ mkUnbondedPoolValidator params
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
  { currTime, range } <- getAdminTime params
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
      -- failed, some of these entries might be updated while the others are not.
      let
        assocList = mkOnchainAssocList assocListCs unbondedPoolUtxos

        entriesInputs :: Array TransactionInput
        entriesInputs = map (fst <<< snd) assocList

      -- Get datums for entries in the list
      entriesDatums :: Array Entry <- getListDatums assocList

      -- Assign rewards and update fields for next cycle
      let
        updatedEntriesDatums :: Array Entry
        updatedEntriesDatums = updateEntriesList depositAmt entriesDatums

      -- Generate constraints/lookups for updating each entry
      -- TODO: Take into account depositList for completing partial updates
      entryUpdates
        :: Array ((TxConstraints Unit Unit) /\ (ScriptLookups PlutusData)) <-
        traverse (updateEntryTx params valHash)
          (zip entriesInputs (zip entriesDatums updatedEntriesDatums))

      -- updateList <-
      --   if null depositList then
      --     traverse (mkEntryUpdateList depositAmt params valHash) assocList
      --   else do
      --     constraintsLookupsList <-
      --       traverse (mkEntryUpdateList depositAmt params valHash) assocList
      --     liftContractM
      --       "depositUnbondedPoolContract: Failed to create updateList'" $
      --       traverse ((!!) constraintsLookupsList) depositList

      let
        constraints :: TxConstraints Unit Unit
        constraints =
          mustBeSignedBy admin
            <> mustValidateIn range

        lookups :: ScriptLookups.ScriptLookups PlutusData
        lookups =
          ScriptLookups.validator validator
            <> ScriptLookups.unspentOutputs unbondedPoolUtxos
            <> ScriptLookups.unspentOutputs adminUtxos

        submitBatch
          :: Array
               ( Tuple
                   (TxConstraints Unit Unit)
                   (ScriptLookups.ScriptLookups PlutusData)
               )
          -> Contract ()
               ( Array
                   ( Tuple
                       (TxConstraints Unit Unit)
                       (ScriptLookups.ScriptLookups PlutusData)
                   )
               )
        submitBatch txBatch = do
          submitTransaction constraints lookups txBatch confirmationTimeout
            submissionAttempts

      -- Submit transaction with possible batching
      failedDeposits <-
        if batchSize == zero then
          submitBatch entryUpdates
        else do
          let updateBatches = splitByLength (toIntUnsafe batchSize) entryUpdates
          failedDeposits' <- traverse submitBatch updateBatches
          pure $ mconcat failedDeposits'
      logInfo_
        "depositUnbondedPoolContract: Finished updating pool entries. /\
        \Entries with failed updates"
        failedDeposits
      failedDepositsIndicies <-
        liftContractM
          "depositUnbondedPoolContract: Failed to create /\
          \failedDepositsIndicies list" $
          traverse (flip elemIndex entryUpdates) failedDeposits
      pure failedDepositsIndicies
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

-- | Get all entries' datums
getListDatums
  :: Array (ByteArray /\ TransactionInput /\ TransactionOutputWithRefScript)
  -> Contract () (Array Entry)
getListDatums arr = for arr \(_ /\ _ /\ txOut) -> do
  -- Get the entry's datum
  dHash <-
    liftContractM
      "getListDatums: Could not get entry's datum hash"
      $ getUtxoDatumHash txOut
  dat <-
    liftedM
      "getListDatums: Cannot get entry's datum" $ getDatumByHash dHash
  -- Parse it
  unbondedListDatum :: UnbondedStakingDatum <-
    liftContractM
      "getListDatums: Cannot parse entry's datum"
      $ fromData (unwrap dat)
  -- The get the entry datum
  case unbondedListDatum of
    EntryDatum { entry } -> pure entry
    StateDatum _ ->
      throwContractError
        "getListDatums: Expected a list datum but found a state datum"
    AssetDatum ->
      throwContractError
        "getListDatums: Expected an list datum but found an asset datum"

-- | Updates all entries in two passes. We need two passes because all
-- entries must contain the `totalDeposited` amount in the pool, which requires
-- traversing the array at least once.
updateEntriesList
  :: BigInt
  -> Array Entry
  -> Array Entry
updateEntriesList nextDepositAmt entries =
  uncurry updateTotalDeposited $ updateRewards nextDepositAmt entries

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
  entryDatumLookup <-
    liftContractM
      "updateEntryTx: Could not create state datum lookup"
      $ ScriptLookups.datum entryDatum
  pure (constraints /\ entryDatumLookup)

