module UnbondedStaking.ClosePool (closeUnbondedPoolContract) where

import Contract.Prelude

import Contract.Address
  ( getNetworkId
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
import Contract.Numeric.Natural (Natural)
import Contract.PlutusData
  ( PlutusData
  , Redeemer(Redeemer)
  , getDatumByHash
  , toData
  )
import Contract.Prim.ByteArray (ByteArray)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (validatorHash)
import Contract.Transaction (TransactionInput, TransactionOutputWithRefScript)
import Contract.TxConstraints
  ( TxConstraints
  , mustBeSignedBy
  , mustIncludeDatum
  , mustSpendScriptOutput
  , mustValidateIn
  )
import Contract.Utxos (getWalletUtxos, utxosAt)
import Control.Alt ((<|>))
import Control.Monad.Maybe.Trans (MaybeT(..), lift, runMaybeT)
import Ctl.Internal.Plutus.Conversion (fromPlutusAddress)
import Data.Array ((:))
import Data.Array as Array
import Data.Array.NonEmpty as NonEmpty
import Data.BigInt (BigInt)
import Partial.Unsafe (unsafePartial)
import Scripts.PoolValidator (mkUnbondedPoolValidator)
import Settings
  ( unbondedStakingTokenName
  , confirmationTimeout
  , submissionAttempts
  )
import Types (ScriptVersion)
import UnbondedStaking.DepositPool
  ( updateEntriesList
  , updateEntryTx
  , updateOutdatedEntriesList
  )
import UnbondedStaking.Types
  ( Entry(Entry)
  , IncompleteClose(..)
  , UnbondedPoolParams(UnbondedPoolParams)
  , UnbondedStakingAction(AdminAct)
  )
import UnbondedStaking.Utils (getAdminTime, getListDatums)
import Utils
  ( getUtxoDatumHash
  , getUtxoWithNFT
  , logInfo_
  , mkOnchainAssocList
  , splitByLength
  , submitBatchesSequentially
  , submitTransaction
  , toIntUnsafe
  )

-- | Closes the unbonded pool and distributes final rewards to users
-- | If the `batchSize` is zero, then funds will be deposited in one TX.
-- | Otherwise the transactions will be made in batches.
-- | If the `incompleteClose` is empty, then reward deposits will be made to all
-- | users. Otherwise only the users within in the list will have rewards
-- | deposited.
closeUnbondedPoolContract
  :: UnbondedPoolParams
  -> ScriptVersion
  -> Natural
  -> Maybe IncompleteClose
  -> Contract () (Maybe IncompleteClose)
closeUnbondedPoolContract
  params@
    ( UnbondedPoolParams
        { admin
        , nftCs
        , assocListCs
        }
    )
  scriptVersion
  batchSize
  incompleteClose' = do
  -- Fetch information related to the pool
  -- Get network ID and check admin's PKH
  networkId <- getNetworkId
  userPkh <- liftedM "closeUnbondedPoolContract: Cannot get user's pkh"
    ownPaymentPubKeyHash
  unless (userPkh == admin) $ throwContractError
    "closeUnbondedPoolContract: Admin is not current user"
  logInfo_ "closeUnbondedPoolContract: Admin PaymentPubKeyHash" userPkh
  -- Get the unbonded pool validator and hash
  validator <- liftedE' "closeUnbondedPoolContract: Cannot create validator"
    $ mkUnbondedPoolValidator params scriptVersion
  let valHash = validatorHash validator
  logInfo_ "closeUnbondedPoolContract: validatorHash" valHash
  let poolAddr = scriptHashAddress valHash Nothing
  logInfo_ "closeUnbondedPoolContract: Pool address"
    $ fromPlutusAddress networkId poolAddr
  -- Get the unbonded pool's utxo (if not consumed alredy)
  unbondedPoolUtxos <- utxosAt poolAddr
  logInfo_ "closeUnbondedPoolContract: Pool UTXOs" unbondedPoolUtxos
  tokenName <- liftContractM
    "closeUnbondedPoolContract: Cannot create TokenName"
    unbondedStakingTokenName
  poolStateUtxo <- runMaybeT do
    poolTxInput /\ poolTxOutput <- MaybeT $ pure $ getUtxoWithNFT
      unbondedPoolUtxos
      nftCs
      tokenName
    lift $ logInfo_ "closeUnbondedPoolContract: Pool's UTXO" poolTxInput
    poolDatumHash <- MaybeT $ pure $ getUtxoDatumHash poolTxOutput
    lift $ logInfo_ "closeUnbondedPoolContract: Pool's UTXO DatumHash"
      poolDatumHash
    poolDatum <- MaybeT $ getDatumByHash poolDatumHash
    pure $ poolTxInput /\ poolTxOutput /\ poolDatum
  logInfo' $ "closeUnbondedPoolContract: Pool state UTxO" <> show poolStateUtxo
  -- Get the bonding range to use
  logInfo' "closeUnbondedPoolContract: Getting admin range..."
  { currTime, range } <- getAdminTime params scriptVersion
  logInfo_ "closeUnbondedPoolContract: Current time: " $ show currTime
  logInfo_ "closeUnbondedPoolContract: TX Range" range
  ------ Update the association list
  logInfo'
    "closeUnbondedPoolContract: STAKE TYPE - StateDatum is \
    \StateDatum { maybeEntryName: Just ..., open: true }"
  let
    assocList = mkOnchainAssocList assocListCs unbondedPoolUtxos
  -- Obtain the entries' inputs and datums and generate the updated versions
  -- of the datums.
  -- If an `IncompleteClose` value is provided, only the given
  -- subset of the entries is updated.
  (entriesInputs /\ entriesDatums /\ updatedEntriesDatums) <-
    case incompleteClose' of
      Nothing -> do
        -- Use all entries
        let
          entriesInputs :: Array TransactionInput
          entriesInputs = map (fst <<< snd) assocList
        entriesDatums :: Array Entry <- getListDatums assocList
        -- Assign rewards and close
        let
          updatedEntriesDatums :: Array Entry
          updatedEntriesDatums = updateAndCloseEntriesList entriesDatums
        pure $ entriesInputs /\ entriesDatums /\ updatedEntriesDatums
      Just (IncompleteClose incompClose) -> do
        -- Use only the entries with the given keys
        let
          assocList'
            :: Array
                 ( ByteArray /\ TransactionInput /\
                     TransactionOutputWithRefScript
                 )
          assocList' = Array.filter
            (\(ba /\ _) -> Array.elem ba incompClose.failedKeys)
            assocList
          entriesInputs = map (fst <<< snd) assocList'
        -- Print warning if some of the failed keys were not found in the pool
        let
          keysNotFound :: Array ByteArray
          keysNotFound =
            Array.difference
              (fst <$> assocList)
              incompClose.failedKeys
        when (Array.length keysNotFound /= 0)
          $ logWarn'
          $ show (Array.length keysNotFound)
          <> " of the outdated entries were not found in the pool: "
          <> show keysNotFound
        entriesDatums <- getListDatums assocList'
        -- Assign rewards and close the outdated entries
        let
          updatedEntriesDatums =
            updateAndCloseOutdatedEntriesList
              incompClose.totalDeposited
              entriesDatums
        pure $ entriesInputs /\ entriesDatums /\ updatedEntriesDatums
  -- Make constraints and lookups and close pool state and entries.
  let
    redeemer = Redeemer $ toData AdminAct

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

  -- Submit transaction that closes state utxo separately (if necessary)
  stateUtxoConsumed <- case poolStateUtxo of
    Just (poolTxInput /\ _ /\ _) -> do
      adminUtxos <- liftedM "closeUnbondedPool: could not get wallet's utxos" $
        getWalletUtxos
      Array.null <$> submitTransaction
        constraints
        lookups
        confirmationTimeout
        submissionAttempts
        [ mustSpendScriptOutput poolTxInput redeemer /\
            ScriptLookups.unspentOutputs adminUtxos
        ]
    Nothing -> pure false
  if stateUtxoConsumed then logInfo'
    "closeUnbondedPoolContract: Succesfully closed pool's state utxo"
  else logInfo'
    "closeUnbondedPool: Could not close pool state utxo"
  -- Submit transaction that closes entries with possible batching
  entryUpdates
    :: Array ((TxConstraints Unit Unit) /\ (ScriptLookups PlutusData)) <-
    traverse (updateEntryTx params valHash)
      ( Array.zip entriesInputs
          (Array.zip entriesDatums updatedEntriesDatums)
      )
  adminUtxos <- liftedM "closeUnbondedPool: could not get wallet's utxos" $
    getWalletUtxos
  failedCloses <-
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
    "closeUnbondedPoolContract: Finished closing /\
    \pool entries. Entries with failed closes"
    failedCloses
  -- We obtain the failed entries and return them as part of a
  -- `IncompleteClose` (if there are any)
  let
    maybeFailedEntries = do
      indices <- NonEmpty.fromArray =<< traverse
        (flip Array.elemIndex entryUpdates)
        failedCloses
      let
        entries = foldl
          ( \acc i -> unsafePartial $ Array.unsafeIndex updatedEntriesDatums i
              : acc
          )
          mempty
          indices
      firstEntry <- Array.head entries
      Just $ IncompleteClose
        { failedKeys: map (unwrap >>> _.key) entries
        , totalDeposited: unwrap >>> _.totalDeposited $ firstEntry
        , stateUtxoConsumed
        }
  -- We return an `IncompleteClose` value if the pool failed to be consumed
  let
    maybeFailedState =
      if stateUtxoConsumed then Nothing
      else Just $ IncompleteClose
        { failedKeys: []
        , totalDeposited: zero
        , stateUtxoConsumed
        }
  pure $ maybeFailedEntries <|> maybeFailedState

updateAndCloseEntriesList :: Array Entry -> Array Entry
updateAndCloseEntriesList = map closeEntry <<< updateEntriesList zero

updateAndCloseOutdatedEntriesList :: BigInt -> Array Entry -> Array Entry
updateAndCloseOutdatedEntriesList totalDeposited =
  map closeEntry <<< updateOutdatedEntriesList zero totalDeposited

closeEntry :: Entry -> Entry
closeEntry (Entry e) = Entry $ e { open = false }
