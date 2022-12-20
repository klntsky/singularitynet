module ClosePool (closeBondedPoolContract) where

import Contract.Prelude

import BondedStaking.TimeUtils (getClosingTime)
import Contract.Address (getNetworkId, ownPaymentPubKeyHash, scriptHashAddress)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedE', liftedM, throwContractError)
import Contract.Numeric.Natural (Natural)
import Contract.PlutusData (Datum(..), Redeemer(..), PlutusData, fromData, getDatumByHash, toData)
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (validatorHash)
import Contract.Transaction (TransactionInput, TransactionOutputWithRefScript)
import Contract.TxConstraints (TxConstraints, mustBeSignedBy, mustIncludeDatum, mustSpendScriptOutput, mustValidateIn)
import Contract.Utxos (getWalletUtxos, utxosAt)
import Ctl.Internal.Plutus.Conversion (fromPlutusAddress)
import Data.Array (elemIndex, (!!))
import Data.Map (toUnfoldable)
import Scripts.PoolValidator (mkBondedPoolValidator)
import Settings (bondedStakingTokenName, confirmationTimeout, submissionAttempts)
import Types (BondedPoolParams(BondedPoolParams), BondedStakingAction(CloseAct), BondedStakingDatum, ScriptVersion)
import Utils (getUtxoDatumHash, getUtxoWithNFT, logInfo_, splitByLength, submitBatchesSequentially, submitTransaction, toIntUnsafe)

closeBondedPoolContract
  :: BondedPoolParams
  -> ScriptVersion
  -> Natural
  -> Array Int
  -> Contract () (Array Int)
closeBondedPoolContract
  params@(BondedPoolParams { admin, nftCs })
  scriptVersion
  batchSize
  closeList = do
  -- Fetch information related to the pool
  -- Get network ID and check admin's PKH
  networkId <- getNetworkId
  userPkh <- liftedM "closeBondedPoolContract: Cannot get user's pkh"
    ownPaymentPubKeyHash
  unless (userPkh == admin) $ throwContractError
    "closeBondedPoolContract: Admin \
    \is not current user"
  logInfo_ "closeBondedPoolContract: Admin PaymentPubKeyHash" admin
  -- Get the bonded pool validator and hash
  validator <- liftedE' "closeBondedPoolContract: Cannot create validator"
    $ mkBondedPoolValidator params scriptVersion
  let valHash = validatorHash validator
  logInfo_ "closeBondedPoolContract: validatorHash" valHash
  let poolAddr = scriptHashAddress valHash Nothing
  logInfo_ "closeBondedPoolContract: Pool address"
    $ fromPlutusAddress networkId poolAddr
  -- Get the bonded pool's utxo
  bondedPoolUtxos <-
    liftedM "closeBondedPoolContract: could not obtain pool utxos" $
      utxosAt poolAddr
  logInfo_ "closeBondedPoolContract: Pool's UTXOs" bondedPoolUtxos
  tokenName <- liftContractM
    "closeBondedPoolContract: Cannot create TokenName"
    bondedStakingTokenName
  poolTxInput /\ poolTxOutput <-
    liftContractM "closeBondedPoolContract: Cannot get state utxo"
      $ getUtxoWithNFT bondedPoolUtxos nftCs tokenName
  logInfo_ "closeBondedPoolContract: Pool's UTXO" poolTxInput
  poolDatumHash <-
    liftContractM
      "closeBondedPoolContract: Could not get Pool UTXO's Datum Hash"
      $ getUtxoDatumHash poolTxOutput
  logInfo_ "closeBondedPoolContract: Pool's UTXO DatumHash" poolDatumHash
  poolDatum <- liftedM "closeBondedPoolContract: Cannot get datum"
    $ getDatumByHash poolDatumHash
  bondedStakingDatum :: BondedStakingDatum <-
    liftContractM
      "closeBondedPoolContract: Cannot extract NFT State datum"
      $ fromData (unwrap poolDatum)
  let bondedStateDatum = Datum $ toData bondedStakingDatum
  bondedStateDatumLookup <-
    liftContractM
      "closeBondedPoolContract: Could not create state datum lookup"
      $ ScriptLookups.datum bondedStateDatum

  -- Get the withdrawing range to use
  logInfo' "closeBondedPoolContract: Getting withdrawing range..."
  { currTime, range: txRange } <- getClosingTime params
  logInfo_ "closeBondedPoolContract: Current time: " $ show currTime
  logInfo_ "closeBondedPoolContract: TX Range" txRange

  -- If closeList is null, update all entries in assocList
  -- Otherwise, just update entries selected by indices in closeList
  spendList <-
    let
      allConstraints = createUtxoConstraint <$>
        (toUnfoldable bondedPoolUtxos)
    in
      if null closeList then pure allConstraints
      else
        liftContractM "depositBondedPoolContract: Failed to create updateList" $
          traverse ((!!) allConstraints) closeList

  -- We build the transaction
  let
    lookups :: ScriptLookups.ScriptLookups PlutusData
    lookups = mconcat
      [ ScriptLookups.validator validator
      , ScriptLookups.unspentOutputs bondedPoolUtxos
      , bondedStateDatumLookup
      -- We deliberately omit the admin utxos, since batching includes
      -- them automatically before every batch.
      ]

    constraints :: TxConstraints Unit Unit
    constraints =
      mustBeSignedBy admin
        <> mustIncludeDatum bondedStateDatum
        <> mustValidateIn txRange

  adminUtxos <- liftedM "closeBondedPoolContract: could not get admin's utxos" $
      getWalletUtxos

  -- Submit transaction with possible batching
  failedDeposits <-
    if batchSize == zero then
      submitTransaction
         constraints
         (lookups <> ScriptLookups.unspentOutputs adminUtxos)
         confirmationTimeout
         submissionAttempts
         spendList
    else do
      let updateBatches = splitByLength (toIntUnsafe batchSize) spendList
      submitBatchesSequentially
         constraints
         lookups
         confirmationTimeout
         submissionAttempts
         updateBatches

  logInfo_
    "closeBondedPoolContract: Finished updating pool entries. /\
    \Entries with failed updates"
    failedDeposits
  -- Return the indices of the failed deposits in the updateList
  liftContractM
    "depositUnbondedPoolContract: Failed to create /\
    \failedDepositsIndicies list" $
    traverse (flip elemIndex spendList) failedDeposits

createUtxoConstraint
  :: Tuple TransactionInput TransactionOutputWithRefScript
  -> Tuple (TxConstraints Unit Unit) (ScriptLookups.ScriptLookups PlutusData)
createUtxoConstraint (input /\ _) = do
  let valRedeemer = Redeemer $ toData CloseAct
  (mustSpendScriptOutput input valRedeemer) /\ mempty
