module ClosePool (closeBondedPoolContract) where

import Contract.Prelude

import Contract.Address
  ( getNetworkId
  , ownPaymentPubKeyHash
  , scriptHashAddress
  )
import Contract.Monad
  ( Contract
  , liftContractM
  , liftedE
  , liftedE'
  , liftedM
  , throwContractError
  )
import Contract.PlutusData
  ( Datum(Datum)
  , PlutusData
  , Redeemer(Redeemer)
  , toData
  )
import Contract.Prim.ByteArray (byteArrayToHex)
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (validatorHash)
import Contract.Transaction
  ( BalancedSignedTransaction(BalancedSignedTransaction)
  , balanceAndSignTx
  , submit
  )
import Contract.TxConstraints
  ( TxConstraints
  , mustBeSignedBy
  , mustIncludeDatum
  , mustSpendScriptOutput
  )
import Contract.Utxos (utxosAt)
import Data.Map (toUnfoldable)
import Plutus.FromPlutusType (fromPlutusType)
import Scripts.PoolValidator (mkBondedPoolValidator)
import Types
  ( BondedPoolParams(BondedPoolParams)
  , BondedStakingAction(CloseAct)
  , BondedStakingDatum(StateDatum)
  )
import Utils (logInfo_)

closeBondedPoolContract :: BondedPoolParams -> Contract () Unit
closeBondedPoolContract params@(BondedPoolParams { admin }) = do
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
    $ mkBondedPoolValidator params
  valHash <- liftContractM "closeBondedPoolContract: Cannot hash validator"
    $ validatorHash validator
  logInfo_ "closeBondedPoolContract: validatorHash" valHash
  let poolAddr = scriptHashAddress valHash
  logInfo_ "closeBondedPoolContract: Pool address"
    $ fromPlutusType (networkId /\ poolAddr)
  -- Get the bonded pool's utxo
  bondedPoolUtxos <-
    liftedM "closeBondedPoolContract: Cannot get pool's utxos at pool address" $
      utxosAt poolAddr
  logInfo_ "closeBondedPoolContract: Pool's UTXOs" bondedPoolUtxos
  let
    bondedStateDatum = Datum $ toData $ StateDatum
      { maybeEntryName: Nothing
      }
  bondedStateDatumLookup <-
    liftContractM
      "closeBondedPoolContract: Could not create state datum lookup"
      $ ScriptLookups.datum bondedStateDatum
  -- We build the transaction
  let
    redeemer = Redeemer $ toData CloseAct

    lookup :: ScriptLookups.ScriptLookups PlutusData
    lookup = mconcat
      [ ScriptLookups.validator validator
      , ScriptLookups.unspentOutputs $ unwrap bondedPoolUtxos
      , bondedStateDatumLookup
      ]

    -- Seems suspect, not sure if typed constraints are working as expected
    constraints :: TxConstraints Unit Unit
    constraints =
      -- Spend all UTXOs to return to Admin:
      foldMap
        (flip mustSpendScriptOutput redeemer <<< fst)
        (toUnfoldable $ unwrap bondedPoolUtxos :: Array _)
        <> mustBeSignedBy admin
        <> mustIncludeDatum bondedStateDatum
  unattachedBalancedTx <-
    liftedE $ ScriptLookups.mkUnbalancedTx lookup constraints
  BalancedSignedTransaction { signedTxCbor } <-
    liftedM
      "closeBondedPoolContract: Cannot balance, reindex redeemers, attach/\
      \datums redeemers and sign"
      $ balanceAndSignTx unattachedBalancedTx
  -- Submit transaction using Cbor-hex encoded `ByteArray`
  transactionHash <- submit signedTxCbor
  logInfo_
    "closeBondedPoolContract: Transaction successfully submitted with hash"
    $ byteArrayToHex
    $ unwrap transactionHash