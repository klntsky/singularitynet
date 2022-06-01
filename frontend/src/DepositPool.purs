module DepositPool (depositBondedPoolContract) where

import Contract.Prelude

import Contract.Address
  ( getNetworkId
  , getWalletAddress
  , ownPaymentPubKeyHash
  , validatorHashEnterpriseAddress
  )
import Contract.Monad
  ( Contract
  , liftContractM
  , liftedE
  , liftedE'
  , liftedM
  , throwContractError
  )
import Contract.PlutusData (PlutusData, Datum(Datum), toData, datumHash)
import Contract.Prim.ByteArray (byteArrayToHex)
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (validatorHash)
import Contract.Transaction
  ( BalancedSignedTransaction(BalancedSignedTransaction)
  , balanceAndSignTx
  , balanceTx
  , submit
  )
import Contract.TxConstraints
  ( TxConstraints
  , mustBeSignedBy
  , mustPayToScript
  , mustSpendScriptOutput
  )
import Contract.Utxos (utxosAt)
import Contract.Value (singleton)
import Control.Applicative (unless)
import Scripts.PoolValidator (mkBondedPoolValidator)
import Settings (bondedStakingTokenName)
import Types
  ( BondedStakingAction(AdminAct)
  , BondedStakingDatum(AssetDatum, StateDatum)
  , BondedPoolParams(BondedPoolParams)
  )
import Types.Redeemer (Redeemer(Redeemer))
import Utils (big, getUtxoWithNFT, logInfo_, nat)

-- Deposits a certain amount in the pool
depositBondedPoolContract :: BondedPoolParams -> Contract () Unit
depositBondedPoolContract params@(BondedPoolParams { admin, nftCs }) = do
  -- Fetch information related to the pool
  -- Get network ID and check admin's PKH
  networkId <- getNetworkId
  userPkh <- liftedM "depositBondedPoolContract: Cannot get user's pkh"
    ownPaymentPubKeyHash
  unless (userPkh == admin) $ throwContractError
    "depositBondedPoolContract: Admin \
    \is not current user"
  logInfo_ "depositBondedPoolContract: Admin PaymentPubKeyHash" userPkh
  -- Get the (Nami) wallet address
  adminAddr <- liftedM "depositBondedPoolContract: Cannot get wallet Address"
    getWalletAddress
  -- Get utxos at the wallet address
  adminUtxos <-
    liftedM "depositBondedPoolContract: Cannot get user Utxos" $
      utxosAt adminAddr
  -- Get the bonded pool validator and hash
  validator <- liftedE' "depositBondedPoolContract: Cannot create validator" $
    mkBondedPoolValidator params
  valHash <- liftedM "depositBondedPoolContract: Cannot hash validator"
    $ validatorHash validator
  logInfo_ "depositBondedPoolContract: validatorHash" valHash
  let poolAddr = validatorHashEnterpriseAddress networkId valHash
  logInfo_ "depositBondedPoolContract: Pool address" poolAddr
  -- Get the bonded pool's utxo
  bondedPoolUtxos <-
    liftedM
      "depositBondedPoolContract: Cannot get pool's utxos at pool address" $
      utxosAt poolAddr
  logInfo_ "depositBondedPoolContract: Pool UTXOs" bondedPoolUtxos
  tokenName <- liftContractM
    "depositBondedPoolContract: Cannot create TokenName"
    bondedStakingTokenName
  poolTxInput /\ poolTxOutput <-
    liftContractM "depositBondedPoolContract: Cannot get state utxo" $
      getUtxoWithNFT bondedPoolUtxos nftCs tokenName
  logInfo_ "depositBondedPoolContract: Pool's UTXO" poolTxInput
  poolDatumHash <-
    liftContractM
      "depositBondedPoolContract: Could not get Pool UTXO's Datum Hash"
      (unwrap poolTxOutput).dataHash
  logInfo_ "depositBondedPoolContract: Pool's UTXO DatumHash" poolDatumHash
  -- Create the datums and their ScriptLookups
  let
    -- We can hardcode the state for now. We should actually fetch the datum
    -- from Ogmios, update it properly and then submit it
    bondedStateDatum = Datum $ toData $ StateDatum
      { maybeEntryName: Nothing
      , sizeLeft: nat 100_000_000
      }
    -- This is the datum of the UTXO that will hold the rewards
    assetDatum = Datum $ toData AssetDatum

  bondedStateDatumLookup <-
    liftContractM
      "depositBondedPoolContract: Could not create state datum lookup"
      =<< ScriptLookups.datum bondedStateDatum
  let
    assetParams = unwrap (unwrap params).bondedAssetClass
    assetCs = assetParams.currencySymbol
    assetTn = assetParams.tokenName
    stateTokenValue = singleton nftCs tokenName one
    depositValue = singleton assetCs assetTn (big 2)
    scriptAddr = validatorHashEnterpriseAddress networkId valHash
  logInfo_
    "depositBondedPoolContract: BondedPool Validator's address"
    scriptAddr
  let
    -- We build the redeemer. The size does not change because there are no
    -- user stakes. It doesn't make much sense to deposit if there wasn't a
    -- change in the total amount of stakes (and accrued rewards). This will
    -- change when user staking is added
    redeemerData = toData $ AdminAct { sizeLeft: nat 100_000_000 }
    redeemer = Redeemer redeemerData

    lookup :: ScriptLookups.ScriptLookups PlutusData
    lookup = mconcat
      [ ScriptLookups.validator validator
      , ScriptLookups.unspentOutputs $ unwrap adminUtxos
      , ScriptLookups.unspentOutputs $ unwrap bondedPoolUtxos
      , bondedStateDatumLookup
      ]

    -- Seems suspect, not sure if typed constraints are working as expected
    constraints :: TxConstraints Unit Unit
    constraints =
      mconcat
        [
          -- Update the pool's state
          mustPayToScript valHash bondedStateDatum stateTokenValue
        -- Deposit rewards in a separate UTXO
        , mustPayToScript valHash assetDatum depositValue
        , mustBeSignedBy admin
        , mustSpendScriptOutput poolTxInput redeemer
        ]
  dh <- liftedM "depositBondedPoolContract: Cannot Hash AssetDatum" $ datumHash
    assetDatum
  dh' <- liftedM "depositBondedPoolContract: Cannot Hash BondedStateDatum" $
    datumHash
      bondedStateDatum
  logInfo_ "depositBondedPoolContract: DatumHash of AssetDatum" dh
  logInfo_ "depositBondedPoolContract: DatumHash of BondedStateDatum" dh'
  unattachedBalancedTx <-
    liftedE $ ScriptLookups.mkUnbalancedTx lookup constraints
  logInfo_
    "depositBondedPoolContract: unAttachedUnbalancedTx"
    unattachedBalancedTx
  let unbalancedTx = (unwrap unattachedBalancedTx).unbalancedTx
  balancedTx <- liftedE $ balanceTx unbalancedTx
  logInfo_ "depositBondedPoolContract: balancedTx" balancedTx
  BalancedSignedTransaction { signedTxCbor } <-
    liftedM
      "depositBondedPoolContract: Cannot balance, reindex redeemers, attach/\
      \datums redeemers and sign"
      $ balanceAndSignTx unattachedBalancedTx
  -- Submit transaction using Cbor-hex encoded `ByteArray`
  transactionHash <- submit signedTxCbor
  logInfo_
    "depositBondedPoolContract: Transaction successfully submitted with hash"
    $ byteArrayToHex
    $ unwrap transactionHash
