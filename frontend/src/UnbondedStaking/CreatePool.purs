module UnbondedStaking.CreatePool (createUnbondedPoolContract) where

import Contract.Prelude

import Contract.Address
  ( Bech32String
  , addressToBech32
  , getWalletAddress
  , ownPaymentPubKeyHash
  , scriptHashAddress
  )
import Contract.Monad
  ( Contract
  , liftContractM
  , liftContractM
  , liftedE
  , liftedE'
  , liftedM
  )
import Contract.PlutusData (Datum(Datum), PlutusData, toData)
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (validatorHash)
import Contract.Transaction
  ( BalancedSignedTransaction
  , TransactionHash
  , balanceAndSignTx
  )
import Contract.TxConstraints
  ( TxConstraints
  , mustMintValue
  , mustSpendPubKeyOutput
  )
import Contract.Utxos (utxosAt)
import Contract.Value (scriptCurrencySymbol, singleton)
import Data.Array (head)
import Data.Map (toUnfoldable)
import Plutus.Conversion (fromPlutusAddress)
import Scripts.ListNFT (mkListNFTPolicy)
import Scripts.PoolValidator (mkUnbondedPoolValidator)
import Scripts.StateNFT (mkStateNFTPolicy)
import Settings
  ( unbondedStakingTokenName
  , confirmationTimeout
  , submissionAttempts
  )
import Types (StakingType(Unbonded))
import Types.Interval (POSIXTime(POSIXTime))
import UnbondedStaking.Types
  ( InitialUnbondedParams(InitialUnbondedParams)
  , UnbondedPoolParams
  , UnbondedStakingDatum(StateDatum)
  )
import UnbondedStaking.Utils (mkUnbondedPoolParams)
import Utils
  ( currentRoundedTime
  , logInfo_
  , repeatUntilConfirmed
  , mustPayToScript
  )

-- Sets up pool configuration, mints the state NFT and deposits
-- in the pool validator's address
createUnbondedPoolContract
  :: InitialUnbondedParams
  -> Contract ()
       { txId :: String
       , unbondedPoolParams :: UnbondedPoolParams
       , address :: Bech32String
       }
createUnbondedPoolContract iup =
  repeatUntilConfirmed confirmationTimeout submissionAttempts $ do
    adminPkh <- liftedM "createUnbondedPoolContract: Cannot get admin's pkh"
      ownPaymentPubKeyHash
    logInfo_ "createUnbondedPoolContract: Admin PaymentPubKeyHash" adminPkh
    -- Get the (Nami) wallet address
    adminAddr <-
      liftedM "createUnbondedPoolContract: Cannot get wallet Address"
        getWalletAddress
    logInfo_ "createUnbondedPoolContract: User Address"
      =<< addressToBech32 adminAddr
    -- Get utxos at the wallet address
    adminUtxos <-
      liftedM "createUnbondedPoolContract: Cannot get user Utxos"
        $ utxosAt adminAddr
    txOutRef <-
      liftContractM "createUnbondedPoolContract: Could not get head UTXO"
        $ fst
        <$> (head $ toUnfoldable adminUtxos)
    logInfo_ "createUnbondedPoolContract: Admin Utxos" adminUtxos
    -- Get the minting policy and currency symbol from the state NFT:
    statePolicy <- liftedE $ mkStateNFTPolicy Unbonded txOutRef
    stateNftCs <-
      liftContractM
        "createUnbondedPoolContract: Cannot get CurrencySymbol from /\
        \state NFT"
        $ scriptCurrencySymbol statePolicy
    -- Get the minting policy and currency symbol from the list NFT:
    listPolicy <- liftedE $ mkListNFTPolicy Unbonded stateNftCs
    assocListCs <-
      liftContractM
        "createUnbondedPoolContract: Cannot get CurrencySymbol from /\
        \state NFT"
        $ scriptCurrencySymbol listPolicy
    -- May want to hardcode this somewhere:
    tokenName <-
      liftContractM "createUnbondedPoolContract: Cannot create TokenName"
        unbondedStakingTokenName
    -- We get the current time and set up the pool to start immediately
    POSIXTime currTime <- currentRoundedTime
    let
      iup' = unwrap iup

      iupWithTime :: InitialUnbondedParams
      iupWithTime = InitialUnbondedParams $ iup'
        { start = currTime
        }
    -- We define the parameters of the pool
    let
      unbondedPoolParams = mkUnbondedPoolParams adminPkh stateNftCs assocListCs
        iupWithTime
    -- Get the bonding validator and hash
    validator <- liftedE' "createUnbondedPoolContract: Cannot create validator"
      $ mkUnbondedPoolValidator unbondedPoolParams
    let
      valHash = validatorHash validator
      mintValue = singleton stateNftCs tokenName one
    address <- addressToBech32 $ scriptHashAddress valHash
    logInfo_ "createUnbondedPoolContract: UnbondedPool Validator's address"
      address
    let
      unbondedStateDatum = Datum $ toData $ StateDatum
        { maybeEntryName: Nothing
        , open: true
        }

      lookup :: ScriptLookups.ScriptLookups PlutusData
      lookup = mconcat
        [ ScriptLookups.mintingPolicy statePolicy
        , ScriptLookups.validator validator
        , ScriptLookups.unspentOutputs adminUtxos
        ]

      -- Seems suspect, not sure if typed constraints are working as expected
      constraints :: TxConstraints Unit Unit
      constraints =
        mconcat
          [ mustPayToScript valHash unbondedStateDatum mintValue
          , mustMintValue mintValue
          , mustSpendPubKeyOutput txOutRef
          ]

    unattachedBalancedTx <-
      liftedE $ ScriptLookups.mkUnbalancedTx lookup constraints

    -- `balanceAndSignTx` does the following:
    -- 1) Balance a transaction
    -- 2) Reindex `Spend` redeemers after finalising transaction inputs.
    -- 3) Attach datums and redeemers to transaction.
    -- 3) Sign tx, returning the Cbor-hex encoded `ByteArray`.
    signedTx <-
      liftedM
        "createUnbondedPoolContract: Cannot balance, reindex redeemers, attach /\
        \datums redeemers and sign"
        $ balanceAndSignTx unattachedBalancedTx

    -- Return the pool info for subsequent transactions
    pure { signedTx, unbondedPoolParams, address }
