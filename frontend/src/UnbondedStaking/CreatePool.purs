module UnbondedStaking.CreatePool
  ( createUnbondedPoolContract
  , getUnbondedPoolContract
  ) where

import Contract.Prelude

import Contract.Address
  ( Bech32String
  , PaymentPubKeyHash
  , addressToBech32
  , getWalletAddress
  , ownPaymentPubKeyHash
  , scriptHashAddress
  )
import Contract.Monad (Contract, liftContractM, liftedE, liftedE', liftedM)
import Contract.PlutusData (Datum(Datum), PlutusData, toData)
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (validatorHash)
import Contract.TxConstraints
  ( TxConstraints
  , mustMintValue
  , mustSpendPubKeyOutput
  )
import Contract.Utxos (getWalletUtxos)
import Contract.Value
  ( CurrencySymbol
  , scriptCurrencySymbol
  , singleton
  )
import Control.Monad.Error.Class (liftMaybe)
import Data.Array as Array
import Data.Map (toUnfoldable)
import Effect.Exception as Exception
import Scripts.ListNFT (mkListNFTPolicy)
import Scripts.PoolValidator (mkUnbondedPoolValidator)
import Scripts.StateNFT (mkStateNFTPolicy)
import Settings
  ( confirmationTimeout
  , submissionAttempts
  , unbondedStakingTokenName
  )
import Types (ScriptVersion, StakingType(Unbonded))
import UnbondedStaking.Types
  ( InitialUnbondedParams
  , UnbondedPoolParams
  , UnbondedStakingDatum(StateDatum)
  )
import UnbondedStaking.Utils (mkUnbondedPoolParams)
import Utils
  ( logInfo_
  , mustPayToScript
  , repeatUntilConfirmed'
  )

-- Sets up pool configuration, mints the state NFT and deposits
-- in the pool validator's address
createUnbondedPoolContract
  :: InitialUnbondedParams
  -> ScriptVersion
  -> Contract ()
       { txId :: String
       , unbondedPoolParams :: UnbondedPoolParams
       , address :: Bech32String
       }
createUnbondedPoolContract iup scriptVersion =
  repeatUntilConfirmed' confirmationTimeout submissionAttempts $ do
    adminPkh <- liftedM "createUnbondedPoolContract: Cannot get admin's pkh"
      ownPaymentPubKeyHash
    -- Get the (Nami) wallet address
    adminAddr <-
      liftedM "createUnbondedPoolContract: Cannot get wallet Address"
        getWalletAddress
    logInfo_ "createUnbondedPoolContract: User Address"
      =<< addressToBech32 adminAddr
    -- Get utxos at the wallet address
    adminUtxos <-
      liftedM "createUnbondedPoolContract: Could not get wallet utxos"
        $ getWalletUtxos
    logInfo_ "Admin utxos:" $ show adminUtxos
    txOutRef <-
      liftContractM "createUnbondedPoolContract: Could not get head UTXO"
        $ fst
        <$> (Array.head $ toUnfoldable adminUtxos)
    logInfo_ "createUnbondedPoolContract: Admin Utxos" adminUtxos
    -- Get the minting policy and currency symbol from the state NFT:
    statePolicy <- liftedE $ mkStateNFTPolicy Unbonded scriptVersion txOutRef
    stateNftCs <-
      liftContractM
        "createUnbondedPoolContract: Cannot get CurrencySymbol from /\
        \state NFT"
        $ scriptCurrencySymbol statePolicy
    -- Get the minting policy and currency symbol from the list NFT:
    listPolicy <- liftedE $ mkListNFTPolicy Unbonded scriptVersion stateNftCs
    assocListCs <-
      liftContractM
        "createUnbondedPoolContract: Cannot get CurrencySymbol from /\
        \state NFT"
        $ scriptCurrencySymbol listPolicy
    -- May want to hardcode this somewhere:
    tokenName <-
      liftContractM "createUnbondedPoolContract: Cannot create TokenName"
        unbondedStakingTokenName
    -- We define the parameters of the pool
    let
      unbondedPoolParams = mkUnbondedPoolParams adminPkh stateNftCs assocListCs
        iup
    -- Get the bonding validator and hash
    validator <- liftedE' "createUnbondedPoolContract: Cannot create validator"
      $ mkUnbondedPoolValidator unbondedPoolParams scriptVersion
    let
      valHash = validatorHash validator
      mintValue = singleton stateNftCs tokenName one
    address <- addressToBech32 $ scriptHashAddress valHash Nothing
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

    ubTx <- liftedE $ ScriptLookups.mkUnbalancedTx lookup constraints

    pure { ubTx, unbondedPoolParams, address }

-- To know the pool we need to know the initialUnbondedParams, the admin pkh and the CS of the state token
getUnbondedPoolContract
  :: PaymentPubKeyHash
  -> CurrencySymbol
  -> InitialUnbondedParams
  -> ScriptVersion
  -> Contract () UnbondedPoolParams
getUnbondedPoolContract adminPkh stateCs ibp scriptVersion = do
  listPolicy <- liftedE (mkListNFTPolicy Unbonded scriptVersion stateCs)
  listCs <-
    liftMaybe
      (Exception.error "Could not obtain currency symbol from list policy")
      $
        scriptCurrencySymbol listPolicy

  pure $ mkUnbondedPoolParams adminPkh stateCs listCs ibp
