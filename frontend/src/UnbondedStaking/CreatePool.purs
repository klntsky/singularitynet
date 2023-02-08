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
import Contract.Transaction
  ( balanceTx
  , signTransaction
  )
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
  , repeatUntilConfirmed
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
  repeatUntilConfirmed confirmationTimeout submissionAttempts $ do
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

    unattachedBalancedTx <-
      liftedE $ ScriptLookups.mkUnbalancedTx lookup constraints

    -- `balanceAndSignTx` does the following:
    -- 1) Balance a transaction
    -- 2) Reindex `Spend` redeemers after finalising transaction inputs.
    -- 3) Attach datums and redeemers to transaction.
    -- 3) Sign tx, returning the Cbor-hex encoded `ByteArray`.
    bTx <- liftedE $ balanceTx unattachedBalancedTx
    signedTx <- signTransaction bTx
    -- Return the pool info for subsequent transactions
    pure { signedTx, unbondedPoolParams, address }

-- Get all the pools at the given address and with the right state token.
-- Although more than one could be returned, in all likelihood the user
-- intended (and managed) to create only one. This is because most pools
-- will have unique start times.
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

--   addListTokenCs
--     :: CurrencySymbol -> Contract () (CurrencySymbol /\ CurrencySymbol)
--   addListTokenCs stateNftCs = do
--     listPolicy <- liftedE (mkListNFTPolicy Unbonded scriptVersion stateNftCs)
--     listNftCs <-
--       liftMaybe
--         (Exception.error "Could not obtain currency symbol from list policy")
--         $
--           scriptCurrencySymbol listPolicy
--     pure $ stateNftCs /\ listNftCs
-- symbols <- traverse addListTokenCs
--   $ Array.mapMaybe (getStateTokenCs <<< getValue)
--   $ Array.fromFoldable poolUtxos
-- when (Array.length symbols > 1) $
--   logWarn'
--     "(getUnbondedPoolsContract) More than one pool with the given address"
-- -- For each symbol, we create the bonded params and we returh all of them
-- adminPkh <- liftedM "(getUnbondedPoolsContract) Cannot get admin's pkh"
--   ownPaymentPubKeyHash
-- pure $ map
--   (\(stateCs /\ listCs) -> mkUnbondedPoolParams adminPkh stateCs listCs ibp)
--   symbols
