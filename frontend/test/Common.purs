module Test.Common(
       fakegixTokenName,
       testInitialParams,
       testInitialParamsNoTimeChecks,
       withWalletsAndPool,
       getAdminWallet,
       getUserWallet) where

import Prelude

import Contract.Address (PaymentPubKeyHash, ownPaymentPubKeyHash)
import Contract.Monad (Contract, liftedE, liftedM)
import Contract.Numeric.Rational ((%))
import Contract.Prelude (mconcat)
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.ScriptLookups (ScriptLookups, mintingPolicy, mkUnbalancedTx, unspentOutputs)
import Contract.Scripts (MintingPolicy)
import Contract.Test.Plutip (PlutipTest, InitialUTxOs, withKeyWallet, withWallets)
import Contract.Transaction (awaitTxConfirmed, balanceTx, signTransaction, submit)
import Contract.TxConstraints (TxConstraints, mustMintValue, mustPayToPubKey)
import Contract.Utxos (getWalletUtxos)
import Contract.Value (CurrencySymbol, TokenName, Value, mkTokenName, scriptCurrencySymbol, singleton)
import Contract.Wallet (KeyWallet)
import Control.Monad.Error.Class (liftMaybe)
import Data.Array as Array
import Data.Bifunctor (rmap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Foldable (foldMap, sum)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple (fst, snd, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Exception as Exception
import Tests.Scripts.AlwaysSucceedsMp (mkTrivialPolicy)
import Types (AssetClass(..), ScriptVersion(..))
import UnbondedStaking.CreatePool (createUnbondedPoolContract)
import UnbondedStaking.Types (InitialUnbondedParams(..), UnbondedPoolParams)
import Utils (currentTime, nat)


type TestInitialParams = {
   initialUnbondedParams :: InitialUnbondedParams
   , scriptVersion :: ScriptVersion
}

fakegixTokenName :: Contract () TokenName
fakegixTokenName = liftMaybe (Exception.error "Could not make FAKEGIX token name") $
    mkTokenName =<< byteArrayFromAscii "FAKEGIX"

getFakegixData :: Contract () (MintingPolicy /\ CurrencySymbol /\ TokenName)
getFakegixData = do
    mp <- mkTrivialPolicy
    cs <- liftMaybe (Exception.error "Could not get CurrencySymbol from minting policy") $
             scriptCurrencySymbol mp
    tn <- fakegixTokenName
    pure $ mp /\ cs /\ tn

-- | Initial parameters for pool that starts at `currentTime` that uses FAKEGIX
-- as staking token.
testInitialParams :: Contract () TestInitialParams
testInitialParams = do
    (_ /\ fakegixCs /\ fakegixTn) <- getFakegixData
    let periodLen = BigInt.fromInt 30_000
    interest <- liftMaybe (Exception.error "testInitialParams: could not make Rational") $ 1 % 100
    start <- unwrap <$> currentTime
    pure
      {
        initialUnbondedParams: InitialUnbondedParams {
          start
          , adminLength: periodLen
          , bondingLength: periodLen
          , userLength: periodLen
          , interestLength: periodLen
          , increments: nat 1
          , interest
          , minStake: nat 1_000
          , maxStake: nat 1_000_000
          , unbondedAssetClass: AssetClass { currencySymbol: fakegixCs, tokenName: fakegixTn }
        }
        , scriptVersion: Production
      }

testInitialParamsNoTimeChecks :: Contract () TestInitialParams
testInitialParamsNoTimeChecks = do
    initParams <- testInitialParams
    pure $ initParams { scriptVersion = DebugNoTimeChecks }

adminInitialUtxos :: InitialUTxOs /\ BigInt
adminInitialUtxos =  (BigInt.fromInt <$> [ 10_000_000, 200_000_000]) /\ BigInt.fromInt 1_000_000

-- | Helper for running contracts that interact with a pool created from the
-- given `InitialUnbondedParams`. The admin wallet is also preprended to the
-- passed wallets.
withWalletsAndPool ::
   Contract () TestInitialParams ->
   Array (InitialUTxOs /\ BigInt) ->
   (Array KeyWallet -> UnbondedPoolParams -> Contract () Unit) ->
   PlutipTest
withWalletsAndPool initParamsContract distr contract =
    withWallets (fst <$> Array.cons adminInitialUtxos distr) \wallets -> do
        adminW <- getAdminWallet wallets
        userPkhs <- dropMaybe <$> (traverse <<< traverse) getUserPkh (Array.tail wallets)
        -- Mint and distribute FAKEGIX
        withKeyWallet adminW $
           mintAndDistributeFakegix (snd adminInitialUtxos) $ Array.zip userPkhs $ snd <$> distr
        -- Create pool
        { initialUnbondedParams, scriptVersion } <- initParamsContract
        { unbondedPoolParams } <- withKeyWallet adminW $
           createUnbondedPoolContract initialUnbondedParams scriptVersion
        contract wallets unbondedPoolParams
    where getUserPkh :: KeyWallet -> Contract () PaymentPubKeyHash
          getUserPkh w =
              withKeyWallet w <<<
              liftedM "(getUserPkh) Could not user's PKH" $
              ownPaymentPubKeyHash
          dropMaybe :: forall a . Maybe (Array a) -> Array a
          dropMaybe (Just arr) = arr
          dropMaybe Nothing = []

-- | Obtains the admin wallet.
getAdminWallet :: Array KeyWallet -> Contract () KeyWallet
getAdminWallet = liftMaybe (Exception.error "Could not get admin wallet") <<< Array.head

-- | Obtain the wallet given by index
getUserWallet :: Int -> Array KeyWallet -> Contract () KeyWallet
getUserWallet idx ws = liftMaybe (Exception.error $ "Could not get user wallet " <> show idx)
   <<< Array.index ws $ idx + 1

-- | Mint the necessay FAKEGIX and distribute it to admin and users
mintAndDistributeFakegix :: BigInt -> Array (PaymentPubKeyHash /\ BigInt) -> Contract () Unit
mintAndDistributeFakegix adminFakegix users = do
    adminUtxos <- liftedM "Could not get admin's utxos" $ getWalletUtxos
    (mp /\ cs /\ tn) <- getFakegixData
    let valueToMint :: Value
        valueToMint = singleton cs tn $ adminFakegix + sum (snd <$> users)
        constraints :: TxConstraints Void Void
        constraints = mconcat
            [
               mustMintValue valueToMint,
               foldMap (uncurry mustPayToPubKey <<< rmap (singleton cs tn)) users
            ]
        lookups :: ScriptLookups Void
        lookups = mconcat
            [
               unspentOutputs adminUtxos,
               mintingPolicy mp
            ]
    ubTx <- liftedE $ mkUnbalancedTx lookups constraints
    tx <- signTransaction =<< liftedE (balanceTx ubTx)
    txId <- submit tx
    awaitTxConfirmed txId

