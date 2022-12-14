module Test.Common(
       testInitialParams,
       testInitialParamsNoTimeChecks,
       withWalletsAndPool,
       getAdminWallet,
       getUserWallet) where

import Prelude

import Contract.Monad (Contract)
import Contract.Numeric.Rational ((%))
import Contract.Test.Plutip (InitialUTxOs, PlutipTest, withKeyWallet, withWallets)
import Contract.Value (adaSymbol, adaToken)
import Contract.Wallet (KeyWallet)
import Control.Monad.Error.Class (liftMaybe)
import Data.Array as Array
import Data.BigInt as BigInt
import Data.Newtype (unwrap)
import Effect.Exception as Exception
import Types (AssetClass(..), ScriptVersion(..))
import UnbondedStaking.CreatePool (createUnbondedPoolContract)
import UnbondedStaking.Types (InitialUnbondedParams(..), UnbondedPoolParams)
import Utils (currentTime, nat)


type TestInitialParams = {
   initialUnbondedParams :: InitialUnbondedParams
   , scriptVersion :: ScriptVersion
}

-- | Initial parameters for pool that starts at `currentTime` that uses ADA
-- as staking token.
testInitialParams :: Contract () TestInitialParams
testInitialParams = do
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
          , unbondedAssetClass: AssetClass { currencySymbol: adaSymbol, tokenName: adaToken }
        }
        , scriptVersion: Production
      }

testInitialParamsNoTimeChecks :: Contract () TestInitialParams
testInitialParamsNoTimeChecks = do
    initParams <- testInitialParams
    pure $ initParams { scriptVersion = DebugNoTimeChecks }

adminInitialUtxos :: InitialUTxOs
adminInitialUtxos = BigInt.fromInt <$> [ 10_000_000, 200_000_000 ]

-- | Helper for running contracts that interact with a pool created from the
-- given `InitialUnbondedParams`. The admin wallet is also preprended to the
-- passed wallets.
withWalletsAndPool ::
   Contract () TestInitialParams ->
   Array InitialUTxOs ->
   (Array KeyWallet -> UnbondedPoolParams -> Contract () Unit) ->
   PlutipTest
withWalletsAndPool initParamsContract distr contract =
    withWallets (Array.cons adminInitialUtxos distr) \wallets -> do
        adminW <- liftMaybe (Exception.error "(withWalletsAndPool) Could not get admin wallet") $
           Array.head wallets
        { initialUnbondedParams, scriptVersion } <- initParamsContract
        { unbondedPoolParams } <- withKeyWallet adminW $
           createUnbondedPoolContract initialUnbondedParams scriptVersion
        contract wallets unbondedPoolParams

-- | Obtains the admin wallet.
getAdminWallet :: Array KeyWallet -> Contract () KeyWallet
getAdminWallet = liftMaybe (Exception.error "Could not get admin wallet") <<< Array.head

-- | Obtain the wallet given by index
getUserWallet :: Int -> Array KeyWallet -> Contract () KeyWallet
getUserWallet idx ws = liftMaybe (Exception.error $ "Could not get user wallet " <> show idx)
   <<< Array.index ws $ idx + 1
