module Test.Common(testInitialParams, withWalletsAndPool) where

import Prelude

import Contract.Log (logInfo')
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
import Types (AssetClass(..))
import UnbondedStaking.CreatePool (createUnbondedPoolContract)
import UnbondedStaking.Types (InitialUnbondedParams(..), UnbondedPoolParams)
import Utils (currentTime, nat)

-- | Initial parameters for pool that starts at `currentTime` that uses ADA
-- as staking token.
testInitialParams :: Contract () InitialUnbondedParams
testInitialParams = do
    let periodLen = BigInt.fromInt 30_000
    interest <- liftMaybe (Exception.error "testInitialParams: could not make Rational") $ 1 % 100
    start <- unwrap <$> currentTime
    pure $ InitialUnbondedParams {
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

adminInitialUtxos :: InitialUTxOs
adminInitialUtxos = BigInt.fromInt <$> [ 10_000_000, 200_000_000 ]

-- | Helper for running contracts that interact with a pool created from the
-- given `InitialUnbondedParams`. The admin wallet is also preprended to the
-- passed wallets.
withWalletsAndPool ::
   Contract () InitialUnbondedParams ->
   Array InitialUTxOs ->
   (Array KeyWallet -> UnbondedPoolParams -> Contract () Unit) ->
   PlutipTest
withWalletsAndPool initParamsContract distr contract =
    withWallets (Array.cons adminInitialUtxos distr) \wallets -> do
        logInfo' "HELLO"
        adminW <- liftMaybe (Exception.error "(withWalletsAndPool) Could not get admin wallet") $
           Array.head wallets
        logInfo' "GOT ADMIN"
        { unbondedPoolParams } <- withKeyWallet adminW $ initParamsContract >>= createUnbondedPoolContract
        logInfo' "INITIALISED"
        contract wallets unbondedPoolParams

