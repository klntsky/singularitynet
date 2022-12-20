module Test.Unit.Admin.DepositNUser (test) where

import Prelude

import Contract.Monad (Contract, throwContractError)
import Contract.Numeric.Natural as Natural
import Contract.Test.Plutip (PlutipTest, InitialUTxOs)
import Control.Monad.Reader (ask, lift)
import Data.Array ((..))
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Traversable (for_)
import Data.Tuple.Nested (type (/\), (/\))
import Test.Common (getAdminWallet, getUserWallet, getWalletFakegix, waitFor, waitForNext, withKeyWallet, withWalletsAndPool)
import UnbondedStaking.DepositPool (depositUnbondedPoolContract)
import UnbondedStaking.Types (Period(..), SnetInitialParams)
import UnbondedStaking.UserStake (userStakeUnbondedPoolContract)
import Utils (nat)

-- | The admin deposits to a pool with `n` user entries. We take into account
-- any rounding error, since this is a multi-user scenario.
test :: Contract () SnetInitialParams -> Int -> Int -> PlutipTest
test initParams userCount batchSize = withWalletsAndPool initParams usersInitialUtxos \wallets -> do
    adminWallet <- getAdminWallet wallets
    { unbondedPoolParams, scriptVersion } <- ask
    let stakeAmtBase = BigInt.fromInt 1000
        depositAmt = BigInt.fromInt 10_000
        expectedAdminDeposit = depositAmt
        maxRoundingError = BigInt.fromInt userCount
    waitFor UserPeriod
    for_ (0 .. (userCount - 1)) \idx -> getUserWallet idx wallets >>= flip withKeyWallet do
          let stakeAmt = BigInt.fromInt (idx + 1) * stakeAmtBase
          initialFakegix <- getWalletFakegix
          _txId <- lift $ userStakeUnbondedPoolContract unbondedPoolParams scriptVersion $ Natural.fromBigInt' stakeAmt
          finalFakegix <- getWalletFakegix
          when (not $ finalFakegix == initialFakegix - stakeAmt) $
             lift $ throwContractError "Incorrect amount of FAKEGIX deducted from user wallet"
    withKeyWallet adminWallet do
          waitFor AdminPeriod
          initialFakegix <- getWalletFakegix
          failedIndices <- lift $ depositUnbondedPoolContract depositAmt unbondedPoolParams scriptVersion (nat batchSize) []
          when (not $ Array.null failedIndices) $
             lift $ throwContractError $ "Some entries failed to be updated " <> show failedIndices
          finalFakegix <- getWalletFakegix
          when (not $ initialFakegix == finalFakegix) $
             lift $ throwContractError "The admin deposited FAKEGIX when they should not have"
    withKeyWallet adminWallet do
          waitForNext AdminPeriod
          initialFakegix <- getWalletFakegix
          failedIndices <- lift $ depositUnbondedPoolContract zero unbondedPoolParams scriptVersion (nat 0) []
          when (not $ Array.null failedIndices) $
             lift $ throwContractError "Some entries failed to be updated"
          finalFakegix <- getWalletFakegix
          let realAdminDeposit = initialFakegix - finalFakegix
          when (not $ realAdminDeposit >= expectedAdminDeposit && realAdminDeposit <= expectedAdminDeposit + maxRoundingError) $
             lift $ throwContractError $
                "The admin did not deposit the expected amount\n"
                <> "Expected: " <> show expectedAdminDeposit <> "(+ " <> show maxRoundingError <> ")\n"
                <> "Actual deposit: " <> show realAdminDeposit
    where usersInitialUtxos :: Array (InitialUTxOs /\ BigInt)
          usersInitialUtxos = Array.replicate userCount $
              map BigInt.fromInt [10_000_000, 100_000_000] /\ (BigInt.fromInt 1_000_000_000)

