module Test.Unit.Admin.Deposit1User (test) where

import Prelude

import Contract.Monad (throwContractError)
import Contract.Numeric.Natural as Natural
import Contract.Test.Plutip (PlutipTest, InitialUTxOs, withKeyWallet)
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Tuple.Nested (type (/\), (/\))
import Test.Common (getAdminWallet, getUserWallet, getWalletFakegix, testInitialParamsNoTimeChecks, withWalletsAndPool)
import Types (ScriptVersion(..))
import UnbondedStaking.DepositPool (depositUnbondedPoolContract)
import UnbondedStaking.UserStake (userStakeUnbondedPoolContract)
import Utils (nat)

scriptVersion :: ScriptVersion
scriptVersion = DebugNoTimeChecks

bobInitialUtxos :: InitialUTxOs /\ BigInt
bobInitialUtxos = map BigInt.fromInt [10_000_000, 100_000_000] /\ (BigInt.fromInt 1_000_000_000)

-- | The admin deposits to a pool with one user entry
test :: PlutipTest
test = withWalletsAndPool testInitialParamsNoTimeChecks [bobInitialUtxos] \wallets ubp -> do
    adminWallet <- getAdminWallet wallets
    bobWallet <- getUserWallet 0 wallets
    withKeyWallet bobWallet do
          let stakeAmt = nat 2000
          initialFakegix <- getWalletFakegix
          _txId <- userStakeUnbondedPoolContract ubp scriptVersion stakeAmt
          finalFakegix <- getWalletFakegix
          when (not $ finalFakegix == initialFakegix - Natural.toBigInt stakeAmt) $
             throwContractError "Incorrect amount of FAKEGIX deducted from user wallet"
    withKeyWallet adminWallet do
          let depositAmt = BigInt.fromInt 10_000
          initialFakegix <- getWalletFakegix
          failedIndices <- depositUnbondedPoolContract depositAmt ubp scriptVersion (nat 0) []
          when (not $ Array.null failedIndices) $
             throwContractError "Some entries failed to be updated"
          finalFakegix <- getWalletFakegix
          when (not $ initialFakegix == finalFakegix) $
             throwContractError "The admin deposited FAKEGIX when they should not have"
