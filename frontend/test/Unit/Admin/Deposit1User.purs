module Test.Unit.Admin.Deposit1User (test) where

import Prelude

import Contract.Log (logDebug')
import Contract.Monad (throwContractError)
import Contract.Numeric.Natural as Natural
import Contract.Test.Plutip (PlutipTest, InitialUTxOs)
import Control.Monad.Reader(ask, lift)
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Tuple.Nested (type (/\), (/\))
import Test.Common (getAdminWallet, getUserWallet, getWalletFakegix, testInitialParams, withWalletsAndPool, withKeyWallet)
import UnbondedStaking.DepositPool (depositUnbondedPoolContract)
import UnbondedStaking.UserStake (userStakeUnbondedPoolContract)
import Utils (nat)

bobInitialUtxos :: InitialUTxOs /\ BigInt
bobInitialUtxos = map BigInt.fromInt [10_000_000, 100_000_000] /\ (BigInt.fromInt 1_000_000_000)

-- | The admin deposits to a pool with one user entry.
test :: PlutipTest
test = withWalletsAndPool testInitialParams [bobInitialUtxos] \wallets -> do
    adminWallet <- getAdminWallet wallets
    bobWallet <- getUserWallet 0 wallets
    { unbondedPoolParams, scriptVersion } <- ask
    let stakeAmt = nat 2000
        depositAmt = BigInt.fromInt 10_000
        --interestRate = _.interest $ unwrap unbondedPoolParams
        expectedAdminDeposit = depositAmt
    -- User stake
    withKeyWallet bobWallet do
          initialFakegix <- getWalletFakegix
          _txId <- lift $ userStakeUnbondedPoolContract unbondedPoolParams scriptVersion stakeAmt
          finalFakegix <- getWalletFakegix
          when (not $ finalFakegix == initialFakegix - Natural.toBigInt stakeAmt) $
             lift $ throwContractError "Incorrect amount of FAKEGIX deducted from user wallet"
    -- Admin promises reward
    withKeyWallet adminWallet do
          initialFakegix <- getWalletFakegix
          failedIndices <- lift $ depositUnbondedPoolContract depositAmt unbondedPoolParams scriptVersion (nat 0) []
          when (not $ Array.null failedIndices) $
             lift $ throwContractError "Some entries failed to be updated"
          finalFakegix <- getWalletFakegix
          when (not $ initialFakegix == finalFakegix) $
             lift $ throwContractError "The admin deposited FAKEGIX when they should not have"
    -- Admin deposits rewards
    withKeyWallet adminWallet do
          initialFakegix <- getWalletFakegix
          failedIndices <- lift $ depositUnbondedPoolContract zero unbondedPoolParams scriptVersion (nat 0) []
          when (not $ Array.null failedIndices) $
             lift $ throwContractError "Some entries failed to be updated"
          finalFakegix <- getWalletFakegix
          when (not $ finalFakegix == initialFakegix - expectedAdminDeposit) $
             lift $ throwContractError $
                "The admin did not deposit the expected amount: \nExpected: " <> show expectedAdminDeposit
                <> "\nActual deposit: " <> show (initialFakegix - finalFakegix)
          logDebug' "CCC"
