module SNet.Test.Unit.Admin.Deposit1User (test) where

import Prelude

import Contract.Monad (Contract, throwContractError)
import Contract.Numeric.Natural as Natural
import Contract.Test.Plutip (PlutipTest, InitialUTxOs)
import Control.Monad.Reader (ask, lift)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Maybe (Maybe(..), isNothing)
import Data.Tuple.Nested (type (/\), (/\))
import SNet.Test.Common
  ( getAdminWallet
  , getUserWallet
  , getWalletFakegix
  , waitFor
  , waitForNext
  , withKeyWallet
  , withWalletsAndPool
  )
import UnbondedStaking.DepositPool (depositUnbondedPoolContract)
import UnbondedStaking.Types (Period(..), SnetInitialParams)
import UnbondedStaking.UserStake (userStakeUnbondedPoolContract)
import Utils (nat)

bobInitialUtxos :: InitialUTxOs /\ BigInt
bobInitialUtxos = map BigInt.fromInt [ 10_000_000, 100_000_000 ] /\
  (BigInt.fromInt 1_000_000_000)

-- | The admin deposits to a pool with one user entry.
test :: Contract () SnetInitialParams -> PlutipTest
test initParams = withWalletsAndPool initParams [ bobInitialUtxos ] \wallets ->
  do
    adminWallet <- getAdminWallet wallets
    bobWallet <- getUserWallet 0 wallets
    { unbondedPoolParams, scriptVersion } <- ask
    let
      stakeAmt = nat 2000
      depositAmt = BigInt.fromInt 10_000
      --interestRate = _.interest $ unwrap unbondedPoolParams
      expectedAdminDeposit = depositAmt
    -- User stake
    waitFor UserPeriod
    withKeyWallet bobWallet do
      initialFakegix <- getWalletFakegix
      _txId <- lift $ userStakeUnbondedPoolContract unbondedPoolParams
        scriptVersion
        stakeAmt
      finalFakegix <- getWalletFakegix
      when (not $ finalFakegix == initialFakegix - Natural.toBigInt stakeAmt)
        $ lift
        $ throwContractError
            "Incorrect amount of FAKEGIX deducted from user wallet"
    -- Admin promises reward
    waitFor AdminPeriod
    withKeyWallet adminWallet do
      initialFakegix <- getWalletFakegix
      failedDeposits <- lift $ depositUnbondedPoolContract depositAmt
        unbondedPoolParams
        scriptVersion
        (nat 0)
        Nothing
      when (not $ isNothing failedDeposits)
        $ lift
        $ throwContractError "Some entries failed to be updated"
      finalFakegix <- getWalletFakegix
      when (not $ initialFakegix == finalFakegix)
        $ lift
        $ throwContractError
            "The admin deposited FAKEGIX when they should not have"
    -- Admin deposits rewards
    waitForNext AdminPeriod
    withKeyWallet adminWallet do
      initialFakegix <- getWalletFakegix
      failedDeposits <- lift $ depositUnbondedPoolContract zero
        unbondedPoolParams
        scriptVersion
        (nat 0)
        Nothing
      when (not $ isNothing failedDeposits)
        $ lift
        $ throwContractError "Some entries failed to be updated"
      finalFakegix <- getWalletFakegix
      when (not $ finalFakegix == initialFakegix - expectedAdminDeposit)
        $ lift
        $ throwContractError
        $
          "The admin did not deposit the expected amount: \nExpected: "
            <> show expectedAdminDeposit
            <> "\nActual deposit: "
            <> show (initialFakegix - finalFakegix)
