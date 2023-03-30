module SNet.Test.Unit.Admin.AdminWithdraw (test) where

import Prelude

import Contract.Address (addressToBech32, getWalletAddress)
import Contract.Monad (Contract, liftedM, throwContractError)
import Contract.Numeric.Natural as Natural
import Contract.Test.Plutip (PlutipTest, InitialUTxOs)
import Control.Monad.Reader (ask, lift)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Tuple.Nested (type (/\), (/\))
import SNet.Test.Common
  ( getAdminWallet
  , getUserWallet
  , getWalletFakegix
  , waitFor
  , withKeyWallet
  , withWalletsAndPool
  )
import UnbondedStaking.Types (Period(..), SnetInitialParams)
import UnbondedStaking.UserStake (userStakeUnbondedPoolContract)
import UnbondedStaking.UserWithdraw (adminWithdrawUnbondedPoolContract)
import Utils (nat)

bobInitialUtxos :: InitialUTxOs /\ BigInt
bobInitialUtxos = map BigInt.fromInt [ 10_000_000, 100_000_000 ] /\
  (BigInt.fromInt 1_000_000_000)

-- | The admin deposits to a pool with one user entry.
test :: Contract SnetInitialParams -> PlutipTest
test initParams = withWalletsAndPool initParams [ bobInitialUtxos ] \wallets ->
  do
    adminWallet <- getAdminWallet wallets
    bobWallet <- getUserWallet 0 wallets
    { unbondedPoolParams, scriptVersion } <- ask
    let stakeAmt = nat 2000
    waitFor UserPeriod
    (userBech32 /\ bobInitialFakegix) <- withKeyWallet bobWallet do
      -- User stake
      initialFakegix <- getWalletFakegix
      _txId <- lift $ userStakeUnbondedPoolContract unbondedPoolParams
        scriptVersion
        stakeAmt
      finalFakegix <- getWalletFakegix
      when (not $ finalFakegix == initialFakegix - Natural.toBigInt stakeAmt)
        $ lift
        $ throwContractError
            "Incorrect amount of FAKEGIX deducted from user wallet"
      -- Get user address
      addr <- lift $ liftedM "Could not get wallet address" getWalletAddress
      bech32 <- lift $ addressToBech32 addr
      pure $ bech32 /\ initialFakegix
    -- Admin withdraws for the user
    waitFor AdminPeriod
    withKeyWallet adminWallet do
      initialFakegix <- getWalletFakegix
      _ <- lift $ adminWithdrawUnbondedPoolContract
        unbondedPoolParams
        scriptVersion
        userBech32
      finalFakegix <- getWalletFakegix
      when (not $ initialFakegix == finalFakegix)
        $ lift
        $ throwContractError
            "The admin deposited FAKEGIX when they should not have"
    -- Check that user has same amount of funds as in the beginning
    withKeyWallet bobWallet do
      bobFinalFakegix <- getWalletFakegix
      when (not $ bobInitialFakegix == bobFinalFakegix)
        $ lift
        $ throwContractError
            "The admin deposited FAKEGIX when they should not have"
