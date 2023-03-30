module Test.Unit.User.StakeAndWithdraw (test) where

import Prelude

import Contract.Log (logInfo')
import Contract.Monad (Contract, throwContractError)
import Contract.Numeric.Natural as Natural
import Contract.Test.Plutip (PlutipTest, InitialUTxOs)
import Control.Monad.Reader (ask, lift)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Tuple.Nested (type (/\), (/\))
import SNet.Test.Common
  ( getUserWallet
  , getWalletFakegix
  , waitFor
  , withKeyWallet
  , withWalletsAndPool
  )
import UnbondedStaking.Types (Period(..), SnetInitialParams)
import UnbondedStaking.UserStake (userStakeUnbondedPoolContract)
import UnbondedStaking.UserWithdraw (userWithdrawUnbondedPoolContract)
import Utils (nat)

bobInitialUtxos :: InitialUTxOs /\ BigInt
bobInitialUtxos = map BigInt.fromInt [ 10_000_000, 100_000_000 ] /\
  (BigInt.fromInt 1_000_000_000)

-- | The user stakes and withdraws in the same period
test :: Contract SnetInitialParams -> PlutipTest
test initParams = withWalletsAndPool initParams [ bobInitialUtxos ] \wallets ->
  do
    let stakeAmt = nat 2000
    waitFor UserPeriod
    bobWallet <- getUserWallet 0 wallets
    withKeyWallet bobWallet do
      initialFakegix <- getWalletFakegix
      { unbondedPoolParams, scriptVersion } <- ask
      _txId <- lift $ userStakeUnbondedPoolContract unbondedPoolParams
        scriptVersion
        stakeAmt
      finalFakegix <- getWalletFakegix
      when (not $ finalFakegix == initialFakegix - Natural.toBigInt stakeAmt)
        $ lift
        $ throwContractError
            "Incorrect amount of FAKEGIX deducted from user wallet"
      lift $ logInfo' "STAKE SUCCEEDED"
    waitFor UserPeriod
    withKeyWallet bobWallet do
      initialFakegix <- getWalletFakegix
      { unbondedPoolParams, scriptVersion } <- ask
      _txId <- lift $ userWithdrawUnbondedPoolContract unbondedPoolParams
        scriptVersion
      finalFakegix <- getWalletFakegix
      when (not $ finalFakegix == initialFakegix + Natural.toBigInt stakeAmt)
        $ lift
        $ throwContractError
            "Incorrect amount of FAKEGIX withdrawn to user wallet"

