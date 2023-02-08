module SNet.Test.Integration.User (stake, withdraw) where

import Contract.Prelude

import Contract.Numeric.Natural as Natural
import Contract.Test.Plutip (withKeyWallet)
import Contract.Wallet (KeyWallet)
import Control.Monad.Error.Class (try)
import Control.Monad.Reader (ask, lift)
import Data.BigInt (BigInt)
import SNet.Test.Common (waitFor)
import UnbondedStaking.Types
  ( Period(..)
  , SnetContract
  )
import UnbondedStaking.UserStake (userStakeUnbondedPoolContract)
import UnbondedStaking.UserWithdraw (userWithdrawUnbondedPoolContract)

stake :: KeyWallet -> BigInt -> SnetContract Unit
stake wallet amt = do
  waitFor UserPeriod
  { unbondedPoolParams, scriptVersion } <- ask
  lift $ withKeyWallet wallet do
    _txId <- userStakeUnbondedPoolContract unbondedPoolParams
      scriptVersion
      (Natural.fromBigInt' amt)
    pure unit

withdraw :: KeyWallet -> SnetContract Unit
withdraw wallet = do
  waitForWithdrawal
  { unbondedPoolParams, scriptVersion } <- ask
  lift $ withKeyWallet wallet do
    _txId <- userWithdrawUnbondedPoolContract unbondedPoolParams
      scriptVersion
    pure unit

-- | We use a wrapper for waitFor' because we don't know exactly what period
-- to wait for: a withdrawal may happen either in a user or closed period.
waitForWithdrawal :: SnetContract Unit
waitForWithdrawal = do
  result <- try $ waitFor UserPeriod
  either (const $ waitFor ClosedPeriod) pure result

