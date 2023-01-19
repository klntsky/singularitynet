module SNet.Test.Integration.Admin (deposit, close) where

import Contract.Prelude

import Contract.Test.Plutip (withKeyWallet)
import Contract.Wallet (KeyWallet)
import Control.Monad.Reader (ask, lift)
import Data.BigInt (BigInt)
import SNet.Test.Common (waitFor)
import UnbondedStaking.DepositPool (depositUnbondedPoolContract)
import UnbondedStaking.Types (Period(AdminPeriod), SnetContract)
import Utils (nat)

deposit :: KeyWallet -> BigInt -> SnetContract Unit
deposit wallet amt = do
  waitFor AdminPeriod
  { unbondedPoolParams, scriptVersion } <- ask
  lift $ withKeyWallet wallet do
    _txId <- depositUnbondedPoolContract
      amt
      unbondedPoolParams
      scriptVersion
      -- No batching 
      (nat 0)
      []
    pure unit

close :: KeyWallet -> SnetContract Unit
close wallet = do
  waitFor AdminPeriod
  { unbondedPoolParams, scriptVersion } <- ask
  lift $ withKeyWallet wallet do
    -- FIXME: Use result
    _ <- depositUnbondedPoolContract zero
      unbondedPoolParams
      scriptVersion
      (nat 0)
      []
    pure unit
