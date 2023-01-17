module SNet.Test.Integration.Admin (deposit) where

import Contract.Prelude

import Contract.Test.Plutip (withKeyWallet)
import Contract.Wallet (KeyWallet)
import Control.Monad.Reader (ask, lift)
import Data.BigInt (BigInt)
import SNet.Test.Common (waitFor)
import UnbondedStaking.DepositPool (depositUnbondedPoolContract)
import UnbondedStaking.Types (Period(UserPeriod), SnetContract)
import Utils (nat)

deposit :: KeyWallet -> BigInt -> SnetContract Unit
deposit wallet amt = do
  waitFor UserPeriod
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
