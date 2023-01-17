module SNet.Test.Integration.User (stake) where

import Contract.Prelude

import Contract.Numeric.Natural as Natural
import Contract.Test.Plutip (withKeyWallet)
import Contract.Wallet (KeyWallet)
import Control.Monad.Reader (ask, lift)
import Data.BigInt (BigInt)
import SNet.Test.Common (waitFor)
import UnbondedStaking.Types (Period(UserPeriod), SnetContract)
import UnbondedStaking.UserStake (userStakeUnbondedPoolContract)

stake :: KeyWallet -> BigInt -> SnetContract Unit
stake wallet amt = do
  waitFor UserPeriod
  { unbondedPoolParams, scriptVersion } <- ask
  lift $ withKeyWallet wallet do
    _txId <- userStakeUnbondedPoolContract unbondedPoolParams
      scriptVersion
      (Natural.fromBigInt' amt)
    pure unit
