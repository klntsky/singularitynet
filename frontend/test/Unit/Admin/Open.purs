module SNet.Test.Unit.Admin.Open (test) where

import Prelude

import Contract.Monad (Contract)
import Contract.Test.Plutip (PlutipTest)
import SNet.Test.Common (withWalletsAndPool)
import UnbondedStaking.Types (SnetInitialParams)

-- | The admin opens the pool
test :: Contract () SnetInitialParams -> PlutipTest
test initParams = withWalletsAndPool initParams [] \_ -> pure unit
