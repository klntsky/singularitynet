module Test.Unit.Admin.Open (test) where

import Prelude

import Contract.Test.Plutip (PlutipTest)
import Test.Common (testInitialParams, withWalletsAndPool)

-- | The admin opens the pool
test :: PlutipTest
test = withWalletsAndPool testInitialParams [] \_ -> pure unit
