module Test.Unit.Admin.Open (test) where

import Prelude

import Contract.Test.Plutip (PlutipTest)
import Test.Common (testInitialParams, withWalletsAndPool)

-- | It doesn't do anything because `withWalletsAndPool` handles pool creation.
test :: PlutipTest
test = withWalletsAndPool testInitialParams [] \_ _ -> pure unit
