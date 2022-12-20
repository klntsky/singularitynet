module Test.Unit.Admin.Close (test) where

import Prelude

import Contract.Monad (throwContractError)
import Contract.Test.Plutip (PlutipTest)
import Control.Monad.Reader (ask, lift)
import Data.Array as Array
import Test.Common (getAdminWallet, testInitialParamsNoTimeChecks, withKeyWallet, withWalletsAndPool)
import UnbondedStaking.ClosePool (closeUnbondedPoolContract)
import Utils (nat)

test :: PlutipTest
test = withWalletsAndPool testInitialParamsNoTimeChecks [] \wallets -> do
    adminWallet <- getAdminWallet wallets
    withKeyWallet adminWallet do
        { unbondedPoolParams, scriptVersion } <- ask
        failedIndices <- lift $ closeUnbondedPoolContract unbondedPoolParams scriptVersion (nat 0) []
        -- Make sure that return value is empty list
        when (not $ Array.null failedIndices) $
           lift $ throwContractError "Some entries failed to be updated"
