module Test.Unit.Admin.Close (test) where

import Prelude

import Contract.Monad (throwContractError)
import Contract.Test.Plutip (PlutipTest, withKeyWallet)
import Data.Array as Array
import Test.Common (getAdminWallet, testInitialParamsNoTimeChecks, withWalletsAndPool)
import Types (ScriptVersion(..))
import UnbondedStaking.ClosePool (closeUnbondedPoolContract)
import Utils (nat)

scriptVersion :: ScriptVersion
scriptVersion = DebugNoTimeChecks

test :: PlutipTest
test = withWalletsAndPool testInitialParamsNoTimeChecks [] \wallets ubp -> do
    adminWallet <- getAdminWallet wallets
    withKeyWallet adminWallet do
        failedIndices <- closeUnbondedPoolContract ubp scriptVersion (nat 0) []
        -- Make sure that return value is empty list
        when (not $ Array.null failedIndices) $
           throwContractError "Some entries failed to be updated"
        pure unit
