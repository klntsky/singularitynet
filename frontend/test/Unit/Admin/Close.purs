module Test.Unit.Admin.Close (test) where

import Prelude

import Contract.Test.Plutip (PlutipTest, withKeyWallet)
import Test.Common (getAdminWallet, testInitialParamsNoTimeChecks, withWalletsAndPool)
import Types (ScriptVersion(..))
import UnbondedStaking.ClosePool (closeUnbondedPoolContract)
import Utils (nat)

test :: PlutipTest
test = withWalletsAndPool testInitialParamsNoTimeChecks [] \wallets ubp -> do
    adminWallet <- getAdminWallet wallets
    withKeyWallet adminWallet do
        _ <- closeUnbondedPoolContract ubp DebugNoTimeChecks (nat 0) []
        pure unit
