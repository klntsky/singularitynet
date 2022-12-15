module Test.Unit.Admin.DepositEmpty (test) where

import Prelude

import Contract.Monad (throwContractError)
import Contract.Test.Plutip (PlutipTest, withKeyWallet)
import Data.Array as Array
import Data.BigInt as BigInt
import Test.Common (getAdminWallet, getWalletFakegix, testInitialParamsNoTimeChecks, withWalletsAndPool)
import Types (ScriptVersion(..))
import UnbondedStaking.DepositPool (depositUnbondedPoolContract)
import Utils (nat)

scriptVersion :: ScriptVersion
scriptVersion = DebugNoTimeChecks

-- | The admin deposits to an empty pool
test :: PlutipTest
test = withWalletsAndPool testInitialParamsNoTimeChecks [] \wallets ubp -> do
    adminWallet <- getAdminWallet wallets
    withKeyWallet adminWallet do
        initialFakegix <- getWalletFakegix
        failedIndices <- depositUnbondedPoolContract (BigInt.fromInt 2000) ubp scriptVersion (nat 0) []
        when (not $ Array.null failedIndices) $
           throwContractError "Some entries failed to be updated"
        finalFakegix <- getWalletFakegix
        when (not $ initialFakegix == finalFakegix) $
           throwContractError "The admin deposited FAKEGIX when they should not have"
