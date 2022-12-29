module SNet.Test.Unit.Admin.DepositEmpty (test) where

import Prelude

import Contract.Monad (Contract, throwContractError)
import Contract.Test.Plutip (PlutipTest)
import Control.Monad.Reader (ask, lift)
import Data.Array as Array
import Data.BigInt as BigInt
import SNet.Test.Common
  ( getAdminWallet
  , getWalletFakegix
  , waitFor
  , withKeyWallet
  , withWalletsAndPool
  )
import UnbondedStaking.DepositPool (depositUnbondedPoolContract)
import UnbondedStaking.Types (Period(..), SnetInitialParams)
import Utils (nat)

-- | The admin deposits to an empty pool
test :: Contract () SnetInitialParams -> PlutipTest
test initParams = withWalletsAndPool initParams [] \wallets -> do
  adminWallet <- getAdminWallet wallets
  { unbondedPoolParams, scriptVersion } <- ask
  withKeyWallet adminWallet do
    waitFor AdminPeriod
    initialFakegix <- getWalletFakegix
    failedIndices <- lift $ depositUnbondedPoolContract (BigInt.fromInt 2000)
      unbondedPoolParams
      scriptVersion
      (nat 0)
      []
    when (not $ Array.null failedIndices)
      $ lift
      $ throwContractError "Some entries failed to be updated"
    finalFakegix <- getWalletFakegix
    when (not $ initialFakegix == finalFakegix)
      $ lift
      $ throwContractError
          "The admin deposited FAKEGIX when they should not have"
