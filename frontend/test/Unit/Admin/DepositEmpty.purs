module SNet.Test.Unit.Admin.DepositEmpty (test) where

import Prelude

import Contract.Monad (Contract, throwContractError)
import Contract.Test.Plutip (PlutipTest)
import Control.Monad.Error.Class (try)
import Control.Monad.Reader (ask, lift)
import Data.BigInt as BigInt
import Data.Either (isRight)
import Data.Maybe (Maybe(Nothing))
import SNet.Test.Common
  ( getAdminWallet
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
    result <- try $ lift $ depositUnbondedPoolContract (BigInt.fromInt 2000)
      unbondedPoolParams
      scriptVersion
      (nat 0)
      Nothing
    when (isRight result)
      $ lift
      $ throwContractError "Deposit should have failed"
