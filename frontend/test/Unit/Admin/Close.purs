module SNet.Test.Unit.Admin.Close (test) where

import Prelude

import Contract.Monad (Contract, throwContractError)
import Contract.Test.Plutip (PlutipTest)
import Control.Monad.Reader (ask, lift)
import Data.Maybe (Maybe(..), isNothing)
import SNet.Test.Common
  ( getAdminWallet
  , waitFor
  , withKeyWallet
  , withWalletsAndPool
  )
import UnbondedStaking.ClosePool (closeUnbondedPoolContract)
import UnbondedStaking.Types (Period(..), SnetInitialParams)
import Utils (nat)

test :: Contract SnetInitialParams -> PlutipTest
test initParams = withWalletsAndPool initParams [] \wallets -> do
  adminWallet <- getAdminWallet wallets
  withKeyWallet adminWallet do
    waitFor AdminPeriod
    { unbondedPoolParams, scriptVersion } <- ask
    failedCloses <- lift $ closeUnbondedPoolContract unbondedPoolParams
      scriptVersion
      (nat 0)
      Nothing
    -- Make sure that return value is empty list
    when (not $ isNothing failedCloses)
      $ lift
      $ throwContractError "Some entries failed to be updated"
