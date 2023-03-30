module SNet.Test.Unit.Admin.CloseNUser (test) where

import Prelude

import Contract.Monad (Contract, throwContractError)
import Contract.Numeric.Natural as Natural
import Contract.Test.Plutip (PlutipTest, InitialUTxOs)
import Control.Monad.Reader (ask, lift)
import Data.Array ((..))
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Maybe (Maybe(..), isNothing)
import Data.Traversable (for_)
import Data.Tuple.Nested (type (/\), (/\))
import SNet.Test.Common
  ( getAdminWallet
  , getUserWallet
  , getWalletFakegix
  , waitFor
  , withKeyWallet
  , withWalletsAndPool
  )
import UnbondedStaking.ClosePool (closeUnbondedPoolContract)
import UnbondedStaking.Types
  ( Period(UserPeriod, AdminPeriod)
  , SnetInitialParams
  )
import UnbondedStaking.UserStake (userStakeUnbondedPoolContract)
import Utils (nat)

-- | The admin closes a pool with `n` user entries.
test :: Contract SnetInitialParams -> Int -> Int -> PlutipTest
test initialParams userCount batchSize = withWalletsAndPool initialParams
  usersInitialUtxos
  \wallets -> do
    adminWallet <- getAdminWallet wallets
    { unbondedPoolParams, scriptVersion } <- ask
    let stakeAmtBase = BigInt.fromInt 1000
    for_ (0 .. (userCount - 1)) \idx -> getUserWallet idx wallets >>= flip
      withKeyWallet
      do
        waitFor UserPeriod
        let stakeAmt = BigInt.fromInt (idx + 1) * stakeAmtBase
        initialFakegix <- getWalletFakegix
        _txId <- lift
          $ userStakeUnbondedPoolContract unbondedPoolParams scriptVersion
          $ Natural.fromBigInt' stakeAmt
        finalFakegix <- getWalletFakegix
        when (not $ finalFakegix == initialFakegix - stakeAmt)
          $ lift
          $ throwContractError
              "Incorrect amount of FAKEGIX deducted from user wallet"
    withKeyWallet adminWallet do
      waitFor AdminPeriod
      failedCloses <- lift $ closeUnbondedPoolContract unbondedPoolParams
        scriptVersion
        (nat batchSize)
        Nothing
      when (not $ isNothing failedCloses)
        $ lift
        $ throwContractError
        $ "Some entries failed to be closed " <> show failedCloses
  where
  usersInitialUtxos :: Array (InitialUTxOs /\ BigInt)
  usersInitialUtxos = Array.replicate userCount $
    map BigInt.fromInt [ 10_000_000, 100_000_000 ] /\
      (BigInt.fromInt 1_000_000_000)

