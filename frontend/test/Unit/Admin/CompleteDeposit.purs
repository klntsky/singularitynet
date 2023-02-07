module SNet.Test.Unit.Admin.CompleteDeposit (test) where

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
import Data.Newtype (unwrap)
import Data.Traversable (for_)
import Data.Tuple.Nested (type (/\), (/\))
import SNet.Test.Common
  ( getAdminWallet
  , getUserWallet
  , getWalletFakegix
  , waitFor
  , waitForNext
  , withKeyWallet
  , withWalletsAndPool
  )
import UnbondedStaking.DepositPool (depositUnbondedPoolContract)
import UnbondedStaking.Types
  ( Entry(..)
  , IncompleteDeposit(..)
  , Period(..)
  , SnetInitialParams
  )
import UnbondedStaking.UserStake (userStakeUnbondedPoolContract)
import UnbondedStaking.Utils (queryAssocListUnbonded)
import Utils (nat)

-- | The admin deposits to a pool with `n` user entries. We take into account
-- any rounding error, since this is a multi-user scenario.
test :: Contract () SnetInitialParams -> Int -> PlutipTest
test initParams userCount = withWalletsAndPool initParams
  usersInitialUtxos
  \wallets -> do
    adminWallet <- getAdminWallet wallets
    { unbondedPoolParams, scriptVersion } <- ask
    let
      stakeAmtBase = BigInt.fromInt 1000
      depositAmt = BigInt.fromInt 10_000
      expectedAdminDeposit = depositAmt
      maxRoundingError = BigInt.fromInt userCount
    waitFor UserPeriod
    for_ (0 .. (userCount - 1)) \idx -> getUserWallet idx wallets >>= flip
      withKeyWallet
      do
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
      initialFakegix <- getWalletFakegix
      failedDeposits <- lift $ depositUnbondedPoolContract depositAmt
        unbondedPoolParams
        scriptVersion
        (nat zero)
        Nothing
      when (not $ isNothing failedDeposits)
        $ lift
        $ throwContractError
        $ "Some entries failed to be updated " <> show failedDeposits
      finalFakegix <- getWalletFakegix
      when (not $ initialFakegix == finalFakegix)
        $ lift
        $ throwContractError
            "The admin deposited FAKEGIX when they should not have"
    withKeyWallet adminWallet $ do
      waitForNext AdminPeriod
      initialFakegix <- getWalletFakegix
      -- We query the assoc list and create two `IncompleteDeposit` values
      entries <- lift $ queryAssocListUnbonded unbondedPoolParams scriptVersion
      let
        keys = map (unwrap >>> _.key) entries
        incompleteDeposit1@(IncompleteDeposit i1) = IncompleteDeposit
          { failedKeys: Array.take 3 keys
          , totalDeposited: Array.foldl
              ( \acc (Entry { deposited, newDeposit }) -> acc + deposited +
                  newDeposit
              )
              zero
              entries
          , nextDepositAmt: zero
          }
        incompleteDeposit2 =
          IncompleteDeposit $ i1
            { failedKeys = Array.drop 3 keys
            }
      -- Deposit to first three users
      failedDeposits1 <- lift $ depositUnbondedPoolContract depositAmt
        unbondedPoolParams
        scriptVersion
        (nat zero)
        (Just incompleteDeposit1)
      when (not $ isNothing failedDeposits1)
        $ lift
        $ throwContractError
        $ "Some entries failed to be updated " <> show failedDeposits1
      -- Deposit to last two users
      failedDeposits2 <- lift $ depositUnbondedPoolContract depositAmt
        unbondedPoolParams
        scriptVersion
        (nat zero)
        (Just incompleteDeposit2)
      when (not $ isNothing failedDeposits2)
        $ lift
        $ throwContractError
        $ "Some entries failed to be updated " <> show failedDeposits2
      -- Validate spent FAKEGIX amount
      finalFakegix <- getWalletFakegix
      let realAdminDeposit = initialFakegix - finalFakegix
      when
        ( not $ realAdminDeposit >= expectedAdminDeposit && realAdminDeposit <=
            expectedAdminDeposit + maxRoundingError
        )
        $ lift
        $ throwContractError
        $
          "The admin did not deposit the expected amount\n"
            <> "Expected: "
            <> show expectedAdminDeposit
            <> "(+ "
            <> show maxRoundingError
            <> ")\n"
            <> "Actual deposit: "
            <> show realAdminDeposit
  where
  usersInitialUtxos :: Array (InitialUTxOs /\ BigInt)
  usersInitialUtxos = Array.replicate userCount $
    map BigInt.fromInt [ 10_000_000, 100_000_000 ] /\
      (BigInt.fromInt 1_000_000_000)

