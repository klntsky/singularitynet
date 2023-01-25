module SNet.Test.Integration.User (stake, stakeCheck, withdraw, withdrawCheck) where

import Contract.Prelude

import Contract.Numeric.Natural as Natural
import Contract.Prim.ByteArray (ByteArray)
import Contract.Test.Plutip (withKeyWallet)
import Contract.Wallet (KeyWallet)
import Control.Monad.Error.Class (try)
import Control.Monad.Reader (ask, lift)
import Data.BigInt (BigInt)
import SNet.Test.Common (waitFor)
import SNet.Test.Integration.Types (CommandResult(..), IntegrationFailure(..), MachineState)
import UnbondedStaking.Types (Entry(..), Period(..), SnetContract, UnbondedPoolParams(..))
import UnbondedStaking.UserStake (userStakeUnbondedPoolContract)
import UnbondedStaking.UserWithdraw (userWithdrawUnbondedPoolContract)

stake :: KeyWallet -> BigInt -> SnetContract Unit
stake wallet amt = do
  waitFor UserPeriod
  { unbondedPoolParams, scriptVersion } <- ask
  lift $ withKeyWallet wallet do
    _txId <- userStakeUnbondedPoolContract unbondedPoolParams
      scriptVersion
      (Natural.fromBigInt' amt)
    pure unit

{- Conditions to check:
      * If the pool is closed, the stake should fail and the funds distribution
        should not change.
           -> No entry should be found with the user key
           -> The amount of funds in the user wallet should be the same
      * If the stake succeeds, the appropriate amount of funds should be moved
        from the wallet to the pool.
           -> There should be an entry with the correct `deposited` amount
           -> The amount of funds in the user wallet should be diminished by
              the stake amount
-}

stakeCheck :: BigInt -> CommandResult -> ByteArray -> MachineState -> MachineState -> Maybe IntegrationFailure
stakeCheck stakeAmt Success key _ { entries: entriesAfter, params: UnbondedPoolParams ubp }
  | not $ any (\(Entry e) -> e.key == key) entriesAfter = Just $ BadTransition "Bad succesful stake: user key not present in assoc list"
  | Natural.toBigInt ubp.minStake > stakeAmt || Natural.toBigInt ubp.maxStake < stakeAmt = Just $ BadTransition $ "Bad succesful stake: amount does not respect bounds. Context: " <> show (ubp.minStake /\ ubp.maxStake /\ stakeAmt)
  | otherwise = Nothing
stakeCheck _ _ _ { totalFakegix: totalBefore } { totalFakegix: totalAfter }
  | otherwise = Nothing

withdraw :: KeyWallet -> SnetContract Unit
withdraw wallet = do
  waitForWithdrawal
  { unbondedPoolParams, scriptVersion } <- ask
  lift $ withKeyWallet wallet do
    _txId <- userWithdrawUnbondedPoolContract unbondedPoolParams
      scriptVersion
    pure unit

{-
      * If the pool is closed, the stake should fail and the funds distribution
      should not change.
-}

withdrawCheck :: CommandResult -> ByteArray -> MachineState -> MachineState -> Maybe IntegrationFailure
withdrawCheck Success key { totalFakegix: totalBefore } { totalFakegix: totalAfter, entries: entriesAfter }
--  | totalAfter >= totalBefore = Just $ BadTransition $ "Bad successful withdraw: totalAfter >= totalBefore. Context" <> show (totalAfter /\ totalBefore)
  | any (\(Entry e) -> e.key == key) entriesAfter = Just $ BadTransition "Bad succesful withdraw: user key present in assoc list"
  | otherwise = Nothing
withdrawCheck _ _ { totalFakegix: totalBefore } { totalFakegix: totalAfter }
  | otherwise = Nothing

-- | We use a wrapper for waitFor' because we don't know exactly what period
-- to wait for: a withdrawal may happen either in a user or closed period.
waitForWithdrawal :: SnetContract Unit
waitForWithdrawal = do
   result <- try $ waitFor UserPeriod
   either (const $ waitFor ClosedPeriod) pure result

