module SNet.Test.Integration.User (stake, withdraw) where

import Contract.Prelude

import Contract.Numeric.Natural as Natural
import Contract.Test.Plutip (withKeyWallet)
import Contract.Wallet (KeyWallet)
import Control.Monad.Reader (ask, lift)
import Data.BigInt (BigInt)
import SNet.Test.Common (waitFor)
import UnbondedStaking.Types (Period(UserPeriod), SnetContract)
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

withdraw :: KeyWallet -> SnetContract Unit
withdraw wallet = do
  waitFor UserPeriod
  { unbondedPoolParams, scriptVersion } <- ask
  lift $ withKeyWallet wallet do
    _txId <- userWithdrawUnbondedPoolContract unbondedPoolParams
      scriptVersion
    pure unit

{-
      * If the pool is closed, the stake should fail and the funds distribution
      should not change.
-}
