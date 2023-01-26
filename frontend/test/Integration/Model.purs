module SNet.Test.Integration.Model
  ( PoolState
  , adminDeposit
  , adminClose
  , initialState
  , userStake
  , userWithdraw
  ) where

import Prelude

import Contract.Numeric.Natural as Natural
import Contract.Numeric.Rational (Rational)
import Control.Monad.State (get, put)
import Control.Monad.State.Class (class MonadState)
import Data.BigInt (BigInt)
import Data.Foldable (sum)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import SNet.Test.Integration.Types (CommandResult(..))
import UnbondedStaking.Types (UnbondedPoolParams(..))
import Utils (roundDown, roundUp, toRational)

{- |

This module contains a type representing the full state as well as state
transition functions of the SingularityNet protocol. These are employed in
the module `Snet.Test.Integration.Arbitrary` to generate the expected
outcomes of each user interaction.

Some notes on when an interaction fails or succeeds:

1. A user stake succeeds iff the pool is open and the amount is within the
   pool's bounds.

2. A user withdrawal succeeds iff there already is some stake from this user.

3. An admin deposit succeeds iff the pool is open and some users have staked.

4. An admin close succeeds iff the pool is open.

-}

-- | A type representing the pool state.
type PoolState =
  {
    -- | Whether the pool is open or closed
    closed :: Boolean
  -- | A map from the stakers' indexes to their staked amount.
  , stakers :: Map Int Rational
  -- | A map of stakers that are candidates for the promised rewards. The value
  -- of the map is the proportion of the rewards they can get.
  , candidates :: Map Int Rational
  -- | Amount of staked assets in the pool
  , staked :: Rational
  -- | Total amount of funds in the pool. Ideally, it should be the same as the
  -- staked amount (all assets are automatically staked). However, due to
  -- rounding, this is not the case.
  , funds :: BigInt
  -- | Amount of funds promised by the admin as rewards for the next cycle.
  , promised :: BigInt
  -- | Pool parameters
  , params :: UnbondedPoolParams
  }

-- | Provides the initial state of a pool defined by its `UnbondedPoolParams`.
initialState :: UnbondedPoolParams -> PoolState
initialState ubp =
  { closed: false
  , stakers: Map.empty
  , candidates: Map.empty
  , staked: zero
  , funds: zero
  , promised: zero
  , params: ubp
  }

-- | Transition function for a user stake.
userStake
  :: forall m. MonadState PoolState m => Int -> BigInt -> m CommandResult
userStake user amt = do
  s@{ params: UnbondedPoolParams ubp } <- get
  case unit of
    _
      | Natural.toBigInt ubp.minStake > amt || Natural.toBigInt ubp.maxStake <
          amt ->
          pure $ ExpectedFailure $ "Stake amount out of bounds. Context: " <>
            show (ubp.minStake /\ ubp.maxStake /\ amt)
      | s.closed -> do
          pure $ ExpectedFailure "Stake attempted when pool is closed"
      | otherwise -> do
          put $ s
            { stakers = Map.insert user (toRational amt) s.stakers
            , staked = s.staked + toRational amt
            , funds = s.funds + amt
            }
          pure Success

-- | Transition function for a user withdraw
userWithdraw :: forall m. MonadState PoolState m => Int -> m CommandResult
userWithdraw user = do
  s <- get
  case Map.lookup user s.stakers of
    Just stakerFunds -> do
      -- A users withdraws the rounded amount he is owed
      let
        withdrawnAmt :: BigInt
        withdrawnAmt = roundDown stakerFunds
      put $ s
        { stakers = Map.delete user s.stakers
        , staked = s.staked - toRational withdrawnAmt
        , funds = s.funds - withdrawnAmt
        }
      pure Success
    Nothing -> pure $ ExpectedFailure
      "Withdraw attempted but the user is not in the pool"

-- | Transition function for an admin deposit.
adminDeposit :: forall m. MonadState PoolState m => BigInt -> m CommandResult
adminDeposit newPromise = do
  s <- get
  case unit of
    _
      | Map.isEmpty s.stakers -> pure $ ExpectedFailure
          "Deposit attempted but there are no users in the pool"
      | s.closed -> pure $ ExpectedFailure
          "Deposit attempted but pool is already closed"
      | otherwise -> do
          -- We distribute what was promised to the candidates still present
          -- in the pool.
          let
            { stakers: stakers'
            , candidates: candidates'
            , staked: staked'
            , adminDeposited
            } =
              updatePool
                s.stakers
                s.candidates
                s.promised
          put $ s
            { stakers = stakers'
            , candidates = candidates'
            -- Funds are increased by the amount the admin deposited
            , funds = s.funds + adminDeposited
            , staked = staked'
            , promised = newPromise
            }
          pure Success

-- | Transition function for an admin close.
adminClose :: forall m. MonadState PoolState m => m CommandResult
adminClose = do
  s <- get
  case unit of
    _
      | not s.closed -> do
          put $ s { closed = true }
          pure Success
      | otherwise -> pure $ ExpectedFailure
          "Close attempted but the pool is already closed"

-- | Distributes funds to candidates and generates new candidates' map.
updatePool
  :: Map Int Rational
  -> Map Int Rational
  -> BigInt
  -> { stakers :: Map Int Rational
     , candidates :: Map Int Rational
     , staked :: Rational
     , adminDeposited :: BigInt
     }
updatePool stakers candidates promised =
  let
    -- Candidates get an amount proportional to their participation
    rewards = Map.mapMaybeWithKey (\k _ -> calcRewards k) stakers
    stakers' = Map.unionWith (+) stakers rewards

    calcRewards :: Int -> Maybe Rational
    calcRewards user = case Map.lookup user candidates of
      Just participation -> Just $ participation * toRational promised
      Nothing -> Nothing

    -- We define the candidates for the next deposit and save their proportion
    -- of the rewards.
    staked = sum stakers'
    candidates' = Map.mapMaybe (\v -> Just $ updateCandidate v) stakers'

    updateCandidate :: Rational -> Rational
    updateCandidate funds = funds / staked

    -- The admin deposits the rounded up sum of all the user's rewards
    adminDeposited = roundUp $ sum rewards
  in
    { stakers: stakers', candidates: candidates', staked, adminDeposited }
