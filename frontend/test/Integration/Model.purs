module SNet.Test.Integration.Model
  ( adminDeposit
  , adminClose
  , initialState
  , userStake
  , userWithdraw
  , addPoolStates
  ) where

import Prelude

import Contract.Numeric.Natural as Natural
import Contract.Numeric.Rational (Rational)
import Control.Monad.State (State, evalState, get, put)
import Control.Monad.State.Class (class MonadState)
import Data.Array ((..))
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.Foldable (sum)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (for, for_)
import Data.Tuple.Nested ((/\))
import Effect.Exception.Unsafe (unsafeThrow)
import SNet.Test.Integration.Types
  ( AdminCommand(..)
  , AdminCommand'
  , Array3
  , CommandResult(..)
  , PoolState
  , StateMachineInputs(..)
  , StateMachineOnlyInputs
  , UserCommand(..)
  , UserCommand'
  )
import UnbondedStaking.Types (UnbondedPoolParams(..))
import Utils (roundDown, roundUp, toRational)

{- |

This module contains transition functions for the SingularityNet protocol.
These are employed in the module `Snet.Test.Integration.Arbitrary` to generate
the expected outcomes of each user interaction.

Some notes on when an interaction fails or succeeds:

1. A user stake succeeds iff the pool is open and the amount is within the
   pool's bounds.

2. A user withdrawal succeeds iff there already is some stake from this user.

3. An admin deposit succeeds iff the pool is open and some users have staked.

4. An admin close succeeds iff the pool is open.

-}

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
          let amt' = toRational amt
          put $ s
            { stakers =
                Map.alter
                  (maybe (Just amt') (\v -> Just $ amt' + v))
                  user
                  s.stakers
            , funds = s.funds + amt
            , staked = s.staked + amt'
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
        , funds = s.funds - withdrawnAmt
        , staked = s.staked - stakerFunds
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
          -- We update the rewards one final time
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
          -- We close the pool and promise no rewards
          put $ s
            { stakers = stakers'
            , candidates = candidates'
            , funds = s.funds + adminDeposited
            , staked = staked'
            , promised = (zero :: BigInt)
            , closed = true
            }
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

-- | Used for constructing specific integration tests where the commands and
-- their results are known but we want the model to generate the pool states.
addPoolStates
  :: UnbondedPoolParams
  -> StateMachineOnlyInputs
  -> StateMachineInputs
addPoolStates ubp { usersInputs: usersInputsPerCycle, adminInputs } = flip
  evalState
  s0
  do
    let
      userIds = 0 .. 1000
    poolStates <- for
      (Array.zip usersInputsPerCycle adminInputs)
      \(usersInputs /\ adminInput) -> do
        -- Execute all users' actions
        for_ (Array.zip userIds usersInputs) \(user /\ userInputs) -> do
          for_ userInputs $ \input -> case input.command of
            UserStake amt -> userStake user amt
            UserWithdraw -> userWithdraw user
        -- Execute admin action
        _ <- case adminInput.command of
          AdminDeposit amt -> adminDeposit amt
          AdminClose -> adminClose
        -- Return pool state
        get
    pure $ StateMachineInputs
      { usersInputs: usersInputsPerCycle, adminInputs, poolStates }
  where
  s0 = initialState ubp
