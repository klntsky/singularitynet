module SNet.Test.Integration.Arbitrary (arbitraryInputs) where

import Prelude

import Control.Monad.State (StateT, evalStateT, get, lift, put)
import Contract.Numeric.Natural as Natural
import Data.Array ((..))
import Data.Array as Array
import Data.Array.NonEmpty as NonEmpty
import Data.BigInt as BigInt
import Data.Maybe (Maybe(Nothing))
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (for, sequence)
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import SNet.Test.Integration.Types (AdminCommand(..), AdminCommand', Array2, CommandResult(..), InputConfig(..), StateMachineInputs(..), UserCommand(..), UserCommand')
import Test.QuickCheck.Gen (Gen, chooseInt, oneOf)
import UnbondedStaking.Types (UnbondedPoolParams(..))

-- | A monad stack that allows us to thread some state between each randomly
-- generated action. This gives us access to the pool state.
type Gen' s a = StateT s Gen a

-- | A type representing a simplified version of the pool state.
-- FIXME: Add staking bounds to allow accurate prediction of user stake failure
type PoolState =
  {
    -- | Whether the pool is open or closed
    closed :: Boolean
  -- | The stakers in the pool referenced by index
  , stakers :: Set Int
  -- | Pool parameters
  , params :: UnbondedPoolParams
  }

evalGen' :: forall s a. s -> Gen' s a -> Gen a
evalGen' s g = evalStateT g s

arbitraryUserCommand :: Int -> Int -> Int -> Gen' PoolState UserCommand'
arbitraryUserCommand user minStake maxStake = do
  -- We choose randomly among all actions
  command <- lift $ oneOf $ NonEmpty.cons'
    ( UserStake <<< BigInt.fromInt <$> chooseInt
        minStake
        maxStake
    )
    [ pure UserWithdraw ]
  {- 1. A user stake succeeds iff the pool is open (unless the amount is out of
        bounds, but we cannot tell that now).
     2. A user withdrawal succeeds iff there already is some stake from this
        user.
     3. A user doing nothing automatically succeeds.
  -}
  s@{ stakers, closed, params: UnbondedPoolParams ubp } <- get
  result <- case command of
    UserStake amt
      | Natural.toBigInt ubp.minStake > amt || Natural.toBigInt ubp.maxStake < amt ->
          pure $ ExpectedFailure $ "Stake amount out of bounds. Context: " <> show (ubp.minStake /\ ubp.maxStake /\ amt)
      | closed -> do
          pure $ ExpectedFailure "Stake attempted when pool is closed"
      | otherwise -> do
          put $ s { stakers = Set.insert user stakers }
          pure Success
    UserWithdraw
      | Set.member user stakers -> do
          put $ s { stakers = Set.delete user stakers }
          pure Success
      | otherwise -> pure $ ExpectedFailure "Withdraw attempted but the user is not in the pool"

  pure { command, result }

arbitraryAdminCommand :: Int -> Int -> Gen' PoolState AdminCommand'
arbitraryAdminCommand minDeposit maxDeposit = do
  command <- lift $ oneOf $ NonEmpty.cons'
    (AdminDeposit <<< BigInt.fromInt <$> chooseInt minDeposit maxDeposit)
    [ pure <<< AdminDeposit <<< BigInt.fromInt $ 0
    , pure AdminClose
    ]
  {- 1. An admin deposit succeeds iff the pool is open and the users have
        staked.
     2. An admin close succeeds iff the pool is open.
  -}
  s@{ stakers, closed } <- get
  result <- case command of
    AdminDeposit _
      | not Set.isEmpty stakers && not closed -> pure Success
      | otherwise -> pure $ ExpectedFailure "Deposit attempted but there are no users in the pool"
    AdminClose
      | not closed -> do
          put $ s { closed = true }
          pure Success
      | otherwise -> pure $ ExpectedFailure "Close attempted but the pool is already closed"
  pure { command, result }

-- | Generate the state machine's inputs (along with their result tags) from an
-- input specification
arbitraryInputs :: UnbondedPoolParams -> InputConfig -> Gen StateMachineInputs
arbitraryInputs ubp cfg =
  map mkStateMachineInputs
    $ evalGen' s0
    $ arbitraryInputs' cfg
  where
  s0 :: PoolState
  s0 = { closed: false, stakers: Set.empty, params: ubp }

  mkStateMachineInputs
    :: Array (Array2 UserCommand' /\ AdminCommand') -> StateMachineInputs
  mkStateMachineInputs = Array.foldl addToInputs emptyInputs
  addToInputs (StateMachineInputs i) (usersCommands /\ adminCommand) =
    StateMachineInputs $ i
      { usersInputs = Array.snoc i.usersInputs usersCommands
      , adminInputs = Array.snoc i.adminInputs adminCommand
      }

  emptyInputs :: StateMachineInputs
  emptyInputs = StateMachineInputs
    { usersInputs: []
    , adminInputs: []
    }

-- | Generate the admin and user's inputs in an interleaved manner, giving
-- the generators access to the intermediate pool state.
arbitraryInputs'
  :: InputConfig
  -> Gen' PoolState (Array (Array2 UserCommand' /\ AdminCommand'))
arbitraryInputs' (InputConfig cfg) = vectorOf' cfg.nCycles do
  let userIds = 0 .. (cfg.nUsers - 1)
  usersInputs :: Array2 UserCommand' <- for userIds \id -> do
    nActions <- lift $ chooseInt 1 cfg.maxUserActionsPerCycle
    vectorOf' nActions $
      uncurry (arbitraryUserCommand id) cfg.stakeRange
  adminInput :: AdminCommand' <- uncurry arbitraryAdminCommand cfg.depositRange
  pure $ usersInputs /\ adminInput
  where
  vectorOf' :: forall s a. Int -> Gen' s a -> Gen' s (Array a)
  vectorOf' n g = sequence $ Array.replicate n g
