module SNet.Test.Integration.Arbitrary (arbitraryInputs) where

import Prelude

import Control.Monad.State (StateT, evalStateT, get, lift)
import Data.Array ((..))
import Data.Array as Array
import Data.Array.NonEmpty as NonEmpty
import Data.BigInt as BigInt
import Data.Traversable (for, sequence)
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import SNet.Test.Integration.Model as Model
import SNet.Test.Integration.Types
  ( AdminCommand(..)
  , AdminCommand'
  , Array2
  , InputConfig(..)
  , StateMachineInputs(..)
  , UserCommand(..)
  , UserCommand'
  , PoolState
  )
import Test.QuickCheck.Gen (Gen, chooseInt, oneOf)
import UnbondedStaking.Types (UnbondedPoolParams)

-- | A monad stack that allows us to thread some state between each randomly
-- generated action. This gives us access to the pool state.
type Gen' s a = StateT s Gen a

-- | Pass an initial state and obtain a random generator.
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
  result <- case command of
    UserStake amt -> Model.userStake user amt
    UserWithdraw -> Model.userWithdraw user

  pure { command, result }

arbitraryAdminCommand :: Int -> Int -> Gen' PoolState AdminCommand'
arbitraryAdminCommand minDeposit maxDeposit = do
  command <- lift $ oneOf $ NonEmpty.cons'
    (AdminDeposit <<< BigInt.fromInt <$> chooseInt minDeposit maxDeposit)
    [ pure <<< AdminDeposit <<< BigInt.fromInt $ 0
    , pure AdminClose
    ]
  result <- case command of
    AdminDeposit newPromise -> Model.adminDeposit newPromise
    AdminClose -> Model.adminClose
  pure { command, result }

-- | Generate the state machine's inputs and resulting states from an input
-- specification.
arbitraryInputs :: UnbondedPoolParams -> InputConfig -> Gen StateMachineInputs
arbitraryInputs ubp cfg =
  map mkStateMachineInputs
    $ evalGen' s0
    $ arbitraryInputs' cfg
  where
  s0 :: PoolState
  s0 = Model.initialState ubp

  mkStateMachineInputs
    :: Array (Array2 UserCommand' /\ AdminCommand' /\ PoolState)
    -> StateMachineInputs
  mkStateMachineInputs = Array.foldl addToInputs emptyInputs
  addToInputs
    (StateMachineInputs i)
    (usersCommands /\ adminCommand /\ poolState) =
    StateMachineInputs $ i
      { usersInputs = Array.snoc i.usersInputs usersCommands
      , adminInputs = Array.snoc i.adminInputs adminCommand
      , poolStates = Array.snoc i.poolStates poolState
      }

  emptyInputs :: StateMachineInputs
  emptyInputs = StateMachineInputs
    { usersInputs: []
    , adminInputs: []
    , poolStates: []
    }

-- | Generate the admin and user's inputs in an interleaved manner, giving
-- the generators access to the intermediate pool state.
arbitraryInputs'
  :: InputConfig
  -> Gen' PoolState (Array (Array2 UserCommand' /\ AdminCommand' /\ PoolState))
arbitraryInputs' (InputConfig cfg) = vectorOf' cfg.nCycles do
  let userIds = 0 .. (cfg.nUsers - 1)
  usersInputs :: Array2 UserCommand' <- for userIds \id -> do
    nActions <- lift $ chooseInt 1 cfg.maxUserActionsPerCycle
    vectorOf' nActions $
      uncurry (arbitraryUserCommand id) cfg.stakeRange
  adminInput :: AdminCommand' <- uncurry arbitraryAdminCommand cfg.depositRange
  poolState <- get
  pure $ usersInputs /\ adminInput /\ poolState
  where
  vectorOf' :: forall s a. Int -> Gen' s a -> Gen' s (Array a)
  vectorOf' n g = sequence $ Array.replicate n g
