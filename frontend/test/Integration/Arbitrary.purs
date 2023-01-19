module SNet.Test.Integration.Arbitrary (arbitraryInputs) where

import Prelude

import Control.Monad.State (StateT, evalStateT, get, lift, put)
import Data.Array.NonEmpty as NonEmpty
import Data.BigInt as BigInt
import Data.Maybe (Maybe(Nothing))
import Data.Tuple (uncurry)
import SNet.Test.Integration.Types
  ( AdminCommand(..)
  , CommandResult(..)
  , InputConfig(..)
  , StateMachineInputs(..)
  , UserCommand(..)
  , UserCommand'
  , AdminCommand'
  )
import Test.QuickCheck.Gen (Gen, chooseInt, oneOf, vectorOf)

-- | A monad stack that allows us to thread some state between each randomly
-- generated action. This gives us access to history. Due to how the protocol
-- is designed, we can tell (for most actions) whether they will succeed or
-- not only based on the sequence of actions carried by the relevant user.
type Gen' s a = StateT s Gen a

evalGen' :: forall s a. s -> Gen' s a -> Gen a
evalGen' s g = evalStateT g s

-- | State used to track which user's actions are valid
type UserState =
  { hasStakeInPool :: Boolean
  }

userState0 :: UserState
userState0 = { hasStakeInPool: false }

arbitraryUserCommand :: Int -> Int -> Gen' UserState UserCommand'
arbitraryUserCommand minStake maxStake = do
  -- We choose randomly among all actions
  command <- lift $ oneOf $ NonEmpty.cons'
    ( UserStake <<< BigInt.fromInt <$> chooseInt
        minStake
        maxStake
    )
    [ pure UserWithdraw
    , pure DoNothing
    ]
  {- We assign the result of the command based on whether the user has stake
     in the pool or not.

     1. A user stake always succeeds (unless the amount is out of
        bounds, but we cannot tell that now).
     2. A user withdrawal succeeds iff there already is some stake from this
        user.
     3. A user doing nothing automatically succeeds.

    We also update the state accordingly.
  -}
  s@{ hasStakeInPool } <- get
  result <- case command of
    UserStake _ -> put (s { hasStakeInPool = true }) *> pure Success
    UserWithdraw
      | hasStakeInPool -> (put s { hasStakeInPool = false }) *> pure Success
      | otherwise -> pure (Failure Nothing)
    DoNothing -> pure Success

  pure { command, result }

-- | State used to track which admin's actions are valid
type AdminState =
  { hasClosed :: Boolean
  }

adminState0 :: AdminState
adminState0 = { hasClosed: false }

arbitraryAdminCommand :: Int -> Int -> Gen' AdminState AdminCommand'
arbitraryAdminCommand minDeposit maxDeposit = do
  command <- lift $ oneOf $ NonEmpty.cons'
    (AdminDeposit <<< BigInt.fromInt <$> chooseInt minDeposit maxDeposit)
    [ pure <<< AdminDeposit <<< BigInt.fromInt $ 0
    , pure AdminClose
    ]
  let result = Success
  pure { command, result }

arbitraryInputs :: InputConfig -> Gen StateMachineInputs
arbitraryInputs (InputConfig cfg) = do
  usersInputs :: Array (Array (Array UserCommand')) <-
    vectorOf cfg.nCycles do
      vectorOf cfg.nUsers do
        nActions <- chooseInt 1 cfg.maxUserActionsPerCycle
        vectorOf nActions
          $ evalGen' userState0
          $ uncurry arbitraryUserCommand cfg.stakeRange
  adminInputs :: Array AdminCommand' <- vectorOf cfg.nCycles
    $ evalGen' adminState0
    $ uncurry arbitraryAdminCommand cfg.depositRange
  pure $ StateMachineInputs { usersInputs, adminInputs }

