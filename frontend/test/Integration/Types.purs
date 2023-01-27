module SNet.Test.Integration.Types
  ( StateMachineInputs(..)
  , StateMachineOnlyInputs
  , PoolState
  , PoolState'
  , IntegrationFailure(..)
  , InputConfig(..)
  , UserCommand(..)
  , EnrichedUserCommand
  , UserCommand'
  , AdminCommand(..)
  , EnrichedAdminCommand
  , AdminCommand'
  , CommandResult(..)
  , UserIdx
  , Fakegix
  , Array2
  , Array3
  , prettyInputs
  ) where

import Prelude

import Contract.Numeric.Rational (Rational)
import Data.Array ((..))
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Exception as Exception
import UnbondedStaking.Types (Entry, UnbondedPoolParams)

type UserIdx = Int
type Fakegix = BigInt

-- | Datatype representing the inputs of the state machine and the intermediate states.
-- There is at least one input per user/per cycle. A user may perform multiple
-- actions, while the admin is expected to only perform one per cycle.
newtype StateMachineInputs =
  StateMachineInputs
    {
      -- M cycles x N users X P commands
      usersInputs :: Array3 UserCommand'
    -- M cycles
    , adminInputs :: Array AdminCommand'
    -- M cycles
    , poolStates :: Array PoolState
    }

derive instance Generic StateMachineInputs _

instance Show StateMachineInputs where
  show = genericShow

-- | Datatype representing the inputs of the state machine. 
type StateMachineOnlyInputs =
  { usersInputs :: Array3 UserCommand'
  , adminInputs :: Array AdminCommand'
  }

-- | A type representing the pool state, as used by the model.
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

-- | A stripped down version of the pool state, obtained via querying the
-- onchain data structures.
type PoolState' =
  { closed :: Boolean
  , stakers :: Map Int Rational
  , staked :: Rational
  , funds :: BigInt
  }

-- | Pretty-prints the inputs of the state machine
prettyInputs :: StateMachineInputs -> String
prettyInputs (StateMachineInputs i) =
  let
    showCycle :: (Int /\ Array2 UserCommand' /\ AdminCommand') -> String
    showCycle (cycle /\ usersCommands /\ adminCommand) = Array.fold
      [ "===== CYCLE " <> show cycle <> " =====\n"
      , "== USERS == \n"
      , showUsers $ Array.zip (0 .. (Array.length usersCommands - 1))
          usersCommands
      , "== ADMIN == \n"
      , showAdmin adminCommand
      , "===== END =====\n"
      ]

    showUsers :: Array (Int /\ Array UserCommand') -> String
    showUsers = Array.foldMap \(id /\ userCommands) ->
      Array.foldMap
        ( \{ command, result } ->
            case command of
              UserStake n ->
                show id
                  <> ": STAKES "
                  <> show (BigInt.toInt n)
                  <> " "
                  <> showResult result
                  <> "\n"
              UserWithdraw ->
                show id
                  <> ": WITHDRAWS "
                  <> " "
                  <> showResult result
                  <> "\n"
        )
        userCommands

    showAdmin :: AdminCommand' -> String
    showAdmin { command, result } = case command of
      AdminDeposit n ->
        "ADMIN: DEPOSITS "
          <> show (BigInt.toInt n)
          <> " "
          <> showResult result
          <> "\n"
      AdminClose ->
        "ADMIN: CLOSES "
          <> showResult result
          <> "\n"

    showResult :: CommandResult -> String
    showResult Success = "[SUCCESS]"
    showResult (ExpectedFailure reason) = "[FAILURE: " <> reason <> " ]"
    showResult (ExecutionFailure err) = "[FAILURE: " <> show err <> " ]"

    nCycles :: Int
    nCycles = Array.length i.adminInputs
  in
    Array.foldMap showCycle
      $ Array.zip (0 .. (nCycles - 1))
      $ Array.zip i.usersInputs i.adminInputs

-- | Datatype that represents how to generate the inputs for the state machine
newtype InputConfig = InputConfig
  { stakeRange :: Int /\ Int
  , depositRange :: Int /\ Int
  , nUsers :: Int
  , nCycles :: Int
  , maxUserActionsPerCycle :: Int
  }

derive instance Generic InputConfig _
instance Show InputConfig where
  show = genericShow

-- | A user may either stake or withdraw.
data UserCommand
  = UserStake BigInt
  | UserWithdraw

derive instance Generic UserCommand _
instance Show UserCommand where
  show = genericShow

-- | The admin does not get the opportunity to do nothing, since this breaks the
-- assumptions of the protocol. Pool creation is taken for granted.
data AdminCommand
  = AdminDeposit BigInt
  | AdminClose

derive instance Generic AdminCommand _
instance Show AdminCommand where
  show = genericShow

-- | The result of executing a command.
data CommandResult
  = Success
  | ExpectedFailure String
  | ExecutionFailure (Maybe Exception.Error)

derive instance Generic CommandResult _
instance Show CommandResult where
  show = genericShow

-- | There are two kinds of integration failures:
--   * Result mismatches: these occur when an input from a user produces a
--     success / failure when the opposite was expected.
--   * Bad transition: these occur when the state of the pool at the end of
--     a cycle is not the expected.
data IntegrationFailure
  = ResultMismatch String CommandResult CommandResult
  | BadTransition String PoolState PoolState'

derive instance Generic IntegrationFailure _
instance Show IntegrationFailure where
  show = genericShow

-- | A user's command enriched with other fields to allow execution and
-- post-conditions evaluation
type EnrichedUserCommand (r :: Row Type) =
  { command :: UserCommand
  , result :: CommandResult
  | r
  }

-- | Just a command
type UserCommand' = EnrichedUserCommand ()

-- | A user's command enriched with other fields to allow execution and
-- post-conditions evaluation
type EnrichedAdminCommand (r :: Row Type) =
  { command :: AdminCommand
  , result :: CommandResult
  | r
  }

type AdminCommand' = EnrichedAdminCommand ()

---- Types for handling nested arrays more easily
type Array2 a = Array (Array a)
type Array3 a = Array (Array2 a)
