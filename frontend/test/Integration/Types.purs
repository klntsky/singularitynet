module SNet.Test.Integration.Types
  ( StateMachineInputs(..)
  , MachineState(..)
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

import Data.Array ((..))
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Exception as Exception
import UnbondedStaking.Types (Entry)

type UserIdx = Int
type Fakegix = BigInt

-- | Datatype representing the inputs of the state machine.
-- There is at least one input per user/per cycle. A user may perform multiple
-- actions, while the admin is expected to only perform one per cycle.
newtype StateMachineInputs =
  StateMachineInputs
    {
      -- M cycles x N users X P commands
      usersInputs :: Array3 UserCommand'
    -- M cycles
    , adminInputs :: Array AdminCommand'
    }

derive instance Generic StateMachineInputs _

instance Show StateMachineInputs where
  show = genericShow

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
    showResult (Failure _) = "[FAILURE]"

    nCycles :: Int
    nCycles = Array.length i.adminInputs
  in
    Array.foldMap showCycle
      $ Array.zip (1 .. nCycles)
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

-- | The state of the machine after each cycle.
type MachineState =
  { totalFakegix :: Fakegix
  , poolOpen :: Boolean
  , entries :: Array Entry
  }

-- | The result of executing a command.
data CommandResult
  = Success
  | Failure (Maybe Exception.Error)

derive instance Generic CommandResult _
instance Show CommandResult where
  show = genericShow

-- The errors that might be thrown by a post-condition after a state
-- transition has occurred.
data IntegrationFailure
  = ResultMismatch String CommandResult CommandResult
  | ShouldNotChangeFunds String BigInt BigInt
  | ShouldNotAddEntry String Entry

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
