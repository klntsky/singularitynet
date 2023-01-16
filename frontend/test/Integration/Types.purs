module SNet.Test.Integration.Types
  ( StateMachineInputs(..)
  , MachineState(..)
  , IntegrationFailure(..)
  , InputConfig(..)
  , UserCommand(..)
  , AdminCommand(..)
  , UserIdx
  , Fakegix
  ) where

import Prelude

import Data.BigInt (BigInt)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\))
import UnbondedStaking.Types (Entry)

type UserIdx = Int
type Fakegix = BigInt

-- This module has type definitions to test a state machine using purescript-quickcheck.
--
-- Datatype representing the inputs of the state machine.
-- There is at least one input per user/per cycle.
newtype StateMachineInputs =
  StateMachineInputs
    { userInputs :: Array (Array (Array UserCommand))
    , adminInput :: Array AdminCommand
    }

derive instance Generic StateMachineInputs _
instance Show StateMachineInputs where
  show = genericShow

-- Datatype that represents how to generate the inputs for the state machine
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

-- A user may either stake, withdraw or do nothing
data UserCommand
  = UserStake BigInt
  | UserWithdraw
  | DoNothing

derive instance Generic UserCommand _
instance Show UserCommand where
  show = genericShow

-- The admin does not get the opportunity to do nothing, since this breaks the
-- assumptions of the protocol. Pool creation is taken for granted.
data AdminCommand
  = AdminDeposit BigInt
  | AdminClose

derive instance Generic AdminCommand _
instance Show AdminCommand where
  show = genericShow

-- The state of the machine after each cycle.
type MachineState =
  { totalFakegix :: Fakegix
  , poolOpen :: Boolean
  , entries :: Array Entry
  }

-- The errors that might be thrown by a post-condition after a state
-- transition has occurred.
data IntegrationFailure =
  BadWithdrawnAmount

derive instance Generic IntegrationFailure _
instance Show IntegrationFailure where
  show = genericShow
