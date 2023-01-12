module Test.Integration.Types where

import Contract.Monad (Contract)
import Data.BigInt (BigInt)
import UnbondedStaking.Types (SnetInitialParams)
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Undefined (undefined)

type UserIdx = Int
type Fakegix = BigInt

-- This module has type definitions to test a state machine using purescript-quickcheck.
--
-- Datatype representing the inputs of the state machine.
-- There is one input per user/per cycle.
type StateMachineInputs =
  { userInputs :: Array (Array UserInput)
  , adminInput :: Array AdminInput
  }

type UserInput =
  { userIdx :: UserIdx
  , command :: UserCommand
  }

-- A user may either stake, withdraw or abstain from doing anything.
data UserCommand
  = UserStake BigInt
  | UserWithdraw
  | DoNothing

type AdminInput = { command :: AdminCommand }

-- The admin does not get the opportunity to do nothing, since this breaks the
-- assumptions of the protocol. Pool creation is taken for granted.
data AdminCommand
  = AdminDeposit BigInt
  | AdminClose

-- The state of the machine after each cycle.
type MachineState =
  { totalFakegix :: Fakegix
  }

-- The errors that might be thrown by a post-condition after a state
-- transition has occurred.
data IntegrationFailure =
  BadWithdrawnAmount

-- The type of an integration test. This type is generated randomly
newtype IntegrationTest = IntegrationTest
  { cycles :: Int
  , users :: Int
  , testInitialParams :: Contract () SnetInitialParams
  , stateMachineInputs :: StateMachineInputs
  }

-- TODO: Write `Arbitrary` instance for `IntegrationTest`
instance Arbitrary IntegrationTest where
  arbitrary = undefined
