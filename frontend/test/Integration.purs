module SNet.Test.Integration (main, runMachine') where

import Contract.Prelude

import Contract.Monad (Contract, throwContractError)
import Contract.Prim.ByteArray (ByteArray)
import Contract.Test.Plutip (PlutipTest)
import Contract.Wallet (KeyWallet)
import Control.Monad.Error.Class (liftMaybe)
import Control.Monad.Reader (lift)
import Data.Array (zip)
import Data.Array as Array
import Data.BigInt as BigInt
import Data.Tuple.Nested ((/\))
import Effect.Exception as Exception
import SNet.Test.Common (withWalletsAndPool)
import SNet.Test.Integration.Arbitrary (arbitraryInputs)
import SNet.Test.Integration.Types
  ( InputConfig(InputConfig)
  , StateMachineInputs(StateMachineInputs)
  , AdminCommand
  , UserCommand
  , MachineState
  , IntegrationFailure
  )
import Test.QuickCheck.Gen (randomSampleOne)
import UnbondedStaking.Types (SnetInitialParams, SnetContract)
import Undefined (undefined)

-- This module does integration testing on a state machine.
--
-- The idea is that, given some constraints (e.g: number of cycles of the test),
-- a value of type `StateMachineInputs` is randomly generated. This contains
-- all the interactions that the users and admin will have with the pool.
--
-- The interactions with the pools are the different commands a user/admin may
-- run. A user can either stake, withdraw or do nothing during a cycle. Whenever
-- they stake or withdraw, they do it during the user period. The admin can
-- only deposit or close the pool. Doing nothing is not an option. Again, the
-- actions are executed in the corresponding periods.
--
-- Then, the state machine is run. Each pool cycle constitutes one transition
-- of the state machine. For each interaction in the cycle (e.g: `UserStake`) a
-- `KeyWallet -> SnetContract ()` is executed. The state of the pool at
-- the end of the cycle is then checked against a number of post-conditions
-- of type `MachineState -> MachineState -> Maybe IntegrationError`, all
-- derived from the user interactions. If no interaction throws an
-- `IntegrationError`, then the test succeeded.

main :: Effect Unit
main = do
  log "HELLO THERE"

runMachine'
  ::
     -- | Initial state of the machine
     Contract () SnetInitialParams
  -- | Inputs (randomly generated)
  -> InputConfig
  -- | Transitions for `AdminCommand` inputs
  -> (AdminCommand -> KeyWallet -> SnetContract Unit)
  -- | Transitions for `UserCommand` inputs
  -> (UserCommand -> KeyWallet -> SnetContract Unit)
  -- | Post-conditions for `AdminCommand` transitions
  -> (AdminCommand -> MachineState -> MachineState -> Maybe IntegrationFailure)
  -- | Post-conditions for `UserCommand` transitions
  -> ( UserCommand
       -> ByteArray
       -> MachineState
       -> MachineState
       -> Maybe IntegrationFailure
     )
  -> PlutipTest
runMachine'
  initialParams
  inputConfig@(InputConfig inputCfg)
  transAdmin
  transUser
  condAdmin
  condUser = do
  let
    distr =
      Array.replicate inputCfg.nUsers
        $ map BigInt.fromInt [ 10_000_000, 100_000_000 ]
        /\
          (BigInt.fromInt 1_000_000_000)

  withWalletsAndPool initialParams distr \wallets -> do
    -- Generate random inputs
    (StateMachineInputs { userInputs, adminInput }) <- liftEffect
      $ randomSampleOne
      $ arbitraryInputs inputConfig
    -- Get the wallets of users and admin
    userWallets <- liftMaybe (Exception.error "Could not get user wallets") $
      Array.tail wallets
    adminWallet <- liftMaybe (Exception.error "Could not get admin wallet") $
      Array.head wallets
    -- Obtain keys of all users. Necessary for post-condition checks
    keys <- getUserKeys userWallets
    let
      loopInputs = zip ((\u -> zip u $ zip keys userWallets) <$> userInputs)
        adminInput
    for_ loopInputs
      \(usersCommands /\ adminCommand) ->
        do
          -- Get initial state of the machine
          machineState0 <- getMachineState
          -- Execute commands from all users
          for_ usersCommands \(userCommands /\ _ /\ userWallet) -> do
            traverse_ (\command -> transUser command userWallet) userCommands
          -- Get state of the machine after
          machineState1 <- getMachineState
          -- Execute all user checks
          let
            userErrors =
              Array.catMaybes $ Array.concatMap
                ( \(userCommands /\ userKey /\ _) ->
                    map
                      ( \command -> condUser command userKey machineState0
                          machineState1
                      )
                      userCommands
                )
                usersCommands
          when (not $ Array.null userErrors)
            $ lift
            $ throwContractError
            $ "Failure during user checks: "
            <> show userErrors
          -- Get state of the machine
          machineState2 <- getMachineState
          -- Execute admin command
          transAdmin adminCommand adminWallet
          -- Get state of the machine after
          machineState3 <- getMachineState
          -- Execute admin check
          let
            adminCheckResult = condAdmin adminCommand machineState2
              machineState3
          case adminCheckResult of
            Just e -> lift $ throwContractError $ "Failure during admin check: "
              <>
                show e
            _ -> pure unit

-- Execute checks from all users

-- TODO
getUserKeys :: Array KeyWallet -> SnetContract (Array ByteArray)
getUserKeys = undefined

-- TODO
getMachineState :: SnetContract MachineState
getMachineState = undefined

-- TODO: How to link each user action to a given user key?
-- * runMachine must provide the previous and current state of the pool
-- * and the key of the relevant user
-- * and the result of the contract execution
