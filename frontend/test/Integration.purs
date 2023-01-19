module SNet.Test.Integration (main, runMachine') where

import Contract.Prelude

import Contract.Address (ownPaymentPubKeyHash)
import Contract.Monad (Contract, launchAff_, liftedM, throwContractError)
import Contract.Prim.ByteArray (ByteArray)
import Contract.Test.Mote (interpretWithConfig)
import Contract.Test.Plutip (PlutipTest, testPlutipContracts)
import Contract.Wallet (KeyWallet)
import Control.Monad.Error.Class (liftMaybe, try)
import Control.Monad.Reader (ask, lift)
import Data.Array (zip)
import Data.Array as Array
import Data.BigInt as BigInt
import Data.Tuple.Nested ((/\))
import Effect.Exception as Exception
import Mote (MoteT, group, test)
import SNet.Test.Common
  ( localPlutipCfg
  , testConfig
  , testInitialParamsNoTimeChecks
  , withKeyWallet
  , withWalletsAndPool
  )
import SNet.Test.Integration.Admin as Admin
import SNet.Test.Integration.Arbitrary (arbitraryInputs)
import SNet.Test.Integration.Types
  ( AdminCommand(..)
  , Array3
  , CommandResult(..)
  , EnrichedUserCommand
  , InputConfig(InputConfig)
  , IntegrationFailure(..)
  , MachineState
  , StateMachineInputs(StateMachineInputs)
  , UserCommand(..)
  , UserCommand'
  )
import SNet.Test.Integration.User as User
import Test.QuickCheck.Gen (randomSampleOne)
import UnbondedStaking.Types (SnetInitialParams, SnetContract)
import UnbondedStaking.Utils
  ( queryAssocListUnbonded
  , queryStateUnbonded
  , queryAssetsUnbonded
  )
import Undefined (undefined)
import Utils (hashPkh)

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
main = launchAff_ $ interpretWithConfig testConfig $ testPlutipContracts
  localPlutipCfg
  suite

suite :: MoteT Aff PlutipTest Aff Unit
suite =
  group "Integration tests" do
    test "Hardcoded" $
      runMachine'
        testInitialParamsNoTimeChecks
        (Left fixedInputs)
        adminTransition
        userTransition
        adminChecks
        userChecks
    test "Random" $ runMachine testInitialParamsNoTimeChecks inputCfg

runMachine
  ::
     -- | Initial state of the machine
     Contract () SnetInitialParams
  -- | Inputs (randomly generated)
  -> InputConfig
  -> PlutipTest
runMachine initParams inputConfig =
  runMachine'
    initParams
    (Right inputConfig)
    adminTransition
    userTransition
    adminChecks
    userChecks

-- FIXME
fixedInputs :: StateMachineInputs
fixedInputs = undefined

-- StateMachineInputs
--   { userInputs: [ [ [ UserStake $ BigInt.fromInt 1000 ] ] ]
--   , adminInput: [ AdminDeposit $ BigInt.fromInt 1000 ]
--   }

inputCfg :: InputConfig
inputCfg = InputConfig
  { stakeRange: 1000 /\ 2000
  , depositRange: 1000 /\ 2000
  , nUsers: 1
  , nCycles: 1
  , maxUserActionsPerCycle: 1
  }

runMachine'
  ::
     -- | Initial state of the machine
     Contract () SnetInitialParams
  -- | Either the inputs or the inputs config to generate them randomly
  -> Either StateMachineInputs InputConfig
  -- | Transitions for `AdminCommand` inputs
  -> (AdminCommand -> KeyWallet -> SnetContract Unit)
  -- | Transitions for `UserCommand` inputs
  -> (UserCommand -> KeyWallet -> SnetContract Unit)
  -- | Post-conditions for `AdminCommand` transitions
  -> ( AdminCommand
       -> CommandResult
       -> MachineState
       -> MachineState
       -> Maybe IntegrationFailure
     )
  -- | Post-conditions for `UserCommand` transitions
  -> ( UserCommand
       -> CommandResult
       -> ByteArray
       -> MachineState
       -> MachineState
       -> Maybe IntegrationFailure
     )
  -> PlutipTest
runMachine'
  initialParams
  eitherInputsConfig
  transAdmin
  transUser
  condAdmin
  condUser = do
  let
    nUsers = 1
    distr =
      Array.replicate nUsers
        $ map BigInt.fromInt [ 10_000_000, 100_000_000 ]
        /\
          (BigInt.fromInt 1_000_000_000)
  withWalletsAndPool initialParams distr \wallets -> do
    -- Get inputs
    (StateMachineInputs { usersInputs, adminInputs: adminCommandsPerCycle }) <-
      either pure genInputs eitherInputsConfig
    -- Get the wallets of users and admin
    usersWallets <- liftMaybe (Exception.error "Could not get user wallets") $
      Array.tail wallets
    adminWallet <- liftMaybe (Exception.error "Could not get admin wallet") $
      Array.head wallets
    -- Obtain keys of all users.
    usersKeys <- getUserKeys usersWallets
    let
      -- Enrich commands with this additional information
      usersCommandsPerCycle
        :: Array3 (EnrichedUserCommand (wallet :: KeyWallet, key :: ByteArray))
      usersCommandsPerCycle =
        map addWalletsAndKeys <$> map
          (\userInputs -> zip usersWallets (zip usersKeys userInputs))
          usersInputs
    -- For each cycle, execute users and admin's actions, validate results and
    -- evaluate post-conditions
    for_ (zip usersCommandsPerCycle adminCommandsPerCycle)
      \(usersCommands /\ adminCommand) ->
        do
          machineState0 <- getMachineState
          -- Execute all commands for each user and validate the results with the
          -- expected ones.
          results <- for usersCommands \userCommands ->
            for userCommands \{ command, wallet, result: expectedResult } -> do
              executionResult <- toResult $ transUser command wallet
              validateResult expectedResult executionResult
          pure unit
          machineState1 <- getMachineState
          -- Evaluate all users' post-conditions and report them
          let
            userErrors :: Array IntegrationFailure
            userErrors =
              Array.catMaybes
                <<<
                  ( Array.concatMap $
                      map
                        ( \{ command, result, key } -> condUser command result
                            key
                            machineState0
                            machineState1
                        )
                  )
                $ usersCommands

          when (not $ Array.null userErrors)
            $ lift
            $ throwContractError
            $ "Failure during user checks: "
            <> show userErrors
          -- Execute admin command and validate result
          let { command, result: expectedResult } = adminCommand
          executionResult <- toResult $ transAdmin command adminWallet
          _ <- validateResult expectedResult executionResult
          machineState2 <- getMachineState
          -- Evaluate admin's post-conditions and report them
          let
            adminError :: Maybe IntegrationFailure
            adminError = condAdmin command expectedResult machineState1
              machineState2
          case adminError of
            Just e ->
              lift
                $ throwContractError
                $ "Failure during admin checks: "
                <> show e
            Nothing -> pure unit

-- | Converts a `SnetContract Unit` into a `SnetContract CommandResult` by
-- wrapping the contract with a `try` and capturing the error.
toResult :: SnetContract Unit -> SnetContract CommandResult
toResult = map (either (Failure <<< Just) $ const Success) <<< try

-- | Compares the command execution's result with the expected result.
-- If they match, it returns the executtion's result. Otherwise, it throws
-- an integration error.
validateResult
  ::
     -- | ^ Expected result
     CommandResult
  ->
  -- | ^ Execution result
  CommandResult
  -> SnetContract CommandResult
validateResult Success Success = pure Success
validateResult (Failure _) e@(Failure _) = pure e
validateResult r1 r2 = lift $ throwContractError $ ResultMismatch
  "There was a mismatch between the expected and the obtained result"
  r1
  r2

-- | Get all the users's keys to be able to provide them to each user
-- post-condition.
getUserKeys :: Array KeyWallet -> SnetContract (Array ByteArray)
getUserKeys wallets = traverse (\k -> withKeyWallet k getKey) wallets
  where
  getKey :: SnetContract ByteArray
  getKey =
    (liftAff <<< hashPkh) =<<
      (lift $ liftedM "getUserKeys: Cannot get user's pkh" ownPaymentPubKeyHash)

-- | Query the pool and construct the state of it
getMachineState :: SnetContract MachineState
getMachineState = do
  -- Get list entries
  { unbondedPoolParams, scriptVersion } <- ask
  entries <- lift $ queryAssocListUnbonded unbondedPoolParams scriptVersion
  -- Get pool state
  maybePoolState <- lift $ queryStateUnbonded unbondedPoolParams scriptVersion
  -- Get all asset utxos
  { stakedAsset } <- lift $ queryAssetsUnbonded unbondedPoolParams scriptVersion
  pure
    { entries
    , totalFakegix: stakedAsset
    , poolOpen: maybe false _.open maybePoolState
    }

-- | Generate the inputs from their configuration
genInputs :: InputConfig -> SnetContract StateMachineInputs
genInputs = liftEffect <<< randomSampleOne <<< arbitraryInputs

-- | Add the user wallets and keys to the user's commands
addWalletsAndKeys
  :: (KeyWallet /\ ByteArray /\ Array UserCommand')
  -> Array (EnrichedUserCommand (wallet :: KeyWallet, key :: ByteArray))
addWalletsAndKeys (kw /\ ba /\ commands) =
  map (\{ command, result } -> { command, result, key: ba, wallet: kw })
    commands

-- | Map from each `UserCommand` to the matching contract
userTransition :: UserCommand -> KeyWallet -> SnetContract Unit
userTransition command wallet = case command of
  UserStake amt -> User.stake wallet amt
  UserWithdraw -> User.withdraw wallet
  DoNothing -> pure unit

-- | Map from each `AdminCommand` to the matching contract
adminTransition :: AdminCommand -> KeyWallet -> SnetContract Unit
adminTransition command wallet = case command of
  AdminDeposit amt -> Admin.deposit wallet amt
  AdminClose -> Admin.close wallet

-- | Map from each `UserCommand` to the matching post-conditions
-- FIXME
userChecks
  :: UserCommand
  -> CommandResult
  -> ByteArray
  -> MachineState
  -> MachineState
  -> Maybe IntegrationFailure
userChecks _ _ _ _ _ = Nothing

-- | Map from each `AdminCommand` to the matching post-conditions
-- FIXME
adminChecks
  :: AdminCommand
  -> CommandResult
  -> MachineState
  -> MachineState
  -> Maybe IntegrationFailure
adminChecks _ _ _ _ = Nothing

