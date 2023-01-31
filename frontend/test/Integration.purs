module SNet.Test.Integration (main, runMachine') where

import Contract.Prelude

import Contract.Address (ownPaymentPubKeyHash)
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, liftedM, throwContractError)
import Contract.Numeric.Rational (Rational)
import Contract.Prim.ByteArray (ByteArray)
import Contract.Test.Mote (interpretWithConfig)
import Contract.Test.Plutip (PlutipTest, testPlutipContracts)
import Contract.Wallet (KeyWallet)
import Control.Monad.Error.Class (liftMaybe, try)
import Control.Monad.Reader (ask, lift)
import Data.Array (zip, (..))
import Data.Array as Array
import Data.BigInt as BigInt
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Effect.Exception as Exception
import Effect.Exception.Unsafe (unsafeThrow)
import Mote (MoteT, group, test, skip)
import SNet.Test.Common
  ( localPlutipCfg
  , testConfigLongTimeout
  , testInitialParamsNoTimeChecks
  , withKeyWallet
  , withWalletsAndPool
  )
import SNet.Test.Integration.Admin as Admin
import SNet.Test.Integration.Arbitrary (arbitraryInputs)
import SNet.Test.Integration.Model (addPoolStates)
import SNet.Test.Integration.Types
  ( AdminCommand(..)
  , Array3
  , CommandResult(..)
  , EnrichedUserCommand
  , InputConfig(InputConfig)
  , IntegrationFailure(..)
  , PoolState
  , PoolState'
  , StateMachineInputs(StateMachineInputs)
  , UserCommand(..)
  , UserCommand'
  , StateMachineOnlyInputs
  , prettyInputs
  )
import SNet.Test.Integration.User as User
import Test.QuickCheck.Gen (randomSampleOne)
import UnbondedStaking.Types
  ( Entry(..)
  , SnetContract
  , SnetInitialParams
  , UnbondedPoolParams
  )
import UnbondedStaking.Utils
  ( queryAssocListUnbonded
  , queryStateUnbonded
  , queryAssetsUnbonded
  )
import Utils (hashPkh, logInfo_, toRational)

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
main = launchAff_ $ interpretWithConfig testConfigLongTimeout $
  testPlutipContracts
    localPlutipCfg
    suite

suite :: MoteT Aff PlutipTest Aff Unit
suite =
  group "Integration tests" do
    test "Two stakes in a row" $
      runMachineWith testInitialParamsNoTimeChecks twoStakesInARow
    test "Rounding" $
      runMachineWith testInitialParamsNoTimeChecks rounding
    group "Random" $ runMachine testInitialParamsNoTimeChecks inputCfg 10

-- | A test for validating stake updates
twoStakesInARow :: StateMachineOnlyInputs
twoStakesInARow =
  { usersInputs:
      [ [ [ { command: UserStake $ BigInt.fromInt 1000, result: Success } ]
        , [ { command: UserStake $ BigInt.fromInt 1100, result: Success }
          , { command: UserStake $ BigInt.fromInt 1200, result: Success }
          ]
        ]
      ]
  , adminInputs:
      [ { command: AdminClose, result: Success } ]
  }

-- | A test for validating rounding behaviour
rounding :: StateMachineOnlyInputs
rounding =
  { usersInputs:
      [ [ [ { command: UserStake $ BigInt.fromInt 1088, result: Success } ]
        , [ { command: UserStake $ BigInt.fromInt 1486, result: Success } ]
        ]
      , [ [ { command: UserStake $ BigInt.fromInt 1274, result: Success } ]
        , [ { command: UserStake $ BigInt.fromInt 1208, result: Success } ]
        ]

      ]
  , adminInputs:
      [ { command: AdminDeposit $ BigInt.fromInt 1138, result: Success }
      , { command: AdminDeposit $ BigInt.fromInt 1435, result: Success }
      ]
  }

runMachineWith
  :: Contract () SnetInitialParams
  -> StateMachineOnlyInputs
  -> PlutipTest
runMachineWith initParams inputs =
  runMachine'
    initParams
    (Left inputs)
    adminTransition
    userTransition
    postConditions

runMachine
  ::
     -- | Initial state of the machine
     Contract () SnetInitialParams
  -- | Inputs (randomly generated)
  -> InputConfig
  -- | Repetitions
  -> Int
  -> MoteT Aff PlutipTest Aff Unit
runMachine initParams inputConfig n =
  for_ (1 .. n) $ \i ->
    test ("Run #" <> show i) $ runMachine'
      initParams
      (Right inputConfig)
      adminTransition
      userTransition
      postConditions

inputCfg :: InputConfig
inputCfg = InputConfig
  { stakeRange: 1000 /\ 2000
  , depositRange: 1000 /\ 2000
  , nUsers: 2
  , nCycles: 2
  , maxUserActionsPerCycle: 2
  }

runMachine'
  ::
     -- | Initial state of the machine
     Contract () SnetInitialParams
  -- | Either the inputs or the inputs config to generate them randomly
  -> Either StateMachineOnlyInputs InputConfig
  -- | Transitions for `AdminCommand` inputs
  -> (AdminCommand -> KeyWallet -> SnetContract Unit)
  -- | Transitions for `UserCommand` inputs
  -> (UserCommand -> KeyWallet -> SnetContract Unit)
  -- | Post-conditions after each cycle's conclusion
  -> (PoolState -> PoolState' -> Array IntegrationFailure)
  -> PlutipTest
runMachine'
  initialParams
  eitherInputsConfig
  transAdmin
  transUser
  postconditions = do
  let
    -- We distribute to a hardcoded number of users, even though some of them
    -- may not be used
    nUsers = 3
    distr =
      Array.replicate nUsers
        $ map BigInt.fromInt [ 10_000_000, 100_000_000 ]
        /\
          (BigInt.fromInt 1_000_000_000)
  withWalletsAndPool initialParams distr \wallets -> do
    -- Get inputs and related information
    { unbondedPoolParams: ubp } <- ask
    inputs@
      ( StateMachineInputs
          { usersInputs, adminInputs: adminCommandsPerCycle, poolStates }
      ) <- genInputs ubp eitherInputsConfig
    let
      nCycles :: Int
      nCycles = Array.length adminCommandsPerCycle
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
          (\usersInputs' -> zip usersWallets (zip usersKeys usersInputs'))
          usersInputs
    -- For each cycle, execute users and admin's actions, validate results and
    -- evaluate post-conditions
    for_
      ( zip (0 .. (nCycles - 1)) $ zip usersCommandsPerCycle $
          zip adminCommandsPerCycle poolStates
      )
      \(cycle /\ usersCommands /\ adminCommand /\ expectedState) ->
        do
          logInfo' "Context (inputs)"
          logInfo' $ prettyInputs inputs
          -- Execute all commands for each user and validate the results with the
          -- expected ones.
          for_ usersCommands \userCommands ->
            for userCommands \{ command, wallet, result: expectedResult } -> do
              executionResult <- toResult $ transUser command wallet
              validateResult command cycle expectedResult executionResult
          -- Execute admin command and validate result
          let { command, result: expectedResult } = adminCommand
          executionResult <- toResult $ transAdmin command adminWallet
          _ <- validateResult command cycle expectedResult executionResult
          actualState <- getMachineState usersKeys
          -- Evaluate post-conditions and report them
          let badTransitions = postconditions expectedState actualState
          when (Array.length badTransitions > 0)
            $ lift
            $ throwContractError
            $ show badTransitions

-- | Converts a `SnetContract Unit` into a `SnetContract CommandResult` by
-- wrapping the contract with a `try` and capturing the error.
toResult :: SnetContract Unit -> SnetContract CommandResult
toResult = map (either (ExecutionFailure <<< Just) $ const Success) <<< try

-- | Compares the command execution's result with the expected result.
-- If they match, it returns the executtion's result. Otherwise, it throws
-- an integration error.
validateResult
  :: forall a
   . Show a
  =>
  -- | ^ Command type
  a
  -- | ^ Cycle in which the command was executed
  -> Int
  -- | ^ Expected result
  -> CommandResult
  ->
  -- | ^ Execution result
  CommandResult
  -> SnetContract CommandResult
validateResult command _ Success Success = do
  logInfo' $ "Succeeded in executing command " <> show command
  pure Success
validateResult command _ (ExecutionFailure _) e@(ExpectedFailure _) = do
  logInfo' $ "Failed succesfully in executing command " <> show command
  pure e
validateResult command _ e@(ExpectedFailure _) (ExecutionFailure _) = do
  logInfo' $ "Failed succesfully in executing command " <> show command
  pure e
validateResult command cycle r1 r2 = lift $ throwContractError $ ResultMismatch
  ( "There was a mismatch between the expected and the obtained result in command "
      <> show command
      <> ", cycle "
      <> show cycle
  )
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

-- | Query the pool and construct the state of it. It takes as input the list
-- of users' keys
getMachineState :: Array ByteArray -> SnetContract PoolState'
getMachineState keys = do
  -- Get list entries' information
  { unbondedPoolParams, scriptVersion } <- ask
  stakers <- entriesToMap <$> lift
    (queryAssocListUnbonded unbondedPoolParams scriptVersion)

  -- Get pool state
  maybePoolState <- lift $ queryStateUnbonded unbondedPoolParams scriptVersion
  lift $ logInfo' $ "maybePoolState: " <> show maybePoolState
  -- Get all asset utxos
  { stakedAsset: funds } <- lift $ queryAssetsUnbonded unbondedPoolParams
    scriptVersion
  pure
    { stakers
    , staked: sum stakers
    , funds
    , closed: maybe true (not <<< _.open) maybePoolState
    }
  where
  keyToIndex :: ByteArray -> Int
  keyToIndex ba = case Array.elemIndex ba keys of
    Just idx -> idx
    Nothing -> unsafeThrow "User key not found in keys array"

  entriesToMap :: Array Entry -> Map Int Rational
  entriesToMap =
    Map.fromFoldable
      <<< map \(Entry e) -> keyToIndex e.key /\
        (toRational e.deposited + e.rewards)

-- | Either generate the inputs from their configuration or add the missing
-- pool states
genInputs
  :: UnbondedPoolParams
  -> Either StateMachineOnlyInputs InputConfig
  -> SnetContract StateMachineInputs
genInputs ubp (Left inputs) = pure $ addPoolStates ubp inputs
genInputs ubp (Right cfg) =
  liftEffect
    $ randomSampleOne
    $ arbitraryInputs ubp cfg

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

-- | Map from each `AdminCommand` to the matching contract
adminTransition :: AdminCommand -> KeyWallet -> SnetContract Unit
adminTransition command wallet = case command of
  AdminDeposit amt -> Admin.deposit wallet amt
  AdminClose -> Admin.close wallet

-- | Validate that the pool state as predicated matches the pool state after
-- each cycle
postConditions
  :: PoolState
  -> PoolState'
  -> Array IntegrationFailure
postConditions expected actual =
  Array.catMaybes
    [ cond (Map.size expected.stakers == Map.size actual.stakers)
        "The number of users in the pool do not match"
    , cond (expected.stakers == actual.stakers)
        "Some of the users' funds do not match"
    , cond (expected.closed == actual.closed)
        "The pool open/close state does not match"
    , cond (expected.funds == actual.funds)
        "The funds in the pool do not match"
    , cond (expected.staked == actual.staked)
        "The amount of staked funds do not match"
    ]
  where
  cond :: Boolean -> String -> Maybe IntegrationFailure
  cond b msg =
    if not b then Just $ BadTransition msg expected actual else Nothing
