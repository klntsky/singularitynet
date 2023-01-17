module SNet.Test.Integration (main, runMachine') where

import Contract.Prelude

import Contract.Address (ownPaymentPubKeyHash)
import Contract.Monad (Contract, launchAff_, liftedM, throwContractError)
import Contract.Prim.ByteArray (ByteArray)
import Contract.Test.Mote (interpretWithConfig)
import Contract.Test.Plutip (PlutipTest, testPlutipContracts)
import Contract.Wallet (KeyWallet)
import Control.Monad.Error.Class (liftMaybe)
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
import SNet.Test.Integration.Arbitrary (arbitraryInputs)
import SNet.Test.Integration.Types
  ( AdminCommand(..)
  , InputConfig(InputConfig)
  , IntegrationFailure
  , MachineState
  , StateMachineInputs(StateMachineInputs)
  , UserCommand(..)
  )
import SNet.Test.Integration.User as Admin
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

fixedInputs :: StateMachineInputs
fixedInputs = StateMachineInputs
  { userInputs: [ [ [ UserStake $ BigInt.fromInt 1000 ] ] ]
  , adminInput: [ AdminDeposit $ BigInt.fromInt 1000 ]
  }

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
    (StateMachineInputs { userInputs, adminInput }) <-
      either pure genInputs eitherInputsConfig
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

userTransition :: UserCommand -> KeyWallet -> SnetContract Unit
userTransition command wallet = case command of
  UserStake amt -> User.stake wallet amt
  UserWithdraw -> pure unit
  DoNothing -> pure unit

adminTransition :: AdminCommand -> KeyWallet -> SnetContract Unit
adminTransition command wallet = case command of
  AdminDeposit amt -> Admin.stake wallet amt
  AdminClose -> pure unit

userChecks
  :: UserCommand
  -> ByteArray
  -> MachineState
  -> MachineState
  -> Maybe IntegrationFailure
userChecks _ _ _ _ = Nothing

adminChecks
  :: AdminCommand -> MachineState -> MachineState -> Maybe IntegrationFailure
adminChecks _ _ _ = Nothing

