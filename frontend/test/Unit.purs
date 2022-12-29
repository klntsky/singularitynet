module SNet.Test.Unit (main, unitTests) where

import Contract.Prelude

import Contract.Config (emptyHooks)
import Contract.Monad (Contract, launchAff_)
import Contract.Test.Mote (interpretWithConfig)
import Contract.Test.Plutip (PlutipTest, PlutipConfig, testPlutipContracts)
import Data.Time.Duration (Seconds(..), fromDuration)
import Data.UInt as UInt
import Mote (MoteT, group, skip, test)
import Options.Applicative
  ( Parser
  , execParser
  , fullDesc
  , header
  , help
  , helper
  , info
  , long
  , progDesc
  , switch
  , (<**>)
  )
import SNet.Test.Common (testInitialParams, testInitialParamsNoTimeChecks)
import SNet.Test.Unit.Admin.Close as Close
import SNet.Test.Unit.Admin.Deposit1User as Deposit1User
import SNet.Test.Unit.Admin.DepositEmpty as DepositEmpty
import SNet.Test.Unit.Admin.DepositNUser as DepositNUser
import SNet.Test.Unit.Admin.Open as Open
import Test.Spec.Runner (Config)
import Test.Unit.User.Stake as Stake
import UnbondedStaking.Types (SnetInitialParams)

unitTests :: Contract () SnetInitialParams -> MoteT Aff PlutipTest Aff Unit
unitTests initParams =
  group "Unit Tests" do
    group "Admin" do
      test "Open/Create Pool" $ Open.test initParams
      test "Close Pool" $ Close.test initParams
      -- We  this until we decide if it's a good idea to fail when
      -- there are no stakers in the pool
      skip $ test "Deposit to empty pool" $ DepositEmpty.test initParams
      -- TODO: Fix deposit and close failing when batching is used in tests
      -- with timechecks activated.
      test "Deposit to pool with 1 user's stake" $ Deposit1User.test initParams
      ( let
          n = 10
          b = 5
        in
          test
            ( "Deposit to pool with " <> show n
                <> " users' stake (batch size = "
                <> show b
                <> ")"
            ) $
            DepositNUser.test initParams n b
      )
      ( let
          n = 10
          b = 5
        in
          test
            ( "Close pool with " <> show n <> " users' stake (batch size = "
                <> show b
                <> ")"
            ) $
            DepositNUser.test initParams n b
      )
    group "User" do
      test "Stake" $ Stake.test initParams

suite :: Boolean -> Effect Unit
suite timechecksOff
  | timechecksOff =
      launchAff_ $ interpretWithConfig testConfigLongTimeout
        $ testPlutipContracts localPlutipCfg
        $ group "Debug - No time checks"
        $ unitTests testInitialParamsNoTimeChecks
  | otherwise =
      launchAff_ $ interpretWithConfig testConfig
        $ testPlutipContracts localPlutipCfgLongSlots
        $ group "Debug"
        $ unitTests testInitialParams

main :: Effect Unit
main = do
  timechecksOff <- parse
  log $ "Timechecks " <> if timechecksOff then "OFF" else "ON"
  suite timechecksOff

timechecksParser :: Parser Boolean
timechecksParser = switch
  $ long "without-timechecks"
  <> help "Deactivate timechecks (useful for quicker testing)"

parse :: Effect Boolean
parse = execParser $ info (timechecksParser <**> helper)
  $ fullDesc
  <> progDesc "Runs unit test-suite"
  <> header "SingularityNet Unit tests"

testConfig :: Config
testConfig =
  { slow: wrap 90.0
  , timeout: Just $ fromDuration $ Seconds 50.0
  , exit: true
  }

testConfigLongTimeout :: Config
testConfigLongTimeout =
  { slow: wrap 400.0
  , timeout: Just $ fromDuration $ Seconds 300.0
  , exit: true
  }

localPlutipCfg :: PlutipConfig
localPlutipCfg =
  { host: "127.0.0.1"
  , port: UInt.fromInt 8082
  , logLevel: Trace
  , clusterConfig: { slotLength: wrap 0.1 }
  -- Server configs are used to deploy the corresponding services. 
  , ogmiosConfig:
      { port: UInt.fromInt 1338
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , kupoConfig:
      { port: UInt.fromInt 1443
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , ogmiosDatumCacheConfig:
      { port: UInt.fromInt 10000
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , postgresConfig:
      { host: "127.0.0.1"
      , port: UInt.fromInt 5433
      , user: "ctxlib"
      , password: "ctxlib"
      , dbname: "ctxlib"
      }
  , suppressLogs: true
  , customLogger: Nothing
  , hooks: emptyHooks
  }

localPlutipCfgLongSlots :: PlutipConfig
localPlutipCfgLongSlots = localPlutipCfg
  { clusterConfig = { slotLength: wrap 0.1 } }
