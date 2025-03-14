module SNet.Test.Unit (main, unitTests) where

import Contract.Prelude
import Contract.Monad (Contract, launchAff_)
import Contract.Test.Mote (interpretWithConfig)
import Contract.Test.Plutip (PlutipTest, testPlutipContracts)
import Mote (MoteT, group, test)
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
import SNet.Test.Common
  ( localPlutipCfg
  , localPlutipCfgLongSlots
  , testConfig
  , testConfigLongTimeout
  , testInitialParams
  , testInitialParamsNoTimeChecks
  )
import SNet.Test.Unit.Admin.Close as Close
import SNet.Test.Unit.Admin.CloseNUser as CloseNUser
import SNet.Test.Unit.Admin.CompleteDeposit as CompleteDeposit
import SNet.Test.Unit.Admin.CompleteClose as CompleteClose
import SNet.Test.Unit.Admin.Deposit1User as Deposit1User
import SNet.Test.Unit.Admin.DepositEmpty as DepositEmpty
import SNet.Test.Unit.Admin.DepositNUser as DepositNUser
import SNet.Test.Unit.Admin.Open as Open
import SNet.Test.Unit.Admin.AdminWithdraw as AdminWithdraw
import Test.Unit.User.Stake as Stake
import Test.Unit.User.StakeAndWithdraw as StakeAndWithdraw
import Test.Unit.User.StakeWaitAndWithdraw as StakeWaitAndWithdraw
import Test.Unit.User.WithdrawFromClosed as WithdrawFromClosed
import UnbondedStaking.Types (SnetInitialParams)

unitTests :: Contract () SnetInitialParams -> MoteT Aff PlutipTest Aff Unit
unitTests initParams =
  group "Unit Tests" do
    group "Admin" do
      test "Open/Create Pool" $ Open.test initParams
      test "Close Pool" $ Close.test initParams
      test "Deposit to empty pool" $ DepositEmpty.test initParams
      test "Deposit to pool with 1 user's stake" $ Deposit1User.test
        initParams
      test "Withdraw funds for 1 user" $ AdminWithdraw.test initParams
      ( let
          n = 10
          b = 5
        in
          test
            ( "Deposit to pool with " <> show n
                <> " users' stake (batch size = "
                <> show b
                <> ")"
            )
            $
              DepositNUser.test initParams n b
      )
      ( let
          n = 5
          b = 3
        in
          test
            ( "Close pool with " <> show n <> " users' stake (batch size = "
                <> show b
                <> ")"
            )
            $
              CloseNUser.test initParams n b
      )
      ( let
          n = 5
        in
          test
            ( "Deposit to pool with " <> show n <>
                " users' stake using completeDeposit"
            )
            $
              CompleteDeposit.test initParams n
      )
      ( let
          n = 5
        in
          test
            ( "Close pool with " <> show n <>
                " users' stake using closeDeposit"
            )
            $
              CompleteClose.test initParams n
      )
    group "User" do
      test "Stake" $ Stake.test initParams
      test "Stake and withdraw" $ StakeAndWithdraw.test initParams
      ( let
          n = 3
        in
          test ("Stake and withdraw after " <> show n <> " cycles")
            $ StakeWaitAndWithdraw.test n initParams
      )
      test "Withdraw from a closed pool" $ WithdrawFromClosed.test
        initParams

suite :: Boolean -> Effect Unit
suite timechecksOff
  | timechecksOff =
      launchAff_ $ interpretWithConfig testConfig
        $ testPlutipContracts localPlutipCfg
        $ group "Debug - No time checks"
        $ unitTests testInitialParamsNoTimeChecks
  | otherwise =
      launchAff_ $ interpretWithConfig testConfigLongTimeout
        $ testPlutipContracts localPlutipCfgLongSlots
        $ group "Debug"
        $ unitTests testInitialParams

main :: Effect Unit
main = do
  timechecksOn <- parse
  log $ "Timechecks " <> if timechecksOn then "ON" else "OFF"
  suite (not timechecksOn)

-- | By default we don't use timechecks (for CI use)
timechecksParser :: Parser Boolean
timechecksParser = switch
  $ long "with-timechecks"
  <> help "Activate timechecks"

parse :: Effect Boolean
parse = execParser $ info (timechecksParser <**> helper)
  $ fullDesc
  <> progDesc "Runs unit test-suite"
  <> header "SingularityNet Unit tests"

