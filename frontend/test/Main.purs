module Test.Main (main) where

import Contract.Prelude

import Contract.Config (emptyHooks)
import Contract.Monad (Contract, launchAff_)
import Contract.Test.Plutip (PlutipConfig, PlutipTest, testPlutipContracts)
import Ctl.Internal.Test.TestPlanM (TestPlanM, interpret)
import Data.UInt as UInt
import Mote (MoteT, group, skip, test)
import Test.Common (testInitialParams, testInitialParamsNoTimeChecks)
import Test.Unit.Admin.Close as Close
import Test.Unit.Admin.Deposit1User as Deposit1User
import Test.Unit.Admin.DepositEmpty as DepositEmpty
import Test.Unit.Admin.DepositNUser as DepositNUser
import Test.Unit.Admin.Open as Open
import Test.Unit.User.Stake as Stake
import UnbondedStaking.Types (SnetInitialParams)

suite :: TestPlanM (Aff Unit) Unit
suite = testPlutipContracts localPlutipCfg do
    group "Debug - No time checks" $ allTests testInitialParamsNoTimeChecks
    group "Debug" $ allTests testInitialParams

allTests :: Contract () SnetInitialParams -> MoteT Aff PlutipTest Aff Unit
allTests initParams =
    group "Unit Tests" do
        group "Admin" do
            test "Open/Create Pool" $ Open.test initParams
            test "Close Pool" $ Close.test initParams
            -- We skip this until we decide if it's a good idea to fail when
            -- there are no stakers in the pool
            skip $ test "Deposit to empty pool" $ DepositEmpty.test initParams
            test "Deposit to pool with 1 user's stake" $ Deposit1User.test initParams
            -- We skip this one until batching behaviour is fixed. Only the
            -- first batch succeeds.
            (let n = 5
                 b = 2
             in skip $ test ("Deposit to pool with " <> show n <> " users' stake") $ DepositNUser.test initParams n b)
            -- We skip this one until batching behaviour is fixed. Only the
            -- first batch succeeds.
            (let n = 5
                 b = 3
             in skip $ test ("Close pool with " <> show n <> " users' stake") $ DepositNUser.test initParams n b)
        group "User" do
            test "Stake" $ Stake.test initParams

main :: Effect Unit
main = launchAff_ $ interpret suite

localPlutipCfg :: PlutipConfig
localPlutipCfg =
  { host: "127.0.0.1" 
   , port: UInt.fromInt 8082 
   , logLevel: Trace 
   -- Server configs are used to deploy the corresponding services. 
   , ogmiosConfig: 
       { port: UInt.fromInt 1338 
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
   , ctlServerConfig: Just 
       { port: UInt.fromInt 8083 
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

