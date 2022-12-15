module Test.Main (main) where

import Contract.Prelude

import Contract.Config (emptyHooks)
import Contract.Monad (launchAff_)
import Contract.Test.Plutip (PlutipConfig, testPlutipContracts)
import Ctl.Internal.Test.TestPlanM (TestPlanM, interpret)
import Data.UInt as UInt
import Mote (group, skip, test)
import Test.Unit.Admin.Close as Close
import Test.Unit.Admin.DepositEmpty as DepositEmpty
import Test.Unit.Admin.Deposit1User as Deposit1User
import Test.Unit.Admin.Open as Open

suite :: TestPlanM (Aff Unit) Unit
suite = testPlutipContracts localPlutipCfg do
    group "Unit Tests" do
        group "Admin" do
            test "Open/Create Pool" Open.test
            test "Close Pool" Close.test
            -- We skip this until we decide if it's a good idea to fail when
            -- there are no stakers in the pool
            skip $ test "Deposit to empty pool" DepositEmpty.test
            test "Deposit to pool with 1 user's stake" Deposit1User.test

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

