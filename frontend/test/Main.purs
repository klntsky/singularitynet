module Test.Main (main) where

import Contract.Prelude

import Contract.Address (PaymentPubKeyHash(..), getWalletAddress, toPubKeyHash)
import Contract.Config (emptyHooks)
import Contract.Log (logInfo')
import Contract.Monad (launchAff_, liftedE, liftedM)
import Contract.ScriptLookups (ScriptLookups, unspentOutputs, mkUnbalancedTx)
import Contract.Test.Plutip
  ( PlutipConfig
  , testPlutipContracts
  , withKeyWallet
  , withWallets
  )
import Contract.Transaction
  ( awaitTxConfirmed
  , balanceTx
  , signTransaction
  , submit
  )
import Contract.TxConstraints (mustPayToPubKey)
import Contract.Utxos (getWalletUtxos)
import Contract.Value (lovelaceValueOf)
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Internal.Test.TestPlanM (TestPlanM, interpret)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.UInt as UInt
import Effect.Exception (error)
import Mote (test)

plutipCfg :: PlutipConfig
plutipCfg =
  { host: "127.0.0.1"
  , port: UInt.fromInt 8082
  , logLevel: Trace
  , clusterConfig: { slotLength: wrap 1.0 }
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

suite :: TestPlanM (Aff Unit) Unit
suite = testPlutipContracts plutipCfg do
  test "Plutip setup test" do
    let
      distribution :: (Array BigInt /\ Array BigInt)
      distribution = [ BigInt.fromInt 5000000, BigInt.fromInt 500000000 ] /\ []
    withWallets distribution \(aliceWallet /\ bobWallet) -> do
      bobAddr <- withKeyWallet bobWallet $ liftedM "Could not get Bob's address"
        $
          getWalletAddress
      withKeyWallet aliceWallet do
        bobPkh <- PaymentPubKeyHash <$>
          liftMaybe (error "Could not get Bob's PKH") (toPubKeyHash bobAddr)
        aliceUtxos <- liftedM "Could not get Alice's utxos" getWalletUtxos
        let
          constraints = mustPayToPubKey bobPkh
            (lovelaceValueOf $ BigInt.fromInt 10000000)

          lookups :: ScriptLookups Void
          lookups = unspentOutputs aliceUtxos
        ubTx <- liftedE $ mkUnbalancedTx lookups constraints
        bTx <- liftedE $ balanceTx ubTx
        signedTx <- signTransaction bTx
        txId <- submit signedTx
        logInfo' $ "TX ID: " <> show txId
        awaitTxConfirmed txId

main :: Effect Unit
main = launchAff_ $ interpret suite
