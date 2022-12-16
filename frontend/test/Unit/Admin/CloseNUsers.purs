module Test.Unit.Admin.CloseNUser (test) where

import Prelude

import Contract.Monad (throwContractError)
import Contract.Numeric.Natural as Natural
import Contract.Test.Plutip (PlutipTest, InitialUTxOs, withKeyWallet)
import Data.Array ((..))
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Traversable (for_)
import Data.Tuple.Nested (type (/\), (/\))
import Test.Common (getAdminWallet, getUserWallet, getWalletFakegix, testInitialParamsNoTimeChecks, withWalletsAndPool)
import Types (ScriptVersion(..))
import UnbondedStaking.ClosePool (closeUnbondedPoolContract)
import UnbondedStaking.UserStake (userStakeUnbondedPoolContract)
import Utils (nat)

scriptVersion :: ScriptVersion
scriptVersion = DebugNoTimeChecks

-- | The admin closes a pool with `n` user entries.
test :: Int -> Int -> PlutipTest
test userCount batchSize = withWalletsAndPool testInitialParamsNoTimeChecks usersInitialUtxos \wallets ubp -> do
    adminWallet <- getAdminWallet wallets
    let stakeAmtBase = BigInt.fromInt 1000
        --expectedPoolValue = sum $ map (_ * stakeAmtBase) $ BigInt.fromInt <$> 0 .. (userCount - 1)
    for_ (0 .. (userCount - 1)) \idx -> getUserWallet idx wallets >>= flip withKeyWallet do
          let stakeAmt = BigInt.fromInt (idx + 1) * stakeAmtBase
          initialFakegix <- getWalletFakegix
          _txId <- userStakeUnbondedPoolContract ubp scriptVersion $ Natural.fromBigInt' stakeAmt
          finalFakegix <- getWalletFakegix
          when (not $ finalFakegix == initialFakegix - stakeAmt) $
             throwContractError "Incorrect amount of FAKEGIX deducted from user wallet"
    withKeyWallet adminWallet do
          failedIndices <- closeUnbondedPoolContract ubp scriptVersion (nat batchSize) []
          when (not $ Array.null failedIndices) $
             throwContractError $ "Some entries failed to be closed " <> show failedIndices
    where usersInitialUtxos :: Array (InitialUTxOs /\ BigInt)
          usersInitialUtxos = Array.replicate userCount $
              map BigInt.fromInt [10_000_000, 100_000_000] /\ (BigInt.fromInt 1_000_000_000)

