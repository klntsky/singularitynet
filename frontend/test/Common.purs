module Test.Common(
       fakegixTokenName,
       testInitialParams,
       testInitialParamsNoTimeChecks,
       withWalletsAndPool,
       waitFor,
       waitForNext,
       getAdminWallet,
       getUserWallet,
       getWalletFakegix,
       getPoolFakegix) where

import Prelude

import Contract.Address (PaymentPubKeyHash, ownPaymentPubKeyHash, scriptHashAddress)
import Contract.Log (logDebug')
import Contract.Monad (Contract, liftedE, liftedM, throwContractError)
import Contract.Numeric.Rational ((%))
import Contract.Prelude (mconcat)
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.ScriptLookups (ScriptLookups, mintingPolicy, mkUnbalancedTx, unspentOutputs)
import Contract.Scripts (MintingPolicy, validatorHash)
import Contract.Test.Plutip (PlutipTest, InitialUTxOs, withKeyWallet, withWallets)
import Contract.Transaction (awaitTxConfirmed, balanceTx, signTransaction, submit)
import Contract.TxConstraints (TxConstraints, mustMintValue, mustPayToPubKey)
import Contract.Utxos (UtxoMap, getWalletUtxos, utxosAt)
import Contract.Value (CurrencySymbol, TokenName, Value, mkTokenName, scriptCurrencySymbol, singleton, valueOf)
import Contract.Wallet (KeyWallet)
import Control.Monad.Error.Class (liftMaybe)
import Data.Array as Array
import Data.Bifunctor (rmap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Foldable (foldMap, sum)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.Traversable (traverse)
import Data.Tuple (fst, snd, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (delay)
import Effect.Aff.Class (liftAff)
import Effect.Exception as Exception
import Scripts.PoolValidator (mkUnbondedPoolValidator)
import Tests.Scripts.AlwaysSucceedsMp (mkTrivialPolicy)
import Types (AssetClass(..), ScriptVersion(..))
import UnbondedStaking.CreatePool (createUnbondedPoolContract)
import UnbondedStaking.Types (InitialUnbondedParams(..), Period(..), UnbondedPoolParams(..))
import UnbondedStaking.Utils (getNextPeriodRange, getPeriodRange, queryStateUnbonded)
import Utils (currentRoundedTime, currentTime, nat)


type TestInitialParams = {
   initialUnbondedParams :: InitialUnbondedParams
   , scriptVersion :: ScriptVersion
}

fakegixTokenName :: Contract () TokenName
fakegixTokenName = liftMaybe (Exception.error "Could not make FAKEGIX token name") $
    mkTokenName =<< byteArrayFromAscii "FAKEGIX"

getFakegixData :: Contract () (MintingPolicy /\ CurrencySymbol /\ TokenName)
getFakegixData = do
    mp <- mkTrivialPolicy
    cs <- liftMaybe (Exception.error "Could not get CurrencySymbol from minting policy") $
             scriptCurrencySymbol mp
    tn <- fakegixTokenName
    pure $ mp /\ cs /\ tn

-- | Initial parameters for pool that starts at `currentTime` that uses FAKEGIX
-- as staking token.
testInitialParams :: Contract () TestInitialParams
testInitialParams = do
    (_ /\ fakegixCs /\ fakegixTn) <- getFakegixData
    let periodLen = BigInt.fromInt 30_000
    interest <- liftMaybe (Exception.error "testInitialParams: could not make Rational") $ 1 % 100
    start <- unwrap <$> currentRoundedTime
    pure
      {
        initialUnbondedParams: InitialUnbondedParams {
          start
          , adminLength: periodLen
          , bondingLength: periodLen
          , userLength: periodLen
          , interestLength: periodLen
          , increments: nat 1
          , interest
          , minStake: nat 1_000
          , maxStake: nat 1_000_000
          , unbondedAssetClass: AssetClass { currencySymbol: fakegixCs, tokenName: fakegixTn }
        }
        , scriptVersion: Production
      }

testInitialParamsNoTimeChecks :: Contract () TestInitialParams
testInitialParamsNoTimeChecks = do
    initParams <- testInitialParams
    pure $ initParams { scriptVersion = DebugNoTimeChecks }

adminInitialUtxos :: InitialUTxOs /\ BigInt
adminInitialUtxos =  (BigInt.fromInt <$> [ 10_000_000, 200_000_000]) /\ BigInt.fromInt 1_000_000

-- Wait for the given period in the *next* cycle to start.
waitForNext :: Period -> UnbondedPoolParams -> ScriptVersion -> Contract () Unit
waitForNext period params sv = waitFor' period params sv true

-- Wait for the given period to start.
waitFor :: Period -> UnbondedPoolParams -> ScriptVersion -> Contract () Unit
waitFor period params sv = waitFor' period params sv false

-- Wait for a given period to start. It can either wait for the period in the
-- current cycle of the next one.
waitFor' :: Period -> UnbondedPoolParams -> ScriptVersion -> Boolean -> Contract () Unit
waitFor' _ _ DebugNoTimeChecks _ = pure unit
waitFor' period params@(UnbondedPoolParams ubp) Production skipCurrent = do
    -- Get current time and add a cycle length if skipping the current period
    -- is desired.
    let cycleLength :: BigInt
        cycleLength = ubp.userLength + ubp.adminLength + ubp.bondingLength
    currTime <- (\t -> if skipCurrent then t + cycleLength else t) <<< unwrap <$> currentRoundedTime
    { open: isOpen } <- queryStateUnbonded params Production
    when (period /= ClosedPeriod && not isOpen) $
       throwContractError "Waiting for period that will not come because pool is already closed"
    case period of
        UserPeriod    -> waitFor'' currTime ubp.start cycleLength zero ubp.userLength
        AdminPeriod   -> waitFor'' currTime ubp.start cycleLength ubp.userLength (ubp.userLength + ubp.adminLength)
        BondingPeriod -> waitFor'' currTime ubp.start cycleLength (ubp.userLength + ubp.adminLength) (ubp.userLength + ubp.adminLength + ubp.bondingLength)
        ClosedPeriod  -> waitUntilClosed params Production

-- Wait for a given period to start. The period is defined with its start and
-- end offsets.
waitFor'' :: BigInt -> BigInt -> BigInt -> BigInt -> BigInt -> Contract () Unit
waitFor'' time baseOffset cycleLength startOffset endOffset =
    let (start /\ end) =
           getPeriodRange
              time
              baseOffset
              cycleLength
              startOffset
              endOffset
        (start' /\ _) =
            getNextPeriodRange
              time
              baseOffset
              cycleLength
              startOffset
              endOffset
    in case time of
        t | t >= start && t <= end -> waitUntil start
          | t < start' -> waitUntil start'
          | otherwise ->
              throwContractError
                "Something went wrong when obtaining the ranges"

waitUntil :: BigInt -> Contract () Unit
waitUntil limit = do
    t <- unwrap <$> currentTime
    if t < limit
        then (liftAff $ delay $ wrap 20_000.0) *> waitUntil limit
        else pure unit

waitUntilClosed :: UnbondedPoolParams -> ScriptVersion -> Contract () Unit
waitUntilClosed ubp sv = do
    isOpen <- _.open <$> queryStateUnbonded ubp sv
    if isOpen
        then pure unit
        else (liftAff $ delay $ wrap 20_000.0) *> waitUntilClosed ubp sv

-- | Helper for running contracts that interact with a pool created from the
-- given `InitialUnbondedParams`. The admin wallet is also preprended to the
-- passed wallets.
withWalletsAndPool ::
   Contract () TestInitialParams ->
   Array (InitialUTxOs /\ BigInt) ->
   (Array KeyWallet -> UnbondedPoolParams -> Contract () Unit) ->
   PlutipTest
withWalletsAndPool initParamsContract distr contract =
    withWallets (fst <$> Array.cons adminInitialUtxos distr) \wallets -> do
        adminW <- getAdminWallet wallets
        userPkhs <- dropMaybe <$> (traverse <<< traverse) getUserPkh (Array.tail wallets)
        -- Mint and distribute FAKEGIX
        withKeyWallet adminW $
           mintAndDistributeFakegix (snd adminInitialUtxos) $ Array.zip userPkhs $ snd <$> distr
        logDebug' "FAKEGIX distributed succesfully!"
        -- Create pool
        { initialUnbondedParams, scriptVersion } <- initParamsContract
        { unbondedPoolParams, address } <- withKeyWallet adminW $
           createUnbondedPoolContract initialUnbondedParams scriptVersion
        logDebug' "Pool created succesfully!"
        logDebug' $ "Pool parameters: " <> show unbondedPoolParams
        logDebug' $ "Pool address: " <> show address
        contract wallets unbondedPoolParams
    where getUserPkh :: KeyWallet -> Contract () PaymentPubKeyHash
          getUserPkh w =
              withKeyWallet w <<<
              liftedM "(getUserPkh) Could not user's PKH" $
              ownPaymentPubKeyHash
          dropMaybe :: forall a . Maybe (Array a) -> Array a
          dropMaybe (Just arr) = arr
          dropMaybe Nothing = []

-- | Obtains the admin wallet.
getAdminWallet :: Array KeyWallet -> Contract () KeyWallet
getAdminWallet = liftMaybe (Exception.error "Could not get admin wallet") <<< Array.head

-- | Obtain the wallet given by index
getUserWallet :: Int -> Array KeyWallet -> Contract () KeyWallet
getUserWallet idx ws = liftMaybe (Exception.error $ "Could not get user wallet " <> show idx)
   <<< Array.index ws $ idx + 1

-- | Obtain the total amount of FAKEGIX in the wallet
getWalletFakegix :: Contract () BigInt
getWalletFakegix = do
   utxosFakegix <=< liftedM "(getWalletFakegix) Could not get wallet utxos" $ getWalletUtxos

-- | Obtain the total amount of FAKEGIX in the pool
getPoolFakegix :: UnbondedPoolParams -> ScriptVersion -> Contract () BigInt
getPoolFakegix ubp scriptVersion = do
    validator <- liftedE $ mkUnbondedPoolValidator ubp scriptVersion
    let valHash = validatorHash validator
        poolAddr = scriptHashAddress valHash Nothing
    utxosFakegix <=< liftedM "depositUnbondedPoolContract: Cannot get pool's utxos at pool address"
        $ utxosAt poolAddr

-- | Mint the necessay FAKEGIX and distribute it to admin and users
mintAndDistributeFakegix :: BigInt -> Array (PaymentPubKeyHash /\ BigInt) -> Contract () Unit
mintAndDistributeFakegix adminFakegix users = do
    adminUtxos <- liftedM "Could not get admin's utxos" $ getWalletUtxos
    (mp /\ cs /\ tn) <- getFakegixData
    let valueToMint :: Value
        valueToMint = singleton cs tn $ adminFakegix + sum (snd <$> users)
        constraints :: TxConstraints Void Void
        constraints = mconcat
            [
               mustMintValue valueToMint,
               foldMap (uncurry mustPayToPubKey <<< rmap (singleton cs tn)) users
            ]
        lookups :: ScriptLookups Void
        lookups = mconcat
            [
               unspentOutputs adminUtxos,
               mintingPolicy mp
            ]
    ubTx <- liftedE $ mkUnbalancedTx lookups constraints
    tx <- signTransaction =<< liftedE (balanceTx ubTx)
    txId <- submit tx
    awaitTxConfirmed txId


utxosFakegix :: UtxoMap -> Contract () BigInt
utxosFakegix utxos = do
   (_ /\ cs /\ tn) <- getFakegixData
   pure <<< valueOf' cs tn <<< foldMap (_.amount <<< unwrap <<< _.output <<< unwrap) $ utxos
   where valueOf' cs tn v = valueOf v cs tn
