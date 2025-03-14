module Utils
  ( big
  , bigIntRange
  , currentRoundedTime
  , currentTime
  , countdownTo
  , findInsertUpdateElem
  , findRemoveOtherElem
  , getAssetsToConsume
  , getUtxoWithNFT
  , hashPkh
  , jsonReader
  , logInfo_
  , mkAssetUtxosConstraints
  , mkOnchainAssocList
  , mkRatUnsafe
  , toRational
  , nat
  , roundDown
  , roundUp
  , splitByLength
  , submitTransaction
  , submitBatchesSequentially
  , toIntUnsafe
  , valueOf'
  , repeatUntilConfirmed
  , repeatUntilConfirmed'
  , mustPayToScript
  , getUtxoDatumHash
  , addressFromBech32
  ) where

import Contract.Prelude hiding (length)

import Contract.Address (Address, Bech32String, PaymentPubKeyHash, getNetworkId)
import Contract.Hashing (blake2b256Hash)
import Contract.Log (logInfo, logInfo')
import Contract.Monad
  ( Contract
  , liftContractM
  , liftedE
  , liftedM
  , tag
  , throwContractError
  )
import Contract.Numeric.Natural (Natural, fromBigInt', toBigInt)
import Contract.Numeric.Rational (Rational, denominator, numerator, (%))
import Contract.PlutusData
  ( Datum
  , DataHash
  , PlutusData
  , Redeemer
  , OutputDatum(OutputDatumHash)
  )
import Contract.Prim.ByteArray (ByteArray, hexToByteArray, byteArrayToHex)
import Contract.ScriptLookups (ScriptLookups, UnattachedUnbalancedTx)
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (PlutusScript, ValidatorHash)
import Contract.Time
  ( ChainTip(..)
  , Tip(..)
  , POSIXTime(POSIXTime)
  , getEraSummaries
  , getSystemStart
  , getTip
  , slotToPosixTime
  )
import Contract.Transaction
  ( BalancedSignedTransaction
  , TransactionInput
  , TransactionOutputWithRefScript(TransactionOutputWithRefScript)
  , awaitTxConfirmedWithTimeout
  , balanceTx
  , plutusV1Script
  , signTransaction
  , submit
  )
import Contract.TxConstraints
  ( TxConstraints
  , DatumPresence(DatumWitness)
  , mustSpendScriptOutput
  )
import Contract.TxConstraints as TxConstraints
import Contract.Utxos (UtxoMap, getWalletUtxos)
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , Value
  , adaSymbol
  , adaToken
  , flattenNonAdaAssets
  , flattenValue
  , getCurrencySymbol
  , getTokenName
  , negation
  , valueOf
  )
import Control.Alternative (guard)
import Control.Monad.Error.Class (liftMaybe, throwError, try)
import Ctl.Internal.BalanceTx.Error (BalanceTxError(InsufficientTxInputs))
import Ctl.Internal.Plutus.Conversion (toPlutusAddress, toPlutusValue)
import Ctl.Internal.Serialization.Address (addressFromBech32, addressNetworkId) as SA
import Ctl.Internal.Serialization.Hash (ed25519KeyHashToBytes)
import Data.Argonaut.Core (Json, caseJsonObject)
import Data.Argonaut.Decode.Combinators (getField) as Json
import Data.Argonaut.Decode.Error (JsonDecodeError(TypeMismatch))
import Data.Array
  ( filter
  , head
  , last
  , length
  , partition
  , mapMaybe
  , slice
  , sortBy
  , (..)
  )
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.BigInt
  ( BigInt
  , fromInt
  , fromNumber
  , quot
  , rem
  , toInt
  , toNumber
  , toString
  )
import Data.Map (Map, toUnfoldable)
import Data.Map as Map
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Seconds, Milliseconds(Milliseconds))
import Data.Unfoldable (unfoldr)
import Effect.Aff (delay)
import Effect.Exception (error, throw)
import Math (ceil)
import Prim.Row (class Lacks)
import Record (insert, delete)
import Settings (unbondedStakingTokenName)
import Types (AssetClass(AssetClass), MintingAction(MintEnd, MintInBetween))
import UnbondedStaking.Types (UnbondedPoolParams(..))

-- | Helper to decode the local inputs such as unapplied minting policy and
-- typed validator
jsonReader
  :: String
  -> Json
  -> Either JsonDecodeError PlutusScript
jsonReader field = do
  caseJsonObject (Left $ TypeMismatch "Expected Object") $ \o -> do
    hex <- Json.getField o field
    case hexToByteArray hex of
      Nothing -> Left $ TypeMismatch "Could not convert to bytes"
      Just bytes -> pure $ plutusV1Script bytes

-- | Get the UTXO with the NFT defined by its `CurrencySymbol` and `TokenName`.
-- If more than one UTXO contains the NFT, something is seriously wrong.
getUtxoWithNFT
  :: UtxoMap
  -> CurrencySymbol
  -> TokenName
  -> Maybe (Tuple TransactionInput TransactionOutputWithRefScript)
getUtxoWithNFT utxoM cs tn =
  let
    utxos = filter hasNFT $ toUnfoldable utxoM
  in
    if length utxos > 1 then Nothing
    else head utxos
  where
  hasNFT
    :: Tuple TransactionInput TransactionOutputWithRefScript
    -> Boolean
  hasNFT (Tuple _ txOutput') =
    let
      txOutput = unwrap $ (unwrap txOutput').output
    in
      valueOf txOutput.amount cs tn == one

-- | This receives a `UtxoMap` with all the asset UTxOs of the pool and the desired
-- | amount to withdraw. It returns a subset of these that sums at least
-- | the given amount and the total amount
getAssetsToConsume
  :: AssetClass -> BigInt -> UtxoMap -> Maybe (UtxoMap /\ BigInt)
getAssetsToConsume (AssetClass ac) withdrawAmt assetUtxos =
  go assetList Map.empty zero
  where
  assetList :: Array (TransactionInput /\ TransactionOutputWithRefScript)
  assetList = Map.toUnfoldable $ assetUtxos

  go
    :: Array (TransactionInput /\ TransactionOutputWithRefScript)
    -> Map TransactionInput TransactionOutputWithRefScript
    -> BigInt
    -> Maybe (UtxoMap /\ BigInt)
  go arr toConsume sum
    | sum >= withdrawAmt = Just $ toConsume /\ (sum - withdrawAmt)
    | null arr = Nothing
    | otherwise = do
        input /\ output <- Array.head arr
        arr' <- Array.tail arr
        let
          assetCount = valueOf (unwrap (unwrap output).output).amount
            ac.currencySymbol
            ac.tokenName
          toConsume' = Map.insert input output toConsume
          sum' = sum + assetCount
        go arr' toConsume' sum'

-- | Builds constraints for asset UTxOs
mkAssetUtxosConstraints :: UtxoMap -> Redeemer -> TxConstraints Unit Unit
mkAssetUtxosConstraints utxos redeemer =
  foldMap (\(input /\ _) -> mustSpendScriptOutput input redeemer)
    ( Map.toUnfoldable $ utxos
        :: Array (TransactionInput /\ TransactionOutputWithRefScript)
    )

roundUp :: Rational -> BigInt
roundUp r =
  let
    n = numerator r
    d = denominator r
  in
    if d == one then n
    else quot (n + d - (rem n d)) d

roundDown :: Rational -> BigInt
roundDown r =
  let
    n = numerator r
    d = denominator r
  in
    quot (n - (rem n d)) d

-- | Converts a `Maybe Rational` to a `Rational` when using the (%) constructor
mkRatUnsafe :: Maybe Rational -> Rational
mkRatUnsafe Nothing = zero
mkRatUnsafe (Just r) = r

-- Helper for making `Rational`s out of `BigInt`s
toRational :: BigInt -> Rational
toRational x = mkRatUnsafe $ x % one

-- | Converts from a contract 'Natural' to an 'Int'
toIntUnsafe :: Natural -> Int
toIntUnsafe = fromMaybe 0 <<< toInt <<< toBigInt

logInfo_
  :: forall (r :: Row Type) (a :: Type)
   . Show a
  => String
  -> a
  -> Contract r Unit
logInfo_ k = flip logInfo mempty <<< tag k <<< show

hashPkh :: PaymentPubKeyHash -> Aff ByteArray
hashPkh =
  pure <<< blake2b256Hash <<< unwrap <<< ed25519KeyHashToBytes <<< unwrap <<<
    unwrap

-- | Makes an on chain assoc list returning the key, input and output. We could
-- | be more stringent on checks to ensure the list is genuinely connected
-- | although on chain code should enforce this.
mkOnchainAssocList
  :: CurrencySymbol
  -> UtxoMap
  -> Array (ByteArray /\ TransactionInput /\ TransactionOutputWithRefScript)
mkOnchainAssocList assocListCs utxos =
  sortBy compareBytes $ mapMaybe getAssocListUtxos $ toUnfoldable utxos
  where
  getAssocListUtxos
    :: TransactionInput /\ TransactionOutputWithRefScript
    -> Maybe (ByteArray /\ TransactionInput /\ TransactionOutputWithRefScript)
  getAssocListUtxos utxo@(_ /\ (TransactionOutputWithRefScript txOutput)) = do
    let val = flattenNonAdaAssets (unwrap (txOutput.output)).amount
    cs /\ tn /\ amt <- head val
    guard (length val == one && cs == assocListCs && amt == one)
    pure $ getTokenName tn /\ utxo

compareBytes
  :: forall (t :: Type). ByteArray /\ t -> ByteArray /\ t -> Ordering
compareBytes (bytes /\ _) (bytes' /\ _) = compare bytes bytes'

-- | Find the assoc list element to update or insert. This can be optimised
-- | if we compare pairs and exit early of course. But we'll do this for
-- | simplicity. THIS MUST BE USED ON A SORTED LIST, i.e. with
-- | `mkOnchainAssocList`. We should probably create a type for the output.
findInsertUpdateElem
  :: Array (ByteArray /\ TransactionInput /\ TransactionOutputWithRefScript)
  -> ByteArray
  -> Maybe
       ( Maybe MintingAction
           /\
             { firstInput :: TransactionInput
             , secondInput :: Maybe TransactionInput
             }
           /\
             { firstOutput :: TransactionOutputWithRefScript
             , secondOutput :: Maybe TransactionOutputWithRefScript
             }
           /\
             { firstKey :: ByteArray
             , secondKey :: Maybe ByteArray
             }
       )
findInsertUpdateElem assocList hashedKey = do
  -- The list should findAssocElem assocList hashedKey = do be sorted so no
  -- need to resort
  let { no, yes } = partition (\t -> (fst t) > hashedKey) assocList
  bytesL /\ txInputL /\ txOutputL <- last no
  -- If we're at the last element, it must be an end stake or updating last
  -- element
  if length yes == zero then do
    -- Workout whether it's an initial deposit
    let
      mintingAction =
        if bytesL == hashedKey then Nothing
        else Just $ MintEnd txInputL
    pure
      $ mintingAction
      /\ { firstInput: txInputL, secondInput: Nothing }
      /\ { firstOutput: txOutputL, secondOutput: Nothing }
      /\ { firstKey: bytesL, secondKey: Nothing }
  -- Otherwise, it is an inbetween stake or updating the first element
  else do
    bytesH /\ txInputH /\ txOutputH <- head yes
    let
      mintingAction =
        if bytesL == hashedKey then Nothing
        else Just $ MintInBetween txInputL txInputH
    pure
      $ mintingAction
      /\ { firstInput: txInputL, secondInput: Just txInputH }
      /\ { firstOutput: txOutputL, secondOutput: Just txOutputH }
      /\ { firstKey: bytesL, secondKey: Just bytesH }

-- | Find the element to remove from the list. This only works for the
-- | in-between case, since it assumes that some entry will have a key less
-- | than the given one.
findRemoveOtherElem
  :: Array (ByteArray /\ TransactionInput /\ TransactionOutputWithRefScript)
  -> ByteArray
  -> Maybe
       ( { firstInput :: TransactionInput
         , secondInput :: TransactionInput
         }
           /\
             { firstOutput :: TransactionOutputWithRefScript
             , secondOutput :: TransactionOutputWithRefScript
             }
           /\
             { firstKey :: ByteArray
             , secondKey :: ByteArray
             }
       )
findRemoveOtherElem assocList hashedKey = do
  let { no, yes } = partition (\t -> (fst t) < hashedKey) assocList
  bytesL /\ txInputL /\ txOutputL <- last yes
  bytesH /\ txInputH /\ txOutputH <- head no
  if bytesH /= hashedKey
  -- If the first element not less than `hashedKey` is not equal, then the
  -- entry has not been found
  then Nothing
  -- Otherwise, this is the entry to remove and the last element of the
  -- entries less than `hashedKey` is the previous entry
  else Just
    $ { firstInput: txInputL, secondInput: txInputH }
    /\ { firstOutput: txOutputL, secondOutput: txOutputH }
    /\ { firstKey: bytesL, secondKey: bytesH }

-- Produce a range from zero to the given bigInt (inclusive)
bigIntRange :: BigInt -> Array BigInt
bigIntRange lim =
  unfoldr
    ( \acc ->
        if acc >= lim then Nothing
        else Just $ acc /\ (acc + one)
    )
    zero

-- Get the node's time rounded to the closest integer (ceiling) in seconds.
currentRoundedTime
  :: forall (r :: Row Type). Contract r POSIXTime
currentRoundedTime = do
  POSIXTime t <- currentTime
  t' <-
    liftMaybe (error "currentRoundedTime: could not convert Number to BigInt")
      $ fromNumber
      $ ceil (toNumber t / 1000.0)
      * 1000.0
  pure $ POSIXTime t'

-- | Get the POSIX time from the node. This is obtained by converting the current
-- | slot.
currentTime
  :: forall (r :: Row Type). Contract r POSIXTime
currentTime = do
  -- Get current slot
  ChainTip { slot } <- getTip >>= case _ of
    Tip chainTip -> pure chainTip
    TipAtGenesis -> throwContractError "currentTime: node returned TipAtGenesis"
  -- Convert slot to POSIXTime
  es <- getEraSummaries
  ss <- getSystemStart
  liftEither
    <<< lmap (error <<< show)
    <=< liftEffect
    $ slotToPosixTime es ss slot

countdownTo :: forall (r :: Row Type). POSIXTime -> Contract r Unit
countdownTo targetTime = countdownTo' Nothing
  where
  countdownTo' :: Maybe POSIXTime -> Contract r Unit
  countdownTo' prevTime = do
    currTime <- currentTime
    let
      msg :: String
      msg = "Countdown: " <> showSeconds delta

      delta :: BigInt
      delta = unwrap targetTime - unwrap currTime
    if currTime >= targetTime then logInfo' "GO"
    else if timeChanged prevTime currTime then logInfo' msg *> wait delta *>
      countdownTo' (Just currTime)
    else wait delta *> countdownTo' (Just currTime)

  wait :: BigInt -> Contract r Unit
  wait n = liftAff $ delay $
    if n > fromInt 5000 then Milliseconds 5000.0
    else Milliseconds $ toNumber n

  showSeconds :: BigInt -> String
  showSeconds n = show $ toNumber n / 1000.0

  timeChanged :: Maybe POSIXTime -> POSIXTime -> Boolean
  timeChanged prevTime currTime = maybe true (_ /= currTime) prevTime

-- | Utility function for splitting an array into equal length sub-arrays
-- | (with remainder array length <= size)
splitByLength :: forall (a :: Type). Int -> Array a -> Array (Array a)
splitByLength size array
  | size == 0 || null array = []
  | otherwise =
      let
        sublistCount =
          if (length array) `mod` size == 0 then ((length array) `div` size) - 1
          else (length array) `div` size
      in
        map (\i -> slice (i * size) ((i * size) + size) array) $
          0 .. sublistCount

-- | Submits a transaction with the given list of constraints/lookups. It
-- returns the same constraints/lookups if it fails.
submitTransaction
  :: UnbondedPoolParams
  -> TxConstraints Unit Unit
  -> ScriptLookups.ScriptLookups PlutusData
  -> Seconds
  -> Int
  -> Array
       ( Tuple
           (TxConstraints Unit Unit)
           (ScriptLookups.ScriptLookups PlutusData)
       )
  -> Contract ()
       ( Array
           ( Tuple
               (TxConstraints Unit Unit)
               (ScriptLookups.ScriptLookups PlutusData)
           )
       )
submitTransaction ubp baseConstraints baseLookups timeout maxAttempts updateList =
  do
    let
      constraintList = fst <$> updateList
      lookupList = snd <$> updateList
      constraints = baseConstraints <> mconcat constraintList
      lookups = baseLookups <> mconcat lookupList
    result <- try $ repeatUntilConfirmed ubp timeout maxAttempts do
      -- Build transaction
      ubTx <- liftedE $ ScriptLookups.mkUnbalancedTx lookups constraints
      pure { ubTx }
    case result of
      Left e -> do
        logInfo_ "submitTransaction:" e
        pure updateList
      Right _ ->
        pure []

-- Submits a series of batches to be executed sequentially, where each batch
-- is defined by a given list of constraints/lookups. Importantly, for each
-- batch the wallet's UTxOs are fetched and added to the lookups.
submitBatchesSequentially
  :: UnbondedPoolParams
  -> TxConstraints Unit Unit
  -> ScriptLookups PlutusData
  -> Seconds
  -> Int
  -> Array
       ( Array
           ( Tuple
               (TxConstraints Unit Unit)
               (ScriptLookups.ScriptLookups PlutusData)
           )
       )
  -> Contract ()
       ( Array
           ( Tuple
               (TxConstraints Unit Unit)
               (ScriptLookups.ScriptLookups PlutusData)
           )
       )
submitBatchesSequentially
  ubp
  constraints
  lookups
  confirmationTimeout
  submissionAttempts
  batches = do
  mconcat <$> traverse submitTransaction' batches
  where
  submitTransaction' batch = do
    utxos <- liftedM "submitBatchesSequentially: could not get wallet utxos" $
      getWalletUtxos
    let lookups' = lookups <> ScriptLookups.unspentOutputs utxos
    submitTransaction ubp constraints lookups' confirmationTimeout
      submissionAttempts
      batch

-- | This function executes a `Contract` that returns a `BalancedSignedTransaction`,
-- | submits it, and waits `timeout` seconds for it to succeed. If it does not,
-- | the function repeats the contract execution and TX submission until it
-- | does or `maxTrials` attempts are completed.
repeatUntilConfirmed
  :: forall (r :: Row Type) (p :: Row Type)
   . Lacks "txId" p
  => Lacks "ubTx" p
  => UnbondedPoolParams
  -> Seconds
  -> Int
  -> Contract r { ubTx :: UnattachedUnbalancedTx | p }
  -> Contract r
       { txId :: String | p }
repeatUntilConfirmed ubp timeout maxTrials contract = do
  result@{ ubTx } <- contract
  logInfo' "repeatUntilConfirmed: transaction built successfully"
  tx <- balanceAndSignTx ubp ubTx
  txHash <- submit tx
  logInfo'
    "repeatUntilConfirmed: transaction submitted. Waiting for confirmation"
  confirmation <- try $ awaitTxConfirmedWithTimeout timeout txHash
  case confirmation of
    Left _ -> do
      logInfo'
        "repeatUntilConfirmed: timeout reached, the transaction was not confirmed"
      if maxTrials == 0 then do
        logInfo'
          "repeatUntilConfirmed: no more trials remaining, throwing exception..."
        liftEffect $ throw "Failed to submit transaction"
      else do
        logInfo' $ "repeatUntilConfirmed: running transaction again, "
          <> show maxTrials
          <> " trials remaining"
        repeatUntilConfirmed ubp timeout (maxTrials - 1) contract
    Right _ -> do
      logInfo' "repeatUntilConfirmed: transaction confirmed!"
      logInfo_ "TX Hash" txHash
      pure $ insert (SProxy :: SProxy "txId") (byteArrayToHex $ unwrap txHash)
        (delete (SProxy :: SProxy "ubTx") result)

-- | A variant of `repeatUntilConfirmed` less comprehensive error support.
-- This is used by `createPool`, since by the time the pool is created, there
-- are no `UnbondedPoolParams` to provide better error messages.
repeatUntilConfirmed'
  :: forall (r :: Row Type) (p :: Row Type)
   . Lacks "txId" p
  => Lacks "ubTx" p
  => Seconds
  -> Int
  -> Contract r { ubTx :: UnattachedUnbalancedTx | p }
  -> Contract r
       { txId :: String | p }

repeatUntilConfirmed' timeout maxTrials contract = do
  result@{ ubTx } <- contract
  logInfo' "repeatUntilConfirmed: transaction built successfully"
  bTx <- liftedE $ balanceTx ubTx
  tx <- signTransaction bTx
  txHash <- submit tx
  logInfo'
    "repeatUntilConfirmed: transaction submitted. Waiting for confirmation"
  confirmation <- try $ awaitTxConfirmedWithTimeout timeout txHash
  case confirmation of
    Left _ -> do
      logInfo'
        "repeatUntilConfirmed: timeout reached, the transaction was not confirmed"
      if maxTrials == 0 then do
        logInfo'
          "repeatUntilConfirmed: no more trials remaining, throwing exception..."
        liftEffect $ throw "Failed to submit transaction"
      else do
        logInfo' $ "repeatUntilConfirmed: running transaction again, "
          <> show maxTrials
          <> " trials remaining"
        repeatUntilConfirmed' timeout (maxTrials - 1) contract
    Right _ -> do
      logInfo' "repeatUntilConfirmed: transaction confirmed!"
      logInfo_ "TX Hash" txHash
      pure $ insert (SProxy :: SProxy "txId") (byteArrayToHex $ unwrap txHash)
        (delete (SProxy :: SProxy "ubTx") result)

-- | Balances and signs TXs. Handles `BalanceInsufficientError`
-- exceptions occasioned by a very low amount of ADA by throwing a custom, more
-- user-friendly error.
balanceAndSignTx
  :: forall r
   . UnbondedPoolParams
  -> UnattachedUnbalancedTx
  -> Contract r BalancedSignedTransaction
balanceAndSignTx ubp ubTx = do
  result <- balanceTx ubTx
  bTx <- case result of
    Left e -> handleBalanceError ubp e
    Right r -> pure r
  signTransaction bTx

-- | Match on a `BalanceTxError` and output a user-friendly, domain-specific
-- error
handleBalanceError
  :: forall r a. UnbondedPoolParams -> BalanceTxError -> Contract r a
handleBalanceError ubp err = case err of
  InsufficientTxInputs expected actual ->
    throwValueDifference
      ubp
      $ toPlutusValue (unwrap expected)
          `minus` toPlutusValue (unwrap actual)
  _ -> throwContractError err
  where
  minus :: Value -> Value -> Value
  minus v1 v2 = v1 <> negation v2

throwValueDifference
  :: forall r a
   . UnbondedPoolParams
  -> Value
  -> Contract r a
throwValueDifference
  (UnbondedPoolParams ubp)
  diff =
  do
    let
      AssetClass { currencySymbol: agixCs, tokenName: agixTn } =
        ubp.unbondedAssetClass

      -- `flattenValue` ignores the non-positive parts and this is exactly what
      -- we want, since negative and zero means a given asset is sufficient in
      -- the inputs.
      insufficientAssets :: Array (CurrencySymbol /\ TokenName /\ BigInt)
      insufficientAssets = flattenValue diff
    nftTn <- liftMaybe
      (error "throwValueDifference: could not get unbonded token name")
      unbondedStakingTokenName
    let
      msg :: BigInt -> String -> String
      msg n assetName = "Insufficient inputs: Missing " <> toString n
        <> " "
        <> assetName

      mismatches :: Array String
      mismatches =
        map
          ( \(cs /\ tn /\ n) -> case unit of
              _
                | cs == adaSymbol && tn == adaToken -> msg n "Lovelace"
                | cs == agixCs && tn == agixTn -> msg n "AGIX"
                | cs == ubp.nftCs && tn == nftTn ->
                    "Missing input: Could not find pool's state UTxO"
                | cs == ubp.assocListCs ->
                    "Missing input: Could not find UTxO for entry with key "
                      <> (byteArrayToHex <<< getTokenName) tn
                | otherwise -> "Insufficient inputs: Missing "
                    <> toString n
                    <> " of unknown asset with currency symbol \""
                    <> (byteArrayToHex <<< getCurrencySymbol) cs
                    <> "\" and token name \""
                    <> (byteArrayToHex <<< getTokenName) tn
                    <> "\""
          )
          insufficientAssets

    throwContractError
      $ mconcat
      $ Array.intersperse ", "
          mismatches

mustPayToScript
  :: forall (i :: Type) (o :: Type)
   . ValidatorHash
  -> Datum
  -> Value
  -> TxConstraints i o
mustPayToScript vh dat = TxConstraints.mustPayToScript vh dat DatumWitness

getUtxoDatumHash :: TransactionOutputWithRefScript -> Maybe DataHash
getUtxoDatumHash = unwrap >>> _.output >>> unwrap >>> _.datum >>> case _ of
  OutputDatumHash dh -> pure dh
  _ -> Nothing

-- Copied from newer CTL revision
addressFromBech32 :: Bech32String -> Contract () Address
addressFromBech32 str = do
  networkId <- getNetworkId
  cslAddress <- liftContractM "addressFromBech32: unable to read address" $
    SA.addressFromBech32 str
  address <-
    liftContractM "addressFromBech32: unable to convert to plutus address" $
      toPlutusAddress cslAddress
  when (networkId /= SA.addressNetworkId cslAddress)
    (throwError $ error "addressFromBech32: address has wrong NetworkId")
  pure address

-- | `valueOf` but with sane argument ordering.
valueOf' :: CurrencySymbol -> TokenName -> Value -> BigInt
valueOf' cs tn v = valueOf v cs tn

-- | Convert from `Int` to `Natural`
nat :: Int -> Natural
nat = fromBigInt' <<< fromInt

-- | Convert from `Int` to `BigInt`
big :: Int -> BigInt
big = fromInt
