module UnbondedStaking.Utils
  ( calculateRewards
  , getAdminTime
  , getUserOrBondingTime
  , getUserTime
  , getContainingPeriodRange
  , getPeriodRange
  , getNextPeriodRange
  , mkUnbondedPoolParams
  , queryAssocListUnbonded
  , queryStateUnbonded
  ) where

import Contract.Prelude hiding (length)

import Contract.Address (PaymentPubKeyHash, scriptHashAddress)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractE, liftContractM, liftedE', liftedM, throwContractError)
import Contract.Numeric.Rational (Rational, (%))
import Contract.PlutusData (getDatumByHash, fromData)
import Contract.Prim.ByteArray (ByteArray)
import Contract.Scripts (validatorHash)
import Contract.Time (POSIXTime(POSIXTime), POSIXTimeRange, always, mkFiniteInterval)
import Contract.Transaction (TransactionInput, TransactionOutputWithRefScript)
import Contract.Utxos (utxosAt)
import Contract.Value (CurrencySymbol)
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Map as Map
import Scripts.PoolValidator (mkUnbondedPoolValidator)
import Settings (unbondedStakingTokenName)
import Types (ScriptVersion(..))
import UnbondedStaking.Types (Entry(..), InitialUnbondedParams(InitialUnbondedParams), PeriodError(..), UnbondedPoolParams(UnbondedPoolParams), UnbondedStakingDatum(..))
import Utils (big, currentRoundedTime, getUtxoDatumHash, mkOnchainAssocList, mkRatUnsafe, valueOf')

-- | Admin deposit/closing
getAdminTime
  :: forall (r :: Row Type)
   . UnbondedPoolParams
  -> ScriptVersion
  -> Contract r { currTime :: POSIXTime, range :: POSIXTimeRange }
getAdminTime (UnbondedPoolParams upp) = case _ of
  Production -> do
    -- Get time and round it up to the nearest second
    currTime@(POSIXTime currTime') <- currentRoundedTime
    -- Get timerange in which the staking should be done
    let
      cycleLength :: BigInt
      cycleLength = upp.userLength + upp.adminLength + upp.bondingLength

      adminStart :: BigInt
      adminStart = upp.userLength

      adminEnd :: BigInt
      adminEnd = adminStart + upp.adminLength - big 1000
    -- Return range
    start /\ end <- liftContractE $
      getContainingPeriodRange currTime' upp.start cycleLength adminStart
        adminEnd
    pure
      { currTime
      , range: mkFiniteInterval (POSIXTime start)
          (POSIXTime $ end + BigInt.fromInt 1)
      }
  DebugNoTimeChecks -> noTimeChecks "getAdminTime"

-- | User deposits/withdrawals
getUserTime
  :: forall (r :: Row Type)
   . UnbondedPoolParams
  -> ScriptVersion
  -> Contract r { currTime :: POSIXTime, range :: POSIXTimeRange }
getUserTime (UnbondedPoolParams upp) = case _ of
  Production -> do
    -- Get time and round it up to the nearest second
    currTime@(POSIXTime currTime') <- currentRoundedTime
    -- Get timerange in which the staking should be done
    let
      cycleLength :: BigInt
      cycleLength = upp.userLength + upp.adminLength + upp.bondingLength

      userStart :: BigInt
      userStart = zero

      userEnd :: BigInt
      userEnd = userStart + upp.userLength - big 1000
    -- Return range
    start /\ end <- liftContractE $ getContainingPeriodRange currTime' upp.start cycleLength userStart userEnd
    pure
      { currTime
      , range: mkFiniteInterval (POSIXTime start)
          (POSIXTime $ end + BigInt.fromInt 1)
      }
  DebugNoTimeChecks -> noTimeChecks "getUserTime"

-- | User withdrawals only
-- | Note: Period can either be in bonding period or user period
getUserOrBondingTime
  :: forall (r :: Row Type)
   . UnbondedPoolParams
  -> ScriptVersion
  -> Contract r { currTime :: POSIXTime, range :: POSIXTimeRange }
getUserOrBondingTime (UnbondedPoolParams upp) = case _ of
  Production -> do
    -- Get time and round it up to the nearest second
    currTime@(POSIXTime currTime') <- currentRoundedTime
    -- Get timerange in which the staking should be done
    let
      cycleLength :: BigInt
      cycleLength = upp.userLength + upp.adminLength + upp.bondingLength

      userStart :: BigInt
      userStart = zero

      userEnd :: BigInt
      userEnd = userStart + upp.userLength - big 1000

      bondingStart :: BigInt
      bondingStart = userEnd + upp.adminLength

      bondingEnd :: BigInt
      bondingEnd = bondingStart + upp.bondingLength - big 1000
      -- Period ranges
      userPeriod = getContainingPeriodRange currTime' upp.start cycleLength
        userStart
        userEnd
      bondingPeriod = getContainingPeriodRange currTime' upp.start cycleLength
        bondingStart
        bondingEnd

      getPeriod
        :: Either PeriodError (Tuple BigInt BigInt)
        -> Either PeriodError (Tuple BigInt BigInt)
        -> Either (Array PeriodError) (Tuple BigInt BigInt)
      getPeriod user bonding = case user /\ bonding of
        Right user' /\ Left _ -> pure user'
        Left _ /\ Right bonding' -> pure bonding'
        Left e1 /\ Left e2 -> Left [e1, e2]
        -- This case is impossible, we just return the user period
        Right user' /\ _ -> pure user'
    -- Return range
    start /\ end <-
      liftContractE  $ getPeriod userPeriod bondingPeriod
    pure
      { currTime
      , range: mkFiniteInterval (POSIXTime start)
          (POSIXTime $ end + BigInt.fromInt 1)
      }
  DebugNoTimeChecks -> noTimeChecks "getBondingTime"

-- | Returns the period's range if it contain the `currentTime`.
--
-- The period is defined by its `startOffset` and `endOffset`, while
-- `baseOffset` and `cycleLength` are parameters of the pool. If the `currTime`
-- is not contained within the period's range of the current interval, it fails.
getContainingPeriodRange
  :: forall (r :: Row Type)
   . BigInt
  -> BigInt
  -> BigInt
  -> BigInt
  -> BigInt
  -> Either PeriodError (Tuple BigInt BigInt)
getContainingPeriodRange
  currentTime
  baseOffset
  cycleLength
  startOffset
  endOffset =
      let periodRange@(start /\ end) =
               getPeriodRange
                 currentTime
                 baseOffset
                 cycleLength
                 startOffset
                 endOffset
          errInfo = { current: wrap currentTime, start: wrap start, end: wrap end }
      in case unit of
          _ | currentTime < start -> Left $ TooSoon errInfo
            | currentTime >= end -> Left $ TooLate errInfo
            | otherwise -> Right periodRange

-- | Creates the `UnbondedPoolParams` from the `InitialUnbondedParams` and
-- | runtime parameters from the user.
mkUnbondedPoolParams
  :: PaymentPubKeyHash
  -> CurrencySymbol
  -> CurrencySymbol
  -> InitialUnbondedParams
  -> UnbondedPoolParams
mkUnbondedPoolParams admin nftCs assocListCs (InitialUnbondedParams iup) = do
  UnbondedPoolParams
    { start: iup.start
    , userLength: iup.userLength
    , adminLength: iup.adminLength
    , bondingLength: iup.bondingLength
    , interestLength: iup.interestLength
    , increments: iup.increments
    , interest: iup.interest
    , minStake: iup.minStake
    , maxStake: iup.maxStake
    , admin
    , unbondedAssetClass: iup.unbondedAssetClass
    , nftCs
    , assocListCs
    }

-- | Calculates user awards according to spec formula
calculateRewards :: Entry -> Rational
calculateRewards (Entry e) = do
  -- New users will have zero total deposited for the first cycle
  if e.totalDeposited == zero then
    zero
  else
    let
      lhs = mkRatUnsafe $ e.totalRewards % e.totalDeposited
      rhs = e.rewards + mkRatUnsafe (e.deposited % one)
      rhs' = rhs - mkRatUnsafe (e.newDeposit % one)
      f = rhs' * lhs
    in
      e.rewards + f

queryAssocListUnbonded
  :: UnbondedPoolParams
  -> ScriptVersion
  -> Contract () (Array Entry)
queryAssocListUnbonded
  params@
    ( UnbondedPoolParams
        { assocListCs
        }
    )
  scriptVersion = do
  -- Fetch information related to the pool
  -- Get the unbonded pool validator and hash
  validator <- liftedE' "queryAssocListUnbonded: Cannot create validator"
    $ mkUnbondedPoolValidator params scriptVersion
  let valHash = validatorHash validator
  let poolAddr = scriptHashAddress valHash Nothing
  -- Get the unbonded pool's utxo
  unbondedPoolUtxos <- utxosAt poolAddr

  let assocList = mkOnchainAssocList assocListCs unbondedPoolUtxos
  getListDatums assocList

queryStateUnbonded
  :: UnbondedPoolParams
  -> ScriptVersion
  -> Contract () { maybeEntryName :: Maybe ByteArray, open :: Boolean }
queryStateUnbonded
  params@
    ( UnbondedPoolParams
        { nftCs
        }
    )
  scriptVersion = do
  -- Fetch information related to the pool
  validator <- liftedE' "queryStateUnbonded: Cannot create validator"
    $ mkUnbondedPoolValidator params scriptVersion
  nftTn <- liftContractM "queryStateUnbonded: Could not get staking token name"
    unbondedStakingTokenName
  let
    valHash = validatorHash validator
    poolAddr = scriptHashAddress valHash Nothing
  -- Get the state UTxO's datum
  unbondedPoolUtxos <- utxosAt poolAddr
  let
    hasStateNft :: TransactionOutputWithRefScript -> Boolean
    hasStateNft = unwrap >>> _.output >>> unwrap >>> _.amount
      >>> valueOf' nftCs nftTn
      >>> (_ == one)
  stateTxOut <- liftContractM "queryStateUnbonded: Could not find state utxo"
    <<< Array.head
    <<< Array.fromFoldable
    $ Map.filter hasStateNft unbondedPoolUtxos
  getDatum stateTxOut
  where
  getDatum
    :: TransactionOutputWithRefScript
    -> Contract () { maybeEntryName :: Maybe ByteArray, open :: Boolean }
  getDatum txOut = do
    dat <- liftedM "" <<< getDatumByHash <=< liftContractM "" $ getUtxoDatumHash
      txOut
    case fromData $ unwrap dat of
      Just (StateDatum r) -> pure r
      Just _ -> throwContractError "queryStateUnbonded: Expected a state datum"
      Nothing -> throwContractError "queryStateUnbonded: Could not parse datum"

-- | Get all entries' datums
getListDatums
  :: Array (ByteArray /\ TransactionInput /\ TransactionOutputWithRefScript)
  -> Contract () (Array Entry)
getListDatums arr = for arr \(_ /\ _ /\ txOut) -> do
  -- Get the entry's datum
  dHash <-
    liftContractM
      "getListDatums: Could not get entry's datum hash"
      $ getUtxoDatumHash txOut
  dat <-
    liftedM
      "getListDatums: Cannot get entry's datum" $ getDatumByHash dHash
  -- Parse it
  unbondedListDatum :: UnbondedStakingDatum <-
    liftContractM
      "getListDatums: Cannot parse entry's datum"
      $ fromData (unwrap dat)
  -- The get the entry datum
  case unbondedListDatum of
    EntryDatum { entry } -> pure entry
    StateDatum _ ->
      throwContractError
        "getListDatums: Expected a list datum but found a state datum"
    AssetDatum ->
      throwContractError
        "getListDatums: Expected an list datum but found an asset datum"

noTimeChecks
  :: forall (r :: Row Type)
   . String
  -> Contract r { currTime :: POSIXTime, range :: POSIXTimeRange }
noTimeChecks fnName = do
  logInfo' $ fnName <> ": Time checks deactivated - omitting check"
  currTime <- currentRoundedTime
  pure { currTime, range: always }

-- | Gets the period's range for the given time
getPeriodRange
  :: forall (r :: Row Type)
   . BigInt
  -> BigInt
  -> BigInt
  -> BigInt
  -> BigInt
  -> BigInt /\ BigInt
getPeriodRange time baseOffset cycleLength startOffset endOffset =
  let
    iteration :: BigInt
    iteration = (time - baseOffset) `BigInt.quot` cycleLength
  in
    (baseOffset + cycleLength * iteration + startOffset)
      /\ (baseOffset + cycleLength * iteration + endOffset)

-- | Gets the period's range for the next cycle
getNextPeriodRange
  :: forall (r :: Row Type)
   . BigInt
  -> BigInt
  -> BigInt
  -> BigInt
  -> BigInt
  -> BigInt /\ BigInt
getNextPeriodRange time baseOffset cycleLength startOffset endOffset =
  getPeriodRange (time + cycleLength) baseOffset cycleLength startOffset
    endOffset
