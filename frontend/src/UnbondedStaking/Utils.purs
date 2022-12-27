module UnbondedStaking.Utils
  ( calculateRewards
  , getAdminTime
  , getBondingTime
  , getUserTime
  , mkUnbondedPoolParams
  , queryAssocListUnbonded
  ) where

import Contract.Prelude hiding (length)

import Contract.Address (PaymentPubKeyHash, scriptHashAddress)
import Contract.Monad (Contract, liftContractM, throwContractError, liftedE', liftedM)
import Contract.Numeric.Rational (Rational, (%))
import Contract.PlutusData (getDatumByHash, fromData)
import Contract.Prim.ByteArray (ByteArray)
import Contract.Scripts (validatorHash)
import Contract.Time (POSIXTime(POSIXTime), POSIXTimeRange, mkFiniteInterval)
import Contract.Transaction (TransactionInput, TransactionOutputWithRefScript)
import Contract.Utxos (utxosAt)
import Contract.Value (CurrencySymbol)
import Data.Array (filter, head, takeWhile, (..))
import Data.BigInt (BigInt, quot, toInt)
import Data.BigInt as BigInt
import Scripts.PoolValidator (mkUnbondedPoolValidator)
import UnbondedStaking.Types (UnbondedPoolParams(UnbondedPoolParams), InitialUnbondedParams(InitialUnbondedParams), Entry(..), UnbondedStakingDatum(..))
import Utils (big, currentRoundedTime, mkRatUnsafe, getUtxoDatumHash, mkOnchainAssocList)

-- | Admin deposit/closing
getAdminTime
  :: forall (r :: Row Type)
   . UnbondedPoolParams
  -> Contract r { currTime :: POSIXTime, range :: POSIXTimeRange }
getAdminTime (UnbondedPoolParams upp) = do
  -- Get time and round it up to the nearest second
  currTime@(POSIXTime currTime') <- currentRoundedTime
  -- Get timerange in which the staking should be done
  let
    cycleLength :: BigInt
    cycleLength = upp.userLength + upp.adminLength + upp.bondingLength

    adminStart :: BigInt
    adminStart = upp.start + upp.userLength

    adminEnd :: BigInt
    adminEnd = adminStart + upp.adminLength - big 1000
  -- Return range
  start /\ end <- liftContractM "getAdminTime: this is not a admin period" $
    isWithinPeriod currTime' cycleLength adminStart adminEnd
  pure
    { currTime
    , range: mkFiniteInterval (POSIXTime start) (POSIXTime $ end + BigInt.fromInt 1)
    }

-- | User deposits/withdrawals
getUserTime
  :: forall (r :: Row Type)
   . UnbondedPoolParams
  -> Contract r { currTime :: POSIXTime, range :: POSIXTimeRange }
getUserTime (UnbondedPoolParams upp) = do
  -- Get time and round it up to the nearest second
  currTime@(POSIXTime currTime') <- currentRoundedTime
  -- Get timerange in which the staking should be done
  let
    cycleLength :: BigInt
    cycleLength = upp.userLength + upp.adminLength + upp.bondingLength

    userStart :: BigInt
    userStart = upp.start

    userEnd :: BigInt
    userEnd = userStart + upp.userLength - big 1000
  -- Return range
  start /\ end <- liftContractM "getUserTime: this is not a user period" $
    isWithinPeriod currTime' cycleLength userStart userEnd
  pure
    { currTime
    , range: mkFiniteInterval (POSIXTime start) (POSIXTime $ end + BigInt.fromInt 1)
    }

-- | User withdrawals only
-- | Note: Period can either be in bonding period or user period
getBondingTime
  :: forall (r :: Row Type)
   . UnbondedPoolParams
  -> Contract r { currTime :: POSIXTime, range :: POSIXTimeRange }
getBondingTime (UnbondedPoolParams upp) = do
  -- Get time and round it up to the nearest second
  currTime@(POSIXTime currTime') <- currentRoundedTime
  -- Get timerange in which the staking should be done
  let
    cycleLength :: BigInt
    cycleLength = upp.userLength + upp.adminLength + upp.bondingLength

    userStart :: BigInt
    userStart = upp.start

    userEnd :: BigInt
    userEnd = userStart + upp.userLength - big 1000

    bondingStart :: BigInt
    bondingStart = upp.start + upp.userLength + upp.adminLength

    bondingEnd :: BigInt
    bondingEnd = bondingStart + upp.bondingLength - big 1000
    -- Period ranges
    userPeriod = isWithinPeriod currTime' cycleLength userStart userEnd
    bondingPeriod = isWithinPeriod currTime' cycleLength bondingStart bondingEnd

    getPeriod
      :: Maybe (Tuple BigInt BigInt)
      -> Maybe (Tuple BigInt BigInt)
      -> Maybe (Tuple BigInt BigInt)
    getPeriod user bonding = case user /\ bonding of
      user' /\ Nothing -> user'
      Nothing /\ bonding' -> bonding'
      _ /\ _ -> Nothing
  -- Return range
  start /\ end <- liftContractM "getUserTime: this is not a user/bonding period"
    $
      getPeriod userPeriod bondingPeriod
  pure { currTime, range: mkFiniteInterval (POSIXTime start) (POSIXTime end) }

-- | Returns the current/previous period and the next valid period in the
-- | future from the current time
isWithinPeriod
  :: forall (r :: Row Type)
   . BigInt
  -> BigInt
  -> BigInt
  -> BigInt
  -> Maybe (Tuple BigInt BigInt)
isWithinPeriod currTime cycleLength start end =
  let
    currentIteration :: Int
    currentIteration = fromMaybe 0 $ toInt (start `quot` cycleLength)

    upperBound :: Int
    upperBound = 1 + currentIteration

    possibleRanges :: Array (Tuple BigInt BigInt)
    possibleRanges =
      ( \i -> (cycleLength * big (i - 1) + start)
          /\ (cycleLength * big (i - 1) + end)
      )
        <$> 1 .. upperBound

    periods :: Array (Tuple BigInt BigInt)
    periods =
      takeWhile (\(start' /\ _) -> currTime >= start')
        possibleRanges

    currentPeriod :: Array (Tuple BigInt BigInt)
    currentPeriod =
      filter
        (\(start' /\ end') -> start' <= currTime && currTime < end')
        periods
  in
    if null currentPeriod then Nothing else head currentPeriod

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
  -> Contract () (Array Entry)
queryAssocListUnbonded
  params@
    ( UnbondedPoolParams
        { assocListCs
        }
    ) = do
  -- Fetch information related to the pool
  -- Get the unbonded pool validator and hash
  validator <- liftedE' "queryAssocListUnbonded: Cannot create validator"
    $ mkUnbondedPoolValidator params
  let valHash = validatorHash validator
  let poolAddr = scriptHashAddress valHash Nothing
  -- Get the unbonded pool's utxo
  unbondedPoolUtxos <- utxosAt poolAddr

  let assocList = mkOnchainAssocList assocListCs unbondedPoolUtxos
  getListDatums assocList

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
