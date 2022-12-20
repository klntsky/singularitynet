module UnbondedStaking.Types
  ( SnetInitialParams
  , SnetContractEnv
  , SnetContract
  , Period(..)
  , Entry(..)
  , InitialUnbondedParams(..)
  , UnbondedPoolParams(..)
  , UnbondedStakingAction(..)
  , UnbondedStakingDatum(..)
  ) where

import Contract.Prelude

import Contract.Address (PaymentPubKeyHash)
import Contract.Monad (Contract(..))
import Contract.Numeric.Natural (Natural)
import Contract.Numeric.Rational (Rational)
import Contract.PlutusData (class FromData, class HasPlutusSchema, class ToData, type (:+), type (:=), type (@@), I, PlutusData(Constr), PNil, genericFromData, genericToData, toData, S, Z)
import Contract.Prim.ByteArray (ByteArray)
import Contract.Value (CurrencySymbol)
import Control.Monad.Reader (ReaderT(..))
import Data.BigInt (BigInt)
import Types (AssetClass, BurningAction, MintingAction, ScriptVersion)

-- | The necessary data for initialising a pool.
type SnetInitialParams = {
   initialUnbondedParams :: InitialUnbondedParams
   , scriptVersion :: ScriptVersion
}

-- | The environment for executing any SNet contract
type SnetContractEnv = {
   unbondedPoolParams :: UnbondedPoolParams
   , scriptVersion :: ScriptVersion
}

type SnetContract a = ReaderT SnetContractEnv (Contract ()) a

-- | This datatype is only used in offchain. It is used to specify a period to
-- wait for. These match the three non-overlapping time intervals that conform
-- a given cycle according to the spec. Additionally, there is a
-- `ClosedPeriod`, which is used to wait until the pool is closed. Since the
-- unbonded pool does not have an end time, this can wait an indefinite amount
-- of time.
data Period = UserPeriod | AdminPeriod | BondingPeriod | ClosedPeriod

derive instance Eq Period
derive instance Generic Period _

instance Show Period where
    show = genericShow

-- TODO: Add missing `ToData` instances for POSIXTime and NatRatio.
newtype UnbondedPoolParams =
  UnbondedPoolParams
    { start :: BigInt
    , userLength :: BigInt
    , adminLength :: BigInt
    , bondingLength :: BigInt
    , interestLength :: BigInt
    , increments :: Natural
    , interest :: Rational
    , minStake :: Natural
    , maxStake :: Natural
    , admin :: PaymentPubKeyHash
    , unbondedAssetClass :: AssetClass
    , nftCs :: CurrencySymbol
    , assocListCs :: CurrencySymbol
    }

derive instance Generic UnbondedPoolParams _
derive instance Eq UnbondedPoolParams
derive instance Newtype UnbondedPoolParams _

instance Show UnbondedPoolParams where
  show = genericShow

newtype InitialUnbondedParams = InitialUnbondedParams
  { start :: BigInt
  , userLength :: BigInt
  , adminLength :: BigInt
  , bondingLength :: BigInt
  , interestLength :: BigInt
  , increments :: Natural
  , interest :: Rational
  , minStake :: Natural
  , maxStake :: Natural
  , unbondedAssetClass :: AssetClass
  }

derive instance Generic InitialUnbondedParams _
derive instance Newtype InitialUnbondedParams _
derive instance Eq InitialUnbondedParams

instance Show InitialUnbondedParams where
  show = genericShow

-- The generic intsance is given below this, but we use this one for compilation
-- speed.
-- We copy the order of the fields from the Haskell implementation
instance ToData UnbondedPoolParams where
  toData (UnbondedPoolParams params) =
    Constr zero
      [ toData params.start
      , toData params.userLength
      , toData params.adminLength
      , toData params.bondingLength
      , toData params.interestLength
      , toData params.increments
      , toData params.interest
      , toData params.minStake
      , toData params.maxStake
      , toData params.admin
      , toData params.unbondedAssetClass
      , toData params.nftCs
      , toData params.assocListCs
      ]

-- Add these back in when generic instances have faster compilation.
-- instance https://github.com/Plutonomicon/cardano-transaction-lib/issues/433
--   HasPlutusSchema UnbondedPoolParams
--     ( "UnbondedPoolParams"
--         :=
--           ( "start" := I BigInt
--               :+ "userLength"
--               := I BigInt
--               :+ "adminLength"
--               := I BigInt
--               :+ "bondingLength"
--               := I BigInt
--               :+ "interestLength"
--               := I BigInt
--               :+ "increments"
--               := I Natural
--               :+ "interest"
--               := I Rational
--               :+ "minStake"
--               := I Natural
--               :+ "maxStake"
--               := I Natural
--               :+ "admin"
--               := I PaymentPubKeyHash
--               :+ "unbondedAssetClass"
--               := I AssetClass
--               :+ "nftCs"
--               := I CurrencySymbol
--               :+ "assocListCs"
--               := I CurrencySymbol
--               :+ PNil
--           )
--         @@ Z
--         :+ PNil
--     )

-- instance ToData UnbondedPoolParams where
--   toData = genericToData

data UnbondedStakingDatum
  = StateDatum { maybeEntryName :: Maybe ByteArray, open :: Boolean }
  | EntryDatum { entry :: Entry }
  | AssetDatum

derive instance Generic UnbondedStakingDatum _
derive instance Eq UnbondedStakingDatum

instance
  HasPlutusSchema UnbondedStakingDatum
    ( "StateDatum"
        :=
          ( "maybeEntryName" := I (Maybe ByteArray)
              :+ "open"
              := I Boolean
              :+ PNil
          )
        @@ Z
        :+ "EntryDatum"
        :=
          ( "entry" := I Entry :+ PNil
          )
        @@ (S Z)
        :+ "AssetDatum"
        := PNil
        @@ (S (S Z))
        :+ PNil
    )

instance FromData UnbondedStakingDatum where
  fromData = genericFromData

instance ToData UnbondedStakingDatum where
  toData = genericToData

instance Show UnbondedStakingDatum where
  show = genericShow

data UnbondedStakingAction
  = AdminAct { totalRewards :: Natural, totalDeposited :: Natural }
  | StakeAct
      { stakeAmount :: Natural
      , stakeHolder :: PaymentPubKeyHash
      , mintingAction :: Maybe MintingAction
      }
  | WithdrawAct
      { stakeHolder :: PaymentPubKeyHash
      , burningAction :: BurningAction
      }
  | CloseAct

instance
  HasPlutusSchema UnbondedStakingAction
    ( "AdminAct"
        :=
          ( "totalRewards" := I Natural
              :+ "totalDeposited"
              := I Natural
              :+ PNil
          )
        @@ Z
        :+ "StakeAct"
        :=
          ( "stakeAmount" := I Natural
              :+ "stakeHolder"
              := I PaymentPubKeyHash
              :+ "mintingAction"
              := I (Maybe MintingAction)
              :+ PNil
          )
        @@ (S Z)
        :+ "WithdrawAct"
        :=
          ( "stakeHolder" := I PaymentPubKeyHash
              :+ "burningAction"
              := I (BurningAction)
              :+ PNil
          )
        @@ (S (S Z))
        :+ "CloseAct"
        := PNil
        @@ (S (S (S Z)))
        :+ PNil
    )

derive instance Generic UnbondedStakingAction _
derive instance Eq UnbondedStakingAction

instance FromData UnbondedStakingAction where
  fromData = genericFromData

instance ToData UnbondedStakingAction where
  toData = genericToData

newtype Entry =
  Entry
    { key :: ByteArray
    , deposited :: BigInt
    , newDeposit :: BigInt
    , rewards :: Rational
    , totalRewards :: BigInt
    , totalDeposited :: BigInt
    , open :: Boolean
    , next :: Maybe ByteArray
    }

derive instance Generic Entry _
derive instance Newtype Entry _
derive instance Eq Entry

instance
  HasPlutusSchema Entry
    ( "Entry"
        :=
          ( "key" := I ByteArray
              :+ "deposited"
              := I BigInt
              :+ "newDeposit"
              := I BigInt
              :+ "rewards"
              := I Rational
              :+ "totalRewards"
              := I BigInt
              :+ "totalDeposited"
              := I BigInt
              :+ "open"
              := I Boolean
              :+ "next"
              := I (Maybe ByteArray)
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

instance FromData Entry where
  fromData = genericFromData

instance ToData Entry where
  toData = genericToData

instance Show Entry where
  show = genericShow
