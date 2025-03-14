module Settings
  ( agixCs
  , agixTn
  , bondedStakingTokenName
  , ntxCs
  , ntxTn
  , testInitUnbondedParams
  , unbondedStakingTokenName
  , confirmationTimeout
  , submissionAttempts
  ) where

import Contract.Prelude

import Contract.Numeric.NatRatio (fromNaturals, toRational)
import Contract.Numeric.Natural (Natural, fromBigInt')
import Contract.Numeric.Rational (Rational)
import Contract.Prim.ByteArray (byteArrayFromAscii, hexToByteArray)
import Contract.Value (CurrencySymbol, TokenName, mkCurrencySymbol, mkTokenName)
import Data.BigInt (BigInt, fromInt)
import Data.Int as Int
import Data.Maybe (Maybe)
import Data.Time.Duration (Seconds(Seconds))
import Types (AssetClass(AssetClass))
import UnbondedStaking.Types (InitialUnbondedParams(InitialUnbondedParams))

bondedStakingTokenName :: Maybe TokenName
bondedStakingTokenName = mkTokenName =<< byteArrayFromAscii "BondedStakingToken"

unbondedStakingTokenName :: Maybe TokenName
unbondedStakingTokenName = mkTokenName =<< byteArrayFromAscii
  "UnbondedStakingToken"

-- Defined as fixed rate for one time increment in APY
unbondedInterest :: Maybe Rational
unbondedInterest = toRational <$> fromNaturals (nat 1) (nat 100)

-- Could make these unsafe/partial for convenience:
agixCs :: Maybe CurrencySymbol
agixCs = mkCurrencySymbol
  =<< hexToByteArray "6f1a1f0c7ccf632cc9ff4b79687ed13ffe5b624cce288b364ebdce50"

agixTn :: Maybe TokenName
agixTn = mkTokenName =<< byteArrayFromAscii "AGIX"

ntxCs :: Maybe CurrencySymbol
ntxCs = mkCurrencySymbol
  =<< hexToByteArray "b5094f93ff9fcba9e8b257197d589cbcde3d92a108804e3a378bd2ce"

ntxTn :: Maybe TokenName
ntxTn = mkTokenName =<< byteArrayFromAscii "NTX"

testInitUnbondedParams :: Maybe InitialUnbondedParams
testInitUnbondedParams = do
  interest <- unbondedInterest
  currencySymbol <- agixCs
  tokenName <- agixTn
  pure $ InitialUnbondedParams
    { start: big 1000 -- dummy value
    , userLength: big 180_000 -- We use 3 minutes to make testing manageable
    , adminLength: big 180_000 -- We use 3 minutes to make testing manageable
    , bondingLength: big 180_000 -- We use 3 minutes to make testing manageable
    , interestLength: big 100
    , increments: nat 1800
    , interest: interest
    , minStake: nat 1
    , maxStake: nat 100_000_000
    , unbondedAssetClass: AssetClass
        { currencySymbol
        , tokenName
        }
    }

-- | The amount of time a contract should wait before considering a TX
-- | submission a failure
confirmationTimeout :: Seconds
confirmationTimeout = Seconds $ Int.toNumber 120

-- | The number of attempts a contract should do before giving up and
-- | throwing an error
submissionAttempts :: Int
submissionAttempts = 5

-- We use local definitions of `nat` and `big` to avoid an import cycle between
-- this module and `Utils`

nat :: Int -> Natural
nat = fromBigInt' <<< fromInt

big :: Int -> BigInt
big = fromInt

