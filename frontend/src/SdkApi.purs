module SdkApi
  ( BondedPoolArgs
  , InitialBondedArgs
  , InitialUnbondedArgs
  , SdkAssetClass
  , SdkConfig
  , SdkInterest
  , SdkServerConfig
  , SdkIncompleteDeposit
  , SdkIncompleteClose
  , AnyType
  , buildContractConfig
  , callCloseBondedPool
  , callCloseUnbondedPool
  , callCreateBondedPool
  , callCreateUnbondedPool
  , callDepositBondedPool
  , callDepositUnbondedPool
  , callGetBondedPools
  , callGetNodeTime
  , callGetUnbondedPools
  , callHashPkh
  , callQueryAssocListUnbondedPool
  , callUserStakeBondedPool
  , callUserStakeUnbondedPool
  , callUserWithdrawBondedPool
  , callUserWithdrawUnbondedPool
  , callJust
  , callNothing
  , callConsumeMaybe
  , fromSdkLogLevel
  , toUnbondedPoolArgs
  ) where

import Contract.Prelude

import ClosePool (closeBondedPoolContract)
import Contract.Address (PaymentPubKeyHash)
import Contract.Config
  ( ConfigParams
  , WalletSpec
      ( ConnectToNami
      , ConnectToGero
      , ConnectToFlint
      , ConnectToLode
      , ConnectToEternl
      )
  )
import Contract.Monad (Contract, runContract)
import Contract.Numeric.NatRatio (fromNaturals, toRational)
import Contract.Numeric.Natural (Natural, fromBigInt, toBigInt)
import Contract.Numeric.Rational (Rational, denominator, numerator)
import Contract.Prim.ByteArray
  ( byteArrayFromAscii
  , byteArrayToHex
  , byteArrayToIntArray
  , hexToByteArray
  , hexToRawBytes
  , rawBytesToHex
  , ByteArray
  )
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , getCurrencySymbol
  , getTokenName
  , mkCurrencySymbol
  , mkTokenName
  )
import Control.Promise (Promise, fromAff)
import Control.Promise as Promise
import CreatePool (createBondedPoolContract, getBondedPoolsContract)
import Ctl.Internal.Serialization.Address (intToNetworkId)
import Ctl.Internal.Serialization.Hash
  ( ed25519KeyHashFromBytes
  , ed25519KeyHashToBytes
  )
import Data.ArrayBuffer.Types (Uint8Array)
import Data.BigInt (BigInt)
import Data.Char (fromCharCode)
import Data.Int as Int
import Data.Log.Level (LogLevel(Trace, Debug, Info, Warn, Error))
import Data.String.CodeUnits (fromCharArray)
import Data.UInt (UInt)
import Data.UInt as UInt
import DepositPool (depositBondedPoolContract)
import Effect.Aff (error)
import Effect.Exception (Error)
import Partial.Unsafe (unsafePartial)
import Types
  ( AssetClass(AssetClass)
  , BondedPoolParams(BondedPoolParams)
  , InitialBondedParams
  , ScriptVersion(..)
  )
import UnbondedStaking.ClosePool (closeUnbondedPoolContract)
import UnbondedStaking.CreatePool
  ( createUnbondedPoolContract
  , getUnbondedPoolsContract
  )
import UnbondedStaking.DepositPool (depositUnbondedPoolContract)
import UnbondedStaking.Types
  ( Entry(..)
  , IncompleteClose(..)
  , IncompleteDeposit(..)
  , InitialUnbondedParams
  , UnbondedPoolParams(UnbondedPoolParams)
  )
import UnbondedStaking.UserStake (userStakeUnbondedPoolContract)
import UnbondedStaking.UserWithdraw (userWithdrawUnbondedPoolContract)
import UnbondedStaking.Utils (queryAssocListUnbonded, calculateRewards)
import UserStake (userStakeBondedPoolContract)
import UserWithdraw (userWithdrawBondedPoolContract)
import Utils (currentRoundedTime, hashPkh)

-- | Configuration needed to call contracts from JS.
type SdkConfig =
  { ogmiosConfig :: SdkServerConfig
  , kupoConfig :: SdkServerConfig
  , datumCacheConfig :: SdkServerConfig
  , networkId :: Number -- converts to Int
  , logLevel :: String -- "Trace", "Debug", "Info", "Warn", "Error"
  , walletSpec :: String -- "Nami", "Gero", "Flint", "Lode"
  }

type SdkServerConfig =
  { host :: String
  , path :: String
  , port :: Number -- converts to UInt
  , secure :: Boolean
  }

type SdkInterest = { numerator :: BigInt, denominator :: BigInt }

type SdkAssetClass = { currencySymbol :: String, tokenName :: String }

fromSdkLogLevel :: String -> Either Error LogLevel
fromSdkLogLevel = case _ of
  "Trace" -> pure Trace
  "Debug" -> pure Debug
  "Info" -> pure Info -- default
  "Warn" -> pure Warn
  "Error" -> pure Error
  s -> Left $ error $ "Invalid `LogLevel`: " <> s

fromSdkServerConfig
  :: String
  -> SdkServerConfig
  -> Either Error
       { port :: UInt
       , path :: Maybe String
       , host :: String
       , secure :: Boolean
       }
fromSdkServerConfig serviceName conf@{ host, secure } = do
  port <-
    note (error $ "invalid " <> serviceName <> " port number")
      $ UInt.fromNumber' conf.port
  pure { port, host, path: fromSdkPath conf.path, secure }

fromSdkPath :: String -> Maybe String
fromSdkPath "" = Nothing
fromSdkPath s = Just s

fromSdkIncompleteDeposit :: SdkIncompleteDeposit -> IncompleteDeposit
fromSdkIncompleteDeposit { failedKeys: fk, totalDeposited, nextDepositAmt } =
  IncompleteDeposit
    { failedKeys: map wrap fk
    , totalDeposited
    , nextDepositAmt
    }

fromSdkIncompleteClose :: SdkIncompleteClose -> IncompleteClose
fromSdkIncompleteClose { failedKeys: fk } =
  IncompleteClose { failedKeys: map wrap fk }

buildContractConfig :: SdkConfig -> Effect (Promise (ConfigParams ()))
buildContractConfig cfg = Promise.fromAff $ do
  ogmiosConfig <- liftEither $ fromSdkServerConfig "ogmios" cfg.ogmiosConfig
  kupoConfig <- liftEither $ fromSdkServerConfig "kupo" cfg.kupoConfig
  datumCacheConfig <- liftEither $ fromSdkServerConfig "ogmios-datum-cache"
    cfg.datumCacheConfig
  networkIdInt <- liftM (errorWithContext "invalid `NetworkId`")
    $ Int.fromNumber cfg.networkId
  networkId <- liftM (errorWithContext "invalid `NetworkId`")
    $ intToNetworkId networkIdInt
  logLevel <- liftEither $ fromSdkLogLevel cfg.logLevel
  walletSpec <- Just <$> liftEither (fromSdkWalletSpec cfg.walletSpec)
  pure
    { ogmiosConfig
    , kupoConfig
    , datumCacheConfig
    , logLevel
    , networkId
    , walletSpec
    , customLogger: Nothing
    , suppressLogs: false
    , extraConfig: {}
    , hooks:
        { beforeInit: Nothing
        , beforeSign: Nothing
        , onError: Nothing
        , onSuccess: Nothing
        }
    }
  where
  errorWithContext :: String -> Error
  errorWithContext msg = error $ "buildContractConfig: " <> msg

callWithArgs
  :: forall (a :: Type) (b :: Type) (c :: Type)
   . (a -> Either Error b)
  -> (b -> Contract () c)
  -> ConfigParams ()
  -> a
  -> Effect (Promise c)
callWithArgs f contract cfg args = Promise.fromAff
  $ runContract cfg
  <<< contract
  =<< liftEither (f args)

toSdkAssetClass :: AssetClass -> SdkAssetClass
toSdkAssetClass (AssetClass ac) =
  { currencySymbol: byteArrayToHex $ getCurrencySymbol ac.currencySymbol
  , tokenName: tokenNameAscii ac.tokenName
  }
  where
  tokenNameAscii :: TokenName -> String
  tokenNameAscii = unsafePartial
    $ fromJust
    <<< map fromCharArray
    <<< traverse fromCharCode
    <<< byteArrayToIntArray
    <<< getTokenName

toSdkInterest :: Rational -> SdkInterest
toSdkInterest i = { numerator: numerator i, denominator: denominator i }

toSdkAdmin :: PaymentPubKeyHash -> String
toSdkAdmin = rawBytesToHex <<< ed25519KeyHashToBytes <<< unwrap <<< unwrap

toSdkCurrencySymbol :: CurrencySymbol -> String
toSdkCurrencySymbol = byteArrayToHex <<< getCurrencySymbol

toSdkIncompleteDeposit :: IncompleteDeposit -> SdkIncompleteDeposit
toSdkIncompleteDeposit
  (IncompleteDeposit { failedKeys: fk, totalDeposited, nextDepositAmt }) =
  { failedKeys: map unwrap fk
  , totalDeposited
  , nextDepositAmt
  }

toSdkIncompleteClose :: IncompleteClose -> SdkIncompleteClose
toSdkIncompleteClose (IncompleteClose { failedKeys: fk }) =
  { failedKeys: map unwrap fk }

fromSdkNat :: String -> String -> BigInt -> Either Error Natural
fromSdkNat context name bint = note (error msg) $ fromBigInt bint
  where
  msg :: String
  msg = context <> ": Could not convert " <> name <> " to `Natural`"

fromSdkAssetClass :: String -> SdkAssetClass -> Either Error AssetClass
fromSdkAssetClass context { currencySymbol, tokenName } = map wrap
  $ { currencySymbol: _, tokenName: _ }
  <$> fromSdkCurrencySymbol context currencySymbol
  <*> fromSdkTokenName context tokenName

fromSdkTokenName :: String -> String -> Either Error TokenName
fromSdkTokenName context tokenName = note (errorWithMsg context "`TokenName`")
  $ mkTokenName
  =<< byteArrayFromAscii tokenName

fromSdkCurrencySymbol :: String -> String -> Either Error CurrencySymbol
fromSdkCurrencySymbol context currencySymbol =
  note (errorWithMsg context "`CurrencySymbol`") $ mkCurrencySymbol =<<
    hexToByteArray currencySymbol

fromSdkInterest :: String -> SdkInterest -> Either Error Rational
fromSdkInterest context { numerator, denominator } = do
  interestNum <- fromSdkNat context "interest numerator" numerator
  interestDen <- fromSdkNat context "interest denominator" denominator
  note (error msg) $ toRational <$> fromNaturals interestNum interestDen
  where
  msg :: String
  msg = context <> ": invalid `Rational`"

fromSdkAdmin :: String -> String -> Either Error PaymentPubKeyHash
fromSdkAdmin context admin = note (error msg)
  $ wrap
  <<< wrap
  <$> (ed25519KeyHashFromBytes <<< unwrap =<< hexToRawBytes admin)
  where
  msg :: String
  msg = context <> ": invalid admin"

fromSdkWalletSpec :: String -> Either Error WalletSpec
fromSdkWalletSpec = case _ of
  "Nami" -> pure ConnectToNami
  "Gero" -> pure ConnectToGero
  "Flint" -> pure ConnectToFlint
  "Lode" -> pure ConnectToLode
  "Eternl" -> pure ConnectToEternl
  s -> Left $ error $ "Invalid `WalletSpec`: " <> s

errorWithMsg :: String -> String -> Error
errorWithMsg context name = error $ context <> ": invalid " <> name

--Bonded------------------------------------------------------------------------

type BondedPoolArgs =
  { iterations :: BigInt -- Natural
  , start :: BigInt -- like POSIXTime
  , end :: BigInt -- like POSIXTime
  , userLength :: BigInt -- like POSIXTime
  , bondingLength :: BigInt -- like POSIXTime
  , interest :: SdkInterest
  , minStake :: BigInt -- Natural
  , maxStake :: BigInt -- Natural
  , bondedAssetClass :: SdkAssetClass
  , admin :: String -- PaymentPubKeyHash
  , nftCs :: String -- CurrencySymbol
  , assocListCs :: String -- CurrencySymbol
  }

type InitialBondedArgs =
  { iterations :: BigInt -- Natural
  , start :: BigInt -- like POSIXTime
  , end :: BigInt -- like POSIXTime
  , userLength :: BigInt -- like POSIXTime
  , bondingLength :: BigInt -- like POSIXTime
  , interest :: SdkInterest
  , minStake :: BigInt -- Natural
  , maxStake :: BigInt -- Natural
  , bondedAssetClass :: SdkAssetClass
  }

callCreateBondedPool
  :: ConfigParams ()
  -> InitialBondedArgs
  -> Effect
       (Promise { args :: BondedPoolArgs, address :: String, txId :: String })
callCreateBondedPool cfg iba = Promise.fromAff do
  ibp <- liftEither $ fromInitialBondedArgs iba
  { bondedPoolParams: bpp, address, txId } <- runContract cfg $
    createBondedPoolContract ibp Production
  pure $ { args: toBondedPoolArgs bpp, address, txId }

callGetBondedPools
  :: ConfigParams ()
  -> String
  -> InitialBondedArgs
  -> Effect (Promise (Array BondedPoolArgs))
callGetBondedPools cfg addrStr iba = Promise.fromAff do
  ibp <- liftEither $ fromInitialBondedArgs iba
  bpps <- runContract cfg $ getBondedPoolsContract addrStr ibp Production
  pure $ map toBondedPoolArgs bpps

callDepositBondedPool
  :: ConfigParams ()
  -> BondedPoolArgs
  -> BigInt
  -> Array Int
  -> Effect (Promise (Array Int))
callDepositBondedPool cfg bpa bi arr = Promise.fromAff $ runContract cfg do
  upp <- liftEither $ fromBondedPoolArgs bpa
  nat <- liftM (error "callDepositBondedPool: Invalid natural number")
    $ fromBigInt bi
  depositBondedPoolContract upp Production nat arr

callCloseBondedPool
  :: ConfigParams ()
  -> BondedPoolArgs
  -> BigInt
  -> Array Int
  -> Effect (Promise (Array Int))
callCloseBondedPool cfg bpa bi arr = Promise.fromAff $ runContract cfg do
  upp <- liftEither $ fromBondedPoolArgs bpa
  nat <- liftM (error "callCloseBondedPool: Invalid natural number")
    $ fromBigInt bi
  closeBondedPoolContract upp Production nat arr

callUserStakeBondedPool
  :: ConfigParams ()
  -> BondedPoolArgs
  -> BigInt
  -> Effect (Promise { txId :: String })
callUserStakeBondedPool cfg bpa bi = Promise.fromAff $ runContract cfg do
  bpp <- liftEither $ fromBondedPoolArgs bpa
  nat <- liftM (error "callUserStakeBondedPool: Invalid natural number")
    $ fromBigInt bi
  userStakeBondedPoolContract bpp Production nat

callUserWithdrawBondedPool
  :: ConfigParams () -> BondedPoolArgs -> Effect (Promise { txId :: String })
callUserWithdrawBondedPool =
  callWithBondedPoolArgs (\ubp -> userWithdrawBondedPoolContract ubp Production)

callWithBondedPoolArgs
  :: (BondedPoolParams -> Contract () { txId :: String })
  -> ConfigParams ()
  -> BondedPoolArgs
  -> Effect (Promise { txId :: String })
callWithBondedPoolArgs contract cfg = callWithArgs fromBondedPoolArgs contract
  cfg

fromInitialBondedArgs
  :: InitialBondedArgs -> Either Error InitialBondedParams
fromInitialBondedArgs iba = do
  iterations <- fromSdkNat' "iteration" iba.iterations
  interest <- fromSdkInterest context iba.interest
  minStake <- fromSdkNat' "minStake" iba.minStake
  maxStake <- fromSdkNat' "maxStake" iba.maxStake
  bondedAssetClass <- fromSdkAssetClass context iba.bondedAssetClass
  pure $ wrap
    { iterations
    , start: iba.start
    , end: iba.end
    , userLength: iba.userLength
    , bondingLength: iba.bondingLength
    , interest
    , minStake
    , maxStake
    , bondedAssetClass
    }
  where
  fromSdkNat' :: String -> BigInt -> Either Error Natural
  fromSdkNat' name = fromSdkNat context name

  context :: String
  context = "fromInitialBondedArgs"

toBondedPoolArgs :: BondedPoolParams -> BondedPoolArgs
toBondedPoolArgs (BondedPoolParams bpp) =
  { iterations: toBigInt bpp.iterations
  , start: bpp.start
  , end: bpp.end
  , userLength: bpp.userLength
  , bondingLength: bpp.bondingLength
  , interest: toSdkInterest bpp.interest
  , minStake: toBigInt bpp.minStake
  , maxStake: toBigInt bpp.maxStake
  , bondedAssetClass: toSdkAssetClass bpp.bondedAssetClass
  , admin: toSdkAdmin bpp.admin
  , nftCs: toSdkCurrencySymbol bpp.nftCs
  , assocListCs: toSdkCurrencySymbol bpp.assocListCs
  }

fromBondedPoolArgs :: BondedPoolArgs -> Either Error BondedPoolParams
fromBondedPoolArgs bpa = do
  iterations <- fromSdkNat' "iterations" bpa.iterations
  interest <- fromSdkInterest context bpa.interest
  minStake <- fromSdkNat' "minStake" bpa.minStake
  maxStake <- fromSdkNat' "maxStake" bpa.maxStake
  admin <- fromSdkAdmin context bpa.admin
  bondedAssetClass <- fromSdkAssetClass context bpa.bondedAssetClass
  nftCs <- fromSdkCurrencySymbol context bpa.nftCs
  assocListCs <- fromSdkCurrencySymbol context bpa.assocListCs
  pure $ wrap
    { iterations
    , start: bpa.start
    , end: bpa.end
    , userLength: bpa.userLength
    , bondingLength: bpa.bondingLength
    , interest
    , minStake
    , maxStake
    , admin
    , bondedAssetClass
    , nftCs
    , assocListCs
    }
  where
  fromSdkNat' :: String -> BigInt -> Either Error Natural
  fromSdkNat' name = fromSdkNat context name

  context :: String
  context = "fromBondedPoolArgs"

--Unbonded----------------------------------------------------------------------

type UnbondedPoolArgs =
  { start :: BigInt -- like POSIXTime
  , userLength :: BigInt -- like POSIXTime
  , adminLength :: BigInt -- like POSIXTime
  , bondingLength :: BigInt -- like POSIXTime
  , interestLength :: BigInt -- like POSIXTime
  , increments :: BigInt -- Natural
  , interest :: SdkInterest
  , minStake :: BigInt -- Natural
  , maxStake :: BigInt -- Natural
  , admin :: String -- PaymentPubKeyHash
  , unbondedAssetClass :: SdkAssetClass
  , nftCs :: String -- CurrencySymbol
  , assocListCs :: String -- CurrencySymbol
  }

type InitialUnbondedArgs =
  { start :: BigInt -- like POSIXTime
  , userLength :: BigInt -- like POSIXTime
  , adminLength :: BigInt -- like POSIXTime
  , interestLength :: BigInt -- like POSIXTime
  , bondingLength :: BigInt -- like POSIXTime
  , increments :: BigInt -- Natural
  , interest :: SdkInterest
  , minStake :: BigInt -- Natural
  , maxStake :: BigInt -- Natural
  , unbondedAssetClass :: SdkAssetClass
  }

type SdkIncompleteDeposit =
  { failedKeys :: Array Uint8Array
  , totalDeposited :: BigInt
  , nextDepositAmt :: BigInt
  }

type SdkIncompleteClose =
  { failedKeys :: Array Uint8Array
  }

callCreateUnbondedPool
  :: ConfigParams ()
  -> InitialUnbondedArgs
  -> Effect
       (Promise { args :: UnbondedPoolArgs, address :: String, txId :: String })
callCreateUnbondedPool cfg iba = Promise.fromAff do
  iup <- liftEither $ fromInitialUnbondedArgs iba
  { unbondedPoolParams: upp, address, txId } <- runContract cfg $
    createUnbondedPoolContract
      iup
      Production
  pure $ { args: toUnbondedPoolArgs upp, address, txId }

callGetUnbondedPools
  :: ConfigParams ()
  -> String
  -> InitialUnbondedArgs
  -> Effect (Promise (Array UnbondedPoolArgs))
callGetUnbondedPools cfg addrStr iba = Promise.fromAff do
  ibp <- liftEither $ fromInitialUnbondedArgs iba
  ubpps <- runContract cfg $ getUnbondedPoolsContract addrStr ibp Production
  pure $ map toUnbondedPoolArgs ubpps

callDepositUnbondedPool
  :: ConfigParams ()
  -> BigInt
  -> UnbondedPoolArgs
  -> BigInt
  -> Maybe SdkIncompleteDeposit
  -> Effect (Promise (Maybe SdkIncompleteDeposit))
callDepositUnbondedPool cfg amt upa bi id' = Promise.fromAff $ runContract cfg
  do
    upp <- liftEither $ fromUnbondedPoolArgs upa
    nat <- liftM (error "callDepositUnbondedPool: Invalid natural number")
      $ fromBigInt bi
    let id = fromSdkIncompleteDeposit <$> id'
    map toSdkIncompleteDeposit <$> depositUnbondedPoolContract amt upp Production nat id

callCloseUnbondedPool
  :: ConfigParams ()
  -> UnbondedPoolArgs
  -> BigInt
  -> Array Int
  -> Effect (Promise (Array Int))
callCloseUnbondedPool cfg upa bi arr = Promise.fromAff $ runContract cfg do
  upp <- liftEither $ fromUnbondedPoolArgs upa
  nat <- liftM (error "callCloseUnbondedPool: Invalid natural number")
    $ fromBigInt bi
  closeUnbondedPoolContract upp Production nat arr

callUserStakeUnbondedPool
  :: ConfigParams ()
  -> UnbondedPoolArgs
  -> BigInt
  -> Effect
       ( Promise
           { txId :: String }
       )
callUserStakeUnbondedPool cfg upa bi = Promise.fromAff $ runContract cfg do
  upp <- liftEither $ fromUnbondedPoolArgs upa
  nat <- liftM (error "callUserStakeUnbondedPool: Invalid natural number")
    $ fromBigInt bi
  userStakeUnbondedPoolContract upp Production nat

callUserWithdrawUnbondedPool
  :: ConfigParams ()
  -> UnbondedPoolArgs
  -> Effect
       ( Promise
           { txId :: String }
       )
callUserWithdrawUnbondedPool =
  callWithUnbondedPoolArgs
    (\ubp -> userWithdrawUnbondedPoolContract ubp Production)

callWithUnbondedPoolArgs
  :: ( UnbondedPoolParams
       -> Contract ()
            { txId :: String }
     )
  -> ConfigParams ()
  -> UnbondedPoolArgs
  -> Effect
       ( Promise
           { txId :: String }
       )
callWithUnbondedPoolArgs contract cfg = callWithArgs fromUnbondedPoolArgs
  contract
  cfg

type UserEntry =
  { key :: ByteArray
  , deposited :: BigInt
  , rewards :: Rational
  , nextCycleRewards :: Rational
  }

callHashPkh :: String -> Effect (Promise ByteArray)
callHashPkh pkh = Promise.fromAff $ do
  p <- liftEither $ fromSdkAdmin "callHashPkh" pkh
  hashPkh p

callQueryAssocListUnbondedPool
  :: ConfigParams ()
  -> UnbondedPoolArgs
  -> Effect (Promise (Array UserEntry))
callQueryAssocListUnbondedPool cfg upa = Promise.fromAff $ runContract cfg do
  upp <- liftEither $ fromUnbondedPoolArgs upa
  entries <- queryAssocListUnbonded upp Production
  pure $ map
    ( \(Entry e) ->
        { key: e.key
        , deposited: e.deposited
        , rewards: e.rewards
        , nextCycleRewards: calculateRewards (wrap e)
        }
    )
    entries

toUnbondedPoolArgs :: UnbondedPoolParams -> UnbondedPoolArgs
toUnbondedPoolArgs (UnbondedPoolParams upp) =
  { start: upp.start
  , userLength: upp.userLength
  , adminLength: upp.adminLength
  , bondingLength: upp.bondingLength
  , interestLength: upp.interestLength
  , increments: toBigInt upp.increments
  , interest: toSdkInterest upp.interest
  , minStake: toBigInt upp.minStake
  , maxStake: toBigInt upp.maxStake
  , admin: toSdkAdmin upp.admin
  , unbondedAssetClass: toSdkAssetClass upp.unbondedAssetClass
  , nftCs: toSdkCurrencySymbol upp.nftCs
  , assocListCs: toSdkCurrencySymbol upp.assocListCs
  }

fromUnbondedPoolArgs :: UnbondedPoolArgs -> Either Error UnbondedPoolParams
fromUnbondedPoolArgs upa = do
  interest <- fromSdkInterest context upa.interest
  minStake <- fromSdkNat' "minStake" upa.minStake
  maxStake <- fromSdkNat' "maxStake" upa.maxStake
  increments <- fromSdkNat' "increments" upa.increments
  admin <- fromSdkAdmin context upa.admin
  unbondedAssetClass <- fromSdkAssetClass context upa.unbondedAssetClass
  nftCs <- fromSdkCurrencySymbol context upa.nftCs
  assocListCs <- fromSdkCurrencySymbol context upa.assocListCs
  pure $ wrap
    { start: upa.start
    , userLength: upa.userLength
    , adminLength: upa.adminLength
    , bondingLength: upa.bondingLength
    , interestLength: upa.interestLength
    , increments
    , interest
    , minStake
    , maxStake
    , admin
    , unbondedAssetClass
    , nftCs
    , assocListCs
    }
  where
  fromSdkNat' :: String -> BigInt -> Either Error Natural
  fromSdkNat' name = fromSdkNat context name

  context :: String
  context = "fromUnbondedPoolArgs"

fromInitialUnbondedArgs
  :: InitialUnbondedArgs -> Either Error InitialUnbondedParams
fromInitialUnbondedArgs iba = do
  interest <- fromSdkInterest context iba.interest
  increments <- fromSdkNat' "increments" iba.increments
  minStake <- fromSdkNat' "minStake" iba.minStake
  maxStake <- fromSdkNat' "maxStake" iba.maxStake
  unbondedAssetClass <- fromSdkAssetClass context iba.unbondedAssetClass
  pure $ wrap
    { start: iba.start
    , userLength: iba.userLength
    , adminLength: iba.adminLength
    , bondingLength: iba.bondingLength
    , interestLength: iba.interestLength
    , interest
    , increments
    , minStake
    , maxStake
    , unbondedAssetClass
    }
  where
  fromSdkNat' :: String -> BigInt -> Either Error Natural
  fromSdkNat' name = fromSdkNat context name

  context :: String
  context = "fromInitialUnbondedArgs"

callGetNodeTime :: ConfigParams () -> Effect (Promise BigInt)
callGetNodeTime cfg = fromAff
  $ runContract cfg { walletSpec = Nothing }
  $ unwrap
  <$> currentRoundedTime

-- We export constructors and consumers of `Maybe` for the JS code

callJust :: forall a . a -> Maybe a
callJust = Just

callNothing :: forall a . Maybe a
callNothing = Nothing

-- We need this escape hatch to allow the JS code to consume a `Maybe a`
-- however it wants
foreign import data AnyType :: Type

callConsumeMaybe :: forall a . (a -> AnyType) -> (Unit -> AnyType) -> Maybe a -> AnyType
callConsumeMaybe just nothing = case _ of
    Nothing -> nothing unit
    Just x -> just x
