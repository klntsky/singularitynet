module UnbondedStaking.UserWithdraw
  ( userWithdrawUnbondedPoolContract
  , adminWithdrawUnbondedPoolContract
  ) where

import Contract.Prelude hiding (length)

import Contract.Address
  ( Address
  , Bech32String
  , PaymentPubKeyHash
  , StakePubKeyHash
  , addressFromBech32
  , getNetworkId
  , getWalletAddress
  , ownPaymentPubKeyHash
  , scriptHashAddress
  )
import Contract.Credential (Credential(..), StakingCredential(..))
import Contract.Log (logInfo')
import Contract.Monad
  ( Contract
  , liftContractM
  , liftedE
  , liftedE'
  , liftedM
  , throwContractError
  )
import Contract.Numeric.Rational (Rational, denominator, numerator)
import Contract.PlutusData
  ( PlutusData
  , Redeemer(Redeemer)
  , Datum(Datum)
  , fromData
  , getDatumByHash
  , toData
  )
import Contract.Prim.ByteArray (ByteArray)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (MintingPolicy, Validator, ValidatorHash, validatorHash)
import Contract.Transaction
  ( TransactionInput
  , TransactionOutput
  , TransactionOutputWithRefScript
  )
import Contract.TxConstraints
  ( TxConstraints
  , mustBeSignedBy
  , mustMintValueWithRedeemer
  , mustPayToPubKey
  , mustPayToPubKeyAddress
  , mustSpendScriptOutput
  , mustValidateIn
  )
import Contract.Utxos (UtxoMap, getWalletUtxos, utxosAt)
import Contract.Value (CurrencySymbol, TokenName, Value, mkTokenName, singleton)
import Control.Monad.Maybe.Trans (MaybeT(MaybeT), runMaybeT, lift)
import Ctl.Internal.Plutus.Conversion (fromPlutusAddress)
import Data.Array (catMaybes)
import Data.BigInt (BigInt)
import Data.Map as Map
import Scripts.ListNFT (mkListNFTPolicy)
import Scripts.PoolValidator (mkUnbondedPoolValidator)
import Settings
  ( unbondedStakingTokenName
  , confirmationTimeout
  , submissionAttempts
  )
import Types
  ( BurningAction(BurnHead, BurnOther, BurnSingle)
  , ListAction(ListRemove)
  , ScriptVersion
  , StakingType(Unbonded)
  )
import UnbondedStaking.Types
  ( Entry(Entry)
  , UnbondedPoolParams(UnbondedPoolParams)
  , UnbondedStakingAction(..)
  , UnbondedStakingDatum(AssetDatum, EntryDatum, StateDatum)
  )
import UnbondedStaking.Utils
  ( getAdminTime
  , getClosingTime
  , getUserOrBondingTime
  )
import Utils
  ( findRemoveOtherElem
  , getAssetsToConsume
  , getUtxoDatumHash
  , getUtxoWithNFT
  , hashPkh
  , logInfo_
  , mkAssetUtxosConstraints
  , mkOnchainAssocList
  , mustPayToScript
  , repeatUntilConfirmed
  )

-- | A collection of values needed to construct the constraints and
-- lookups for a withdrawal Tx.
type WithdrawalData =
  {
    -- General pool information
    assocList ::
      Array (ByteArray /\ TransactionInput /\ TransactionOutputWithRefScript)
  , poolUtxos :: UtxoMap
  , poolValidator :: Validator
  , listPolicy :: MintingPolicy
  , poolValidatorHash :: ValidatorHash
  , poolValidatorAddress :: Address
  , poolCs :: CurrencySymbol
  , poolTn :: TokenName
  , assetCs :: CurrencySymbol
  , assetTn :: TokenName
  -- User info
  , userPkh :: PaymentPubKeyHash
  , userSpkh :: Maybe StakePubKeyHash
  , userAddr :: Address
  , userKey :: ByteArray
  , entryInput :: TransactionInput
  , entryOutput :: TransactionOutputWithRefScript
  -- Specific Tx info
  , userSubmitted :: Boolean
  , burnEntryValue :: Value
  , assetsToConsume :: UtxoMap
  , withdrawChangeAmt :: BigInt
  , withdrawChangeValue :: Value
  , withdrawAmt :: BigInt
  , withdrawValue :: Value
  }

userWithdrawUnbondedPoolContract
  :: UnbondedPoolParams
  -> ScriptVersion
  -> Contract ()
       { txId :: String }
userWithdrawUnbondedPoolContract params scriptVersion = do
  addr <- liftedM
    "userWithdrawUnbondedPoolContract: could not get own wallet address"
    getWalletAddress
  userWithdrawUnbondedPoolContract' params scriptVersion addr true

adminWithdrawUnbondedPoolContract
  :: UnbondedPoolParams
  -> ScriptVersion
  -> Bech32String
  -> Contract ()
       { txId :: String }
adminWithdrawUnbondedPoolContract params scriptVersion bech32 = do
  addr <- addressFromBech32 bech32
  userWithdrawUnbondedPoolContract' params scriptVersion addr false

userWithdrawUnbondedPoolContract'
  :: UnbondedPoolParams
  -> ScriptVersion
  -> Address
  -> Boolean
  -> Contract ()
       { txId :: String }
userWithdrawUnbondedPoolContract' params scriptVersion userAddr iAmUser =
  repeatUntilConfirmed params confirmationTimeout submissionAttempts
    do
      -- Get withdrawal information
      d <- getWithdrawalData params scriptVersion userAddr iAmUser
      -- Get staking range
      userEntry <- unwrap <$> getEntryDatumFromOutput d.entryOutput
      logInfo' "userWithdrawUnbondedPoolContract: Getting user range..."
      { range } <- case unit of
        _
          | not iAmUser -> getAdminTime params scriptVersion
          | userEntry.open -> getUserOrBondingTime params scriptVersion
          | otherwise -> getClosingTime params scriptVersion
      -- Build base constraints and lookups
      utxos <-
        liftedM "userWithdrawUnbondedPoolContract: could not get wallet utxos"
          $ getWalletUtxos
      -- Get own payment pub key hash
      ownPkh <- liftedM
        "userWithdrawUnbondedPoolContract: could not get own PKH"
        ownPaymentPubKeyHash
      let
        assetDatum = Datum $ toData AssetDatum
        mustPayChange = mustPayToScript d.poolValidatorHash assetDatum
          d.withdrawChangeValue
        mustPayToUser = maybe (mustPayToPubKey d.userPkh d.withdrawValue)
          (\spkh -> mustPayToPubKeyAddress d.userPkh spkh d.withdrawValue)
          d.userSpkh

        baseConstraints :: TxConstraints Unit Unit
        baseConstraints = mconcat
          [ if d.withdrawChangeAmt > zero then mustPayChange
            else mempty
          , mustBeSignedBy ownPkh
          , mustPayToUser
          , mustValidateIn range
          ]

        baseLookups :: ScriptLookups.ScriptLookups PlutusData
        baseLookups = mconcat
          [ ScriptLookups.validator d.poolValidator
          , ScriptLookups.mintingPolicy d.listPolicy
          , ScriptLookups.unspentOutputs utxos
          , ScriptLookups.unspentOutputs d.poolUtxos
          ]
      -- Add constraints/lookups based on whether the entry is open or closed
      (constraints /\ lookups) <-
        if userEntry.open then mkOpenEntryConstraints d
        else mkClosedEntryConstraints d
      -- Build and submit transaction
      ubTx <-
        liftedE $ ScriptLookups.mkUnbalancedTx
          (baseLookups <> lookups)
          (baseConstraints <> constraints)
      pure { ubTx }

-- | Obtain the necessary data for constructing the user withdrawal's
-- constraints and lookups. With this info it's possible to do any kind of
-- withdrawal.
getWithdrawalData
  :: UnbondedPoolParams
  -> ScriptVersion
  -> Address
  -> Boolean
  -> Contract () WithdrawalData
getWithdrawalData
  params@
    ( UnbondedPoolParams
        { unbondedAssetClass
        , assocListCs
        , nftCs
        }
    )
  scriptVersion
  userAddr
  userSubmitted = do
  -- Get the unbonded pool validator hash and address
  poolValidator <-
    liftedE' "getWithdrawalData: Cannot create validator"
      $ mkUnbondedPoolValidator params scriptVersion
  let poolValidatorHash = validatorHash poolValidator
  logInfo_ "getWithdrawalData: validatorHash" poolValidatorHash
  let poolValidatorAddress = scriptHashAddress poolValidatorHash Nothing
  networkId <- getNetworkId
  logInfo_ "getWithdrawalData: Pool address"
    $ fromPlutusAddress networkId poolValidatorAddress

  -- Get the unbonded pool's currency symbol and token name
  let poolCs = nftCs
  poolTn <- liftContractM
    "getWithdrawalData: Could not get the unbonded staking token name"
    unbondedStakingTokenName

  -- Get the onchain list's minting policy
  listPolicy <- liftedE $ mkListNFTPolicy Unbonded scriptVersion nftCs

  -- Get the unbonded pool's utxo
  poolUtxos <- utxosAt poolValidatorAddress
  logInfo_ "getWithdrawalData: Pool UTxOs" poolUtxos

  -- Get asset UTxOs in unbonded pool
  logInfo'
    "getWithdrawalData: Getting unbonded assets in \
    \the pool..."
  unbondedAssetUtxos <- getUnbondedAssetUtxos poolUtxos
  logInfo_ "userWithdrawnUnbondedPoolContract: Unbonded Asset UTxOs"
    unbondedAssetUtxos

  -- Get the minting policy and currency symbol from the list NFT:
  -- listPolicy <- liftedE $ mkListNFTPolicy Unbonded scriptVersion nftCs

  -- Get the user's PKH, key and token
  userPkh <- case _.addressCredential $ unwrap userAddr of
    PubKeyCredential pkh -> pure $ wrap pkh
    _ -> throwContractError
      "getWithdrawalData: passed address is not a PubKey address"
  userKey <- liftAff $ hashPkh userPkh
  assocListTn <-
    liftContractM
      "getWithdrawalData: Could not create token name for user"
      $ mkTokenName userKey
  logInfo_ "getWithdrawalData: User's assoc list token name"
    assocListTn

  -- Get the user's (optional) stake pub key hash
  userSpkh <- runMaybeT do
    stakingCred' <- MaybeT $ pure $ _.addressStakingCredential $ unwrap userAddr
    case stakingCred' of
      StakingHash cred -> case cred of
        PubKeyCredential pkh -> pure $ wrap pkh
        _ -> lift $ throwContractError
          "getWithdrawalData: passed address is not a PubKey address"
      _ -> lift $ throwContractError
        "getWithdrawalData: passed address does not have a StakingHash credential"

  -- Get the onchain association list
  let assocList = mkOnchainAssocList assocListCs poolUtxos

  -- Get user entry utxo
  entryInput /\ entryOutput <-
    liftContractM
      "getWithdrawalData: Cannot get assocList utxo"
      $ getUtxoWithNFT poolUtxos assocListCs assocListTn
  userEntry <- unwrap <$> getEntryDatumFromOutput entryOutput
  logInfo_ "getWithdrawalData: entry to consume" userEntry

  -- Get staked asset's information
  let
    assetParams = unwrap unbondedAssetClass
    assetCs = assetParams.currencySymbol
    assetTn = assetParams.tokenName

  -- Calculate amount to withdraw
  let
    rewards :: Rational
    rewards = userEntry.rewards

    rewardsRounded :: BigInt
    rewardsRounded = numerator rewards / denominator rewards

    withdrawAmt :: BigInt
    withdrawAmt = userEntry.deposited + rewardsRounded

    withdrawValue :: Value
    withdrawValue = singleton assetCs assetTn withdrawAmt

  -- Obtain asset asset UTxOs to consume and change that needs to be returned
  -- to the pool
  assetsToConsume /\ withdrawChangeAmt <-
    liftContractM
      "mkWithdrawalData: Cannot get asset \
      \UTxOs to consume" $
      getAssetsToConsume
        unbondedAssetClass
        withdrawAmt
        unbondedAssetUtxos

  -- Collect all the obtained information for further use in constructing
  -- the constraints and lookups.
  pure
    { assocList
    , poolUtxos
    , poolValidator
    , listPolicy
    , poolValidatorHash
    , poolValidatorAddress
    , assetCs
    , assetTn
    , poolCs
    , poolTn
    , userAddr
    , userPkh
    , userSpkh
    , userKey
    , entryInput
    , entryOutput
    , burnEntryValue: singleton assocListCs assocListTn (-one)
    , userSubmitted
    , assetsToConsume
    , withdrawAmt
    , withdrawValue
    , withdrawChangeAmt
    , withdrawChangeValue:
        singleton
          assetCs
          assetTn
          withdrawChangeAmt
    }

-- | Build constraints for a closed entry withdrawal
mkClosedEntryConstraints
  :: WithdrawalData
  -> Contract () ((TxConstraints Unit Unit) /\ ScriptLookups PlutusData)
mkClosedEntryConstraints d = do
  let
    -- Build validator redeemer
    valRedeemer = Redeemer <<< toData $
      WithdrawAct
        { stakeHolder: d.userPkh
        , burningAction: BurnSingle d.entryInput
        }
    -- Build minting policy redeemer
    mintRedeemer =
      if d.userSubmitted then Redeemer $ toData $ ListRemove $ BurnSingle
        d.entryInput
      else Redeemer $ toData AdminAct

    constraints :: TxConstraints Unit Unit
    constraints =
      mconcat
        [ mustSpendScriptOutput d.entryInput valRedeemer
        , mkAssetUtxosConstraints d.assetsToConsume valRedeemer
        , mustMintValueWithRedeemer mintRedeemer d.burnEntryValue
        ]
  pure $ constraints /\ mempty

-- | Build constraints for an open entry withdrawal
mkOpenEntryConstraints
  :: WithdrawalData
  -> Contract () ((TxConstraints Unit Unit) /\ (ScriptLookups PlutusData))
mkOpenEntryConstraints d = do
  -- Get state utxo
  poolTxInput /\ poolTxOutput <-
    liftContractM
      "mkOpenEntryConstraints: Cannot get state utxo"
      $ getUtxoWithNFT d.poolUtxos d.poolCs d.poolTn
  logInfo'
    "mkOpenEntryConstraints: Getting head entry of the pool..."
  headEntry /\ _ <- getStateDatumFromOutput poolTxOutput
  logInfo_ "mkOpenEntryConstraints: Head entry of the pool" headEntry
  case headEntry of
    Nothing -> throwContractError
      "mkOpenEntryConstraints: no entries in the pool, expected at least one"
    Just headKey -> do
      logInfo' "mkOpenEntryConstraints: Found the head entry successfully"
      case compare d.userKey headKey of
        -- If hashedUserPkh < headKey, we are trying to withdraw a non-existent
        --  entry
        LT -> throwContractError
          "mkOpenEntryConstraints: entry key < head key (non existent)"
        -- If hashedUserPkh == key, we are trying to withdraw the first entry of
        --  the list
        EQ -> do
          logInfo' "mkOpenEntryConstraints: Compare EQ"
          firstEntryConstraints d poolTxInput
        -- If the hashed key is greater then  we must look at the assoc. list
        -- in more detail
        GT -> do
          logInfo' "mkOpenEntryConstraints: Compare GT"
          otherEntryConstraints d

firstEntryConstraints
  :: WithdrawalData
  -> TransactionInput
  -> Contract () ((TxConstraints Unit Unit) /\ (ScriptLookups PlutusData))
firstEntryConstraints d poolTxInput = do
  -- Get the datum of the head entry and the key of the new head
  logInfo'
    "firstEntryConstraints: getting datum of entry to\
    \consume (head)..."
  oldHeadEntry <- unwrap <$> getEntryDatumFromOutput d.entryOutput
  logInfo_ "firstEntryConstraints: entry to consume"
    oldHeadEntry
  let
    newHeadKey :: Maybe ByteArray
    newHeadKey = oldHeadEntry.next

    newState :: Datum
    newState = Datum <<< toData $
      StateDatum
        { maybeEntryName: newHeadKey
        , open: true
        }
    -- Build validator redeemer
    valRedeemer =
      if d.userSubmitted then Redeemer $ toData $
        WithdrawAct
          { stakeHolder: d.userPkh
          , burningAction: BurnHead poolTxInput d.entryInput
          }
      else Redeemer $ toData AdminAct
    -- Build minting policy redeemer
    mintRedeemer = Redeemer $ toData $ ListRemove $ BurnHead
      poolTxInput
      d.entryInput

    stateDatumLookup = ScriptLookups.datum newState
    stateTokenValue = singleton d.poolCs d.poolTn one

    constraints :: TxConstraints Unit Unit
    constraints =
      mconcat
        [ mustSpendScriptOutput poolTxInput valRedeemer
        , mustSpendScriptOutput d.entryInput valRedeemer
        , mkAssetUtxosConstraints d.assetsToConsume valRedeemer
        , mustMintValueWithRedeemer mintRedeemer d.burnEntryValue
        , mustPayToScript d.poolValidatorHash newState stateTokenValue
        ]
  pure $ constraints /\ stateDatumLookup

otherEntryConstraints
  :: WithdrawalData
  -> Contract () ((TxConstraints Unit Unit) /\ (ScriptLookups PlutusData))
otherEntryConstraints d = do
  -- The hashed key is greater than so we must look at the assoc. list
  -- in more detail
  { firstInput, secondInput }
    /\ { firstOutput, secondOutput }
    /\ _ <-
    liftContractM
      "userWithdrawUnbondedPoolContract: Cannot get position in Assoc. \
      \List"
      $ findRemoveOtherElem d.assocList d.userKey
  -- Get the entry datum of the previous entry
  logInfo'
    "userWithdrawUnbondedPoolContract: getting datum of previous\
    \entry..."
  prevEntry <- unwrap <$> getEntryDatumFromOutput firstOutput
  logInfo_ "userWithdrawUnbondedPoolContract: entry to consume"
    prevEntry
  -- Get the entry datum of the entry to consume
  logInfo'
    "userWithdrawUnbondedPoolContract: getting datum of entry to\
    \ burn..."
  burnEntry <- unwrap <$> getEntryDatumFromOutput secondOutput
  logInfo_ "userWithdrawUnbondedPoolContract: entry to consume"
    burnEntry
  let
    -- Build updated previous entry and its lookup
    prevEntryUpdated = Datum $ toData $ EntryDatum
      { entry: Entry $ prevEntry
          { next = burnEntry.next
          }
      }
  -- Build validator redeemer
  let
    prevEntryDatumLookup = ScriptLookups.datum prevEntryUpdated
    valRedeemer =
      if d.userSubmitted then Redeemer $ toData $
        WithdrawAct
          { stakeHolder: d.userPkh
          , burningAction: BurnOther firstInput secondInput
          }
      else Redeemer $ toData AdminAct

    mintRedeemer = Redeemer $ toData $ ListRemove $ BurnOther
      firstInput
      secondInput

    prevTxOutput :: TransactionOutput
    prevTxOutput = (unwrap firstOutput).output

    prevTxValue :: Value
    prevTxValue = (unwrap prevTxOutput).amount

    constraints :: TxConstraints Unit Unit
    constraints =
      mconcat
        [ mustSpendScriptOutput firstInput valRedeemer
        , mustSpendScriptOutput secondInput valRedeemer
        , mkAssetUtxosConstraints d.assetsToConsume valRedeemer
        , mustMintValueWithRedeemer mintRedeemer d.burnEntryValue
        , mustPayToScript d.poolValidatorHash prevEntryUpdated prevTxValue
        ]
  pure $ constraints /\ prevEntryDatumLookup

-- | This function filters all the asset UTxOs from a `UtxoMap`
getUnbondedAssetUtxos :: forall (r :: Row Type). UtxoMap -> Contract r UtxoMap
getUnbondedAssetUtxos utxos = do
  assetUtxos <- catMaybes <$> for utxoAssocList \utxo@(_ /\ txOutput) -> do
    datumHash <- liftContractM "getAssetUtxos: could not get datum hash"
      $ getUtxoDatumHash txOutput
    datum <-
      liftContractM "getAssetUtxos: could not get datum"
        =<< getDatumByHash datumHash
    unbondedDatum :: UnbondedStakingDatum <-
      liftContractM
        "getAssetUtxos: could not parse datum as an unbonded staking \
        \datum" $ fromData (unwrap datum)
    case unbondedDatum of
      AssetDatum -> pure $ Just utxo
      _ -> pure Nothing
  pure $ Map.fromFoldable assetUtxos
  where
  utxoAssocList :: Array (TransactionInput /\ TransactionOutputWithRefScript)
  utxoAssocList = Map.toUnfoldable utxos

-- | Get entry datum from transaction output
getEntryDatumFromOutput
  :: forall (r :: Row Type). TransactionOutputWithRefScript -> Contract r Entry
getEntryDatumFromOutput txOut = do
  unbondedDatum <- getUnbondedDatum txOut
  case unbondedDatum of
    EntryDatum { entry: e } -> pure e
    _ -> throwContractError
      "getEntryDatumFromOutput: datum is not of Entry \
      \type"

-- | Get state datum from transaction output
getStateDatumFromOutput
  :: forall (r :: Row Type)
   . TransactionOutputWithRefScript
  -> Contract r (Tuple (Maybe ByteArray) Boolean)
getStateDatumFromOutput txOut = do
  unbondedDatum <- getUnbondedDatum txOut
  case unbondedDatum of
    StateDatum { maybeEntryName: key, open: isOpen } -> pure $ (key /\ isOpen)
    _ -> throwContractError
      "getStateDatumFromOutput: datum is not of State \
      \type"

-- | Gets an unbonded datum from a transaction output
getUnbondedDatum
  :: forall (r :: Row Type)
   . TransactionOutputWithRefScript
  -> Contract r UnbondedStakingDatum
getUnbondedDatum =
  liftContractM
    "getUnbondedDatum: could not parse datum as unbonded staking datum"
    <<< fromData
    <<< unwrap
    <=< liftContractM "getUnbondedDatum: could not get datum"
    <=< getDatumByHash
    <=< liftContractM "getUnbondedDatum: could not get datum hash"
    <<< getUtxoDatumHash
