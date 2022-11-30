{-# LANGUAGE UndecidableInstances #-}

module UnbondedStaking.UnbondedPool (
  punbondedPoolValidator,
  punbondedPoolValidatorUntyped,
) where

import Control.Monad ((<=<))
import GHC.Records (getField)
import InductiveLogic (
  doesNotConsumeUnbondedAssetGuard,
  hasEntryToken,
  hasListNft,
  hasStateToken,
  pointsNowhere,
  pointsTo,
 )

import Plutarch.Api.V1 (
  PAddress,
  PDatumHash,
  PMaybeData (PDJust, PDNothing),
  PPubKeyHash,
  PScriptContext,
  PScriptPurpose,
  PTokenName (PTokenName),
  PTuple,
  PTxInInfo,
  PTxOut,
  PTxOutRef,
 )
import Plutarch.Api.V1.Scripts (PDatum)
import Plutarch.Builtin (pforgetData)
import Plutarch.Crypto (pblake2b_256)
import Plutarch.DataRepr (HRec)
import Plutarch.Unsafe (punsafeCoerce)

import PInterval (
  getUnbondedBondingPeriodIncrement,
  getUnbondedPeriod,
 )
import PNatural (
  PNatRatio,
  PNatural,
  mkNatRatioUnsafe,
  natOne,
  natPow,
  natZero,
  ratZero,
  roundDown,
  roundUp,
  toNatRatio,
  (#*),
  (#+),
  (#-),
 )
import PTypes (
  HField,
  PAssetClass,
  PBurningAction (PBurnHead, PBurnOther, PBurnSingle),
  PMintingAction (PMintEnd, PMintHead, PMintInBetween),
  PPeriod (BondingPeriod, DepositWithdrawPeriod),
  PTxInInfoFields,
  PTxInInfoHRec,
  PTxInfoFields,
  PTxInfoHRec,
  adminUpdatePeriod,
  bondingPeriod,
  depositWithdrawPeriod,
  passetClass,
 )

import SingularityNet.Settings (unbondedStakingTokenName)

import UnbondedStaking.PTypes (
  PBoolData,
  PEntry,
  PEntryFields,
  PEntryHRec,
  PUnbondedPoolParams,
  PUnbondedPoolParamsFields,
  PUnbondedPoolParamsHRec,
  PUnbondedStakingAction (
    PAdminAct,
    PCloseAct,
    PStakeAct,
    PWithdrawAct
  ),
  PUnbondedStakingDatum (PAssetDatum, PEntryDatum, PStateDatum),
 )
import Utils (
  getCoWithDatum,
  getContinuingOutputWithNFT,
  getDatum,
  getDatumHash,
  getInput,
  getOutputSignedBy,
  getTokenCount,
  getTokenName,
  oneWith,
  parseStakingDatum,
  peq,
  pfalse,
  pfind,
  pguardC,
  pletC,
  pnestedIf,
  ptrue,
  ptryFromUndata,
  punit,
  signedBy,
  signedOnlyBy,
  toPBool,
  (>:),
 )

{- The validator has two responsibilities

     1. Make sure that the list's inductive conditions are preserved

     2. Do business logic checks, which means validating the datums of the
     inputs and outputs (and therefore validate the correct outputs are
     produced)

   Some basic conditions are checked by the minting policy (see `ListNFT`), so
   the validator needs to make sure that a value was minted to be able to assume
   them.

   Specifically, the minting policy makes sure that the correct inputs are
   consumed/burnt (e.g: in a list insertion, only two list entries are consumed
   and no other NFT. Also, the minted entry's `TokenName` matches the
   signatory's PKH).
-}
punbondedPoolValidator ::
  forall (s :: S).
  Term
    s
    ( PUnbondedPoolParams
        :--> PUnbondedStakingDatum
        :--> PUnbondedStakingAction
        :--> PScriptContext
        :--> PUnit
    )
punbondedPoolValidator = phoistAcyclic $
  plam $ \params dat act ctx -> unTermCont $ do
    -- Retrieve fields from parameters
    ctxF <- tcont $ pletFields @'["txInfo", "purpose"] ctx
    txInfoF <- tcont $ pletFields @PTxInfoFields ctxF.txInfo
    paramsF <- tcont $ pletFields @PUnbondedPoolParamsFields params
    let period = getUnbondedPeriod # txInfoF.validRange # params
    -- Match on redeemer, check period and minted value, execute the
    -- corresponding logic
    pure $
      pmatch act $ \case
        PAdminAct _ -> unTermCont $ do
          pguardC
            "punbondedPoolValidator: wrong period for PAdminAct \
            \redeemer"
            $ period #== adminUpdatePeriod
          pure $ adminLogic txInfoF paramsF
        PStakeAct dataRecord -> unTermCont $ do
          pguardC
            "punbondedPoolValidator: wrong period for PStakeAct \
            \redeemer"
            $ period #== depositWithdrawPeriod
          stakeActParamsF <-
            tcont $
              pletFields
                @["stakeAmount", "pubKeyHash", "maybeMintingAction"]
                dataRecord
          pure $
            stakeActLogic
              txInfoF
              paramsF
              ctxF.purpose
              dat
              stakeActParamsF
        PWithdrawAct dataRecord -> unTermCont $ do
          -- Period validation is done inside of the redeemer action
          withdrawActParamsF <-
            tcont $ pletFields @["pubKeyHash", "burningAction"] dataRecord
          withdrawActLogic
            txInfoF
            paramsF
            ctxF.purpose
            dat
            withdrawActParamsF
            period
        PCloseAct _ -> unTermCont $ do
          pguardC
            "punbondedPoolValidator: wrong period for PCloseAct \
            \redeemer"
            $ period #== adminUpdatePeriod
          pure $ adminLogic txInfoF paramsF

-- Untyped version to be serialised. This version is responsible for verifying
-- that the parameters (pool params, datum and redeemer) have the proper types.
-- The script context should always be safe.
punbondedPoolValidatorUntyped ::
  forall (s :: S).
  Term
    s
    ( PData
        :--> PData
        :--> PData
        :--> PData
        :--> PUnit
    )
punbondedPoolValidatorUntyped = plam $ \pparams dat act ctx ->
  punbondedPoolValidator
    # unTermCont (ptryFromUndata pparams)
    # unTermCont (ptryFromUndata dat)
    # unTermCont (ptryFromUndata act)
    # punsafeCoerce ctx

adminLogic ::
  forall (s :: S).
  PTxInfoHRec s ->
  PUnbondedPoolParamsHRec s ->
  Term s PUnit
adminLogic txInfoF paramsF = unTermCont $ do
  -- We check that the transaction was signed by the pool operator
  pguardC "adminLogic: transaction not signed by admin" $
    signedBy txInfoF.signatories paramsF.admin
  pure punit

stakeActLogic ::
  forall (s :: S).
  PTxInfoHRec s ->
  PUnbondedPoolParamsHRec s ->
  Term s PScriptPurpose ->
  Term s PUnbondedStakingDatum ->
  HRec
    '[ HField s "stakeAmount" PNatural
     , HField s "pubKeyHash" PPubKeyHash
     , HField s "maybeMintingAction" (PMaybeData PMintingAction)
     ] ->
  Term s PUnit
stakeActLogic txInfoF paramsF purpose datum stakeActParamsF = unTermCont $ do
  -- Get the input being spent
  spentInput <-
    tcont . pletFields @PTxInInfoFields
      =<< getInput purpose txInfoF.inputs
  let poolAddr :: Term s PAddress
      poolAddr = pfield @"address" # spentInput.resolved
  -- Check that input spent is not an asset UTXO (user cannot withdraw)
  doesNotConsumeUnbondedAssetGuard datum
  -- Validate holder's signature
  pguardC "stakeActLogic: tx not exclusively signed by the stake-holder" $
    signedOnlyBy txInfoF.signatories stakeActParamsF.pubKeyHash
  -- Check that amount is positive
  let stakeAmt :: Term s PNatural
      stakeAmt = pfromData stakeActParamsF.stakeAmount
  pguardC "stakeActLogic: stake amount is not positive or within bounds" $
    natZero #<= stakeAmt
  -- Get asset output of the transaction (new locked stake)
  assetOutput <-
    (tcont . pletFields @'["value"])
      =<< fmap
        fst
        ( getCoWithDatum
            poolAddr
            isAssetDatum
            txInfoF.outputs
            $ getField @"data" txInfoF
        )
  -- Check that the correct amount of the assetclass is locked
  unbondedAsset <-
    tcont . pletFields @'["currencySymbol", "tokenName"] $
      paramsF.unbondedAssetClass
  pguardC "stakeActLogic: amount deposited does not match redeemer's amount" $
    oneWith # (peq # unbondedAsset.currencySymbol)
      # (peq # unbondedAsset.tokenName)
      # (peq #$ pto $ pfromData stakeActParamsF.stakeAmount)
      # assetOutput.value
  pure . pmatch stakeActParamsF.maybeMintingAction $ \case
    -- If some minting action is provided, this is a new stake and inductive
    -- conditions must be checked
    PDJust mintAct -> unTermCont $ do
      -- Check that minted value is a list entry (minting policy is run)
      pguardC "stakeActLogic: failure when checking minted value in minting tx" $
        hasListNft paramsF.assocListCs txInfoF.mint
      -- Check inductive conditions and business logic
      newStakeLogic
        txInfoF
        paramsF
        spentInput
        datum
        stakeActParamsF.pubKeyHash
        stakeActParamsF.stakeAmount
        $ pfield @"_0" # mintAct
    -- If no minting action is provided, this is a stake update
    PDNothing _ -> unTermCont $ do
      -- A list token should *not* be minted
      pguardC
        "stakeActLogic: failure when checking minted value in non-minting \
        \ tx"
        $ pnot #$ hasListNft paramsF.assocListCs txInfoF.mint
      -- Check business logic
      updateStakeLogic
        txInfoF
        paramsF
        spentInput
        datum
        stakeActParamsF.stakeAmount
        stakeActParamsF.pubKeyHash
  where
    isAssetDatum :: Term s (PDatum :--> PBool)
    isAssetDatum = plam $ \dat' -> unTermCont $ do
      dat <-
        ptryFromUndata @PUnbondedStakingDatum
          . pforgetData
          . pdata
          $ dat'
      pure . pmatch dat $ \case
        PAssetDatum _ -> ptrue
        _ -> pfalse

-- This function validates the update of a an already existing entry in the list
updateStakeLogic ::
  forall (s :: S).
  PTxInfoHRec s ->
  PUnbondedPoolParamsHRec s ->
  PTxInInfoHRec s ->
  Term s PUnbondedStakingDatum ->
  Term s PNatural ->
  Term s PPubKeyHash ->
  TermCont s (Term s PUnit)
updateStakeLogic txInfoF paramsF spentInputF datum stakeAmt holderPkh = do
  -- Construct some useful values for later
  stakeHolderKey <- pletC $ pblake2b_256 # pto holderPkh
  stakeHolderTn <- pletC $ pcon $ PTokenName $ stakeHolderKey
  spentInputResolved <-
    tcont . pletFields @'["address", "value", "datumHash"] $
      spentInputF.resolved
  let poolAddr :: Term s PAddress
      poolAddr = spentInputResolved.address
      txInfoData = getField @"data" txInfoF
  newEntryTok <- pletC $ passetClass # paramsF.assocListCs # stakeHolderTn
  ---- FETCH DATUMS ----
  oldEntryF <- tcont . pletFields @PEntryFields =<< getEntryData datum
  newEntryF <- getOutputEntry poolAddr newEntryTok txInfoData txInfoF.outputs
  ---- BUSINESS LOGIC ----
  pguardC "updateStakeLogic: update failed because pool is not open" $
    toPBool # oldEntryF.open
      #&& toPBool # newEntryF.open
  pguardC "updateStakeLogic: spent entry's key does not match user's key" $
    oldEntryF.key #== stakeHolderKey
  pguardC "updateStakeLogic: new entry does not have the stakeholder's key" $
    newEntryF.key #== stakeHolderKey
  pguardC "updateStakeLogic: incorrect update of newDeposit" $
    newEntryF.newDeposit #== oldEntryF.newDeposit #+ stakeAmt
  pguardC "updateStakeLogic: incorrect update of deposit" $
    newEntryF.deposited #== oldEntryF.deposited #+ stakeAmt
  pguardC "updateStakeLogic: update increases stake beyond allowed bounds" $
    pfromData paramsF.minStake #<= newEntryF.deposited
      #&& pfromData newEntryF.deposited #<= paramsF.maxStake
  pguardC
    "updateStakeLogic: update should not change rewards, totalRewards, \
    \  totalDeposited, or next fields"
    $ oldEntryF.rewards #== newEntryF.rewards
      #&& oldEntryF.totalRewards #== newEntryF.totalRewards
      #&& oldEntryF.totalDeposited #== newEntryF.totalDeposited
      #&& oldEntryF.next #== newEntryF.next
  pure punit

{- | This function checks all inductive conditions and makes all necessary
 business logic validation on the state/entry updates and new entries
-}
newStakeLogic ::
  forall (s :: S).
  PTxInfoHRec s ->
  PUnbondedPoolParamsHRec s ->
  PTxInInfoHRec s ->
  Term s PUnbondedStakingDatum ->
  Term s PPubKeyHash ->
  Term s PNatural ->
  Term s PMintingAction ->
  TermCont s (Term s PUnit)
newStakeLogic txInfoF paramsF spentInputF datum holderPkh stakeAmt mintAct = do
  -- Construct some useful values for later
  stakeHolderKey <- pletC $ pblake2b_256 # pto holderPkh
  stakeHolderTn <- pletC $ pcon $ PTokenName $ stakeHolderKey
  spentInputResolvedF <-
    tcont . pletFields @'["address", "value", "datumHash"] $
      spentInputF.resolved
  let poolAddr :: Term s PAddress
      poolAddr = spentInputResolvedF.address
      stateTn :: Term s PTokenName
      stateTn = pconstant unbondedStakingTokenName
  stateTok <- pletC $ passetClass # paramsF.nftCs # stateTn
  newEntryTok <- pletC $ passetClass # paramsF.assocListCs # stakeHolderTn
  txInfoData <- pletC $ getField @"data" txInfoF
  pure . pmatch mintAct $ \case
    PMintHead stateOutRef -> unTermCont $ do
      ---- FETCH DATUMS ----
      -- Get datum for next state
      nextStateTxOut <-
        getContinuingOutputWithNFT poolAddr stateTok txInfoF.outputs
      nextStateHash <- getDatumHash nextStateTxOut
      (nextEntryKey, nextOpen) <-
        getStateData
          =<< parseStakingDatum
          =<< getDatum nextStateHash txInfoData
      -- Get datum for current state
      (entryKey, open) <- getStateData datum
      -- Get datum for new list entry
      newEntryF <-
        getOutputEntry poolAddr newEntryTok txInfoData txInfoF.outputs
      ---- BUSINESS LOGIC ----
      pguardC "newStakeLogic: update failed because pool state is not open" $
        toPBool # open
          #&& toPBool # nextOpen
      newEntryGuard paramsF newEntryF stakeAmt stakeHolderKey
      ---- INDUCTIVE CONDITIONS ----
      -- Validate that spentOutRef is the state UTXO and matches redeemer
      pguardC "newStakeLogic (mintHead): spent input is not the state UTXO" $
        hasStateToken
          spentInputResolvedF.value
          (paramsF.nftCs, pconstant unbondedStakingTokenName)
      pguardC
        "newStakeLogic (mintHead): spent input does not match redeemer \
        \input"
        $ spentInputF.outRef #== pfield @"_0" # stateOutRef
      -- Validate next state
      pguardC
        "newStakeLogic (mintHead): next pool state does not point to new \
        \ entry"
        $ nextEntryKey `pointsTo` newEntryF.key
      -- Validate list order and links
      pure . pmatch entryKey $ \case
        -- We are shifting the current head forwards
        PDJust currentEntryKey' -> unTermCont $ do
          currentEntryKey <-
            pletC $ pfromData $ pfield @"_0" # currentEntryKey'
          -- Validate order of entries
          pguardC
            "newStakeLogic (mintInBetween): new entry's key should be \
            \strictly less than current entry"
            $ pfromData newEntryF.key #< currentEntryKey
          -- The new entry should point to the current entry
          pguardC
            "newStakeLogic (mintInBetween): new entry should point to \
            \current entry"
            $ newEntryF.next `pointsTo` currentEntryKey
          pure punit
        -- This is the first stake of the pool
        PDNothing _ -> unTermCont $ do
          -- The new entry should *not* point to anything
          pguardC
            "newStakeLogic (mintInBetween): new entry should not point to \
            \anything"
            $ pointsNowhere newEntryF.next
          pure punit
    PMintInBetween outRefs -> unTermCont $ do
      ---- FETCH DATUMS ----
      entriesRefsF <-
        tcont $ pletFields @'["previousEntry", "currentEntry"] outRefs
      -- Get datum for prevEntry
      prevEntryF <- tcont . pletFields @PEntryFields =<< getEntryData datum
      -- Get datum for prevEntryUpdated
      let prevEntryTok :: Term s PAssetClass
          prevEntryTok =
            getTokenName
              paramsF.assocListCs
              spentInputResolvedF.value
      prevEntryUpdatedF <-
        getOutputEntry poolAddr prevEntryTok txInfoData txInfoF.outputs
      -- Get datum for new list entry
      newEntryF <-
        getOutputEntry poolAddr newEntryTok txInfoData txInfoF.outputs
      -- Get the current entry's key
      currEntryKey <- pure . pmatch prevEntryF.next $ \case
        PDJust key -> pfield @"_0" # key
        PDNothing _ ->
          ptraceError
            "newStakeLogic (mintEnd): the previous \
            \ entry does not point to another entry"
      ---- BUSINESS LOGIC ----
      pguardC "newStakeLogic: update failed because pool state is not open" $
        toPBool # prevEntryF.open
          #&& toPBool # newEntryF.open
      -- Validate initialization of new entry
      newEntryGuard paramsF newEntryF stakeAmt stakeHolderKey
      -- Previous entry should keep the same values when updated
      equalEntriesGuard prevEntryF prevEntryUpdatedF
      ---- INDUCTIVE CONDITIONS ----
      -- Validate that previousEntry is a list entry and matches redeemer
      pguardC "newStakeLogic (mintEnd): spent input is not an entry" $
        hasListNft paramsF.assocListCs spentInputResolvedF.value
      pguardC
        "newStakeLogic (mintEnd): spent input is not the same as input in \
        \ redeemer"
        $ spentInputF.outRef #== entriesRefsF.previousEntry
      -- Previous entry should now point to the new entry
      pguardC
        "newStakeLogic (mintEnd): the previous entry should point to the \
        \new entry"
        $ prevEntryUpdatedF.next `pointsTo` newEntryF.key
      -- And new entry should point to the current entry
      pguardC
        "newStakeLogic (mintEnd): the new entry should point to the \
        \current entry"
        $ newEntryF.next `pointsTo` currEntryKey
      -- Validate entries' order
      pguardC
        "newStakeLogic (mintEnd): failed to validate order in previous, \
        \current and new entry"
        $ pfromData prevEntryF.key #< newEntryF.key
          #&& newEntryF.key #< currEntryKey
      pure punit
    PMintEnd listEndOutRef' -> unTermCont $ do
      ---- FETCH DATUMS ----
      listEndOutRef <- pletC $ pfield @"_0" # listEndOutRef'
      -- Get datum for endEntry
      endEntryF <- tcont . pletFields @PEntryFields =<< getEntryData datum
      -- Get datum for endEntryUpdated
      let endEntryTok :: Term s PAssetClass
          endEntryTok =
            getTokenName
              paramsF.assocListCs
              spentInputResolvedF.value
      endEntryUpdated <-
        getOutputEntry poolAddr endEntryTok txInfoData txInfoF.outputs
      -- Get datum for new list entry
      newEntryF <-
        getOutputEntry poolAddr newEntryTok txInfoData txInfoF.outputs
      ---- BUSINESS LOGIC ----
      pguardC "newStakeLogic: update failed because pool state is not open" $
        toPBool # endEntryF.open
          #&& toPBool # newEntryF.open
      -- Validate initialization of new entry
      newEntryGuard paramsF newEntryF stakeAmt stakeHolderKey
      -- End entry should keep the same values when updated
      equalEntriesGuard endEntryF endEntryUpdated
      ---- INDUCTIVE CONDITIONS ----
      -- Validate that endEntry is a list entry and matches redeemer
      pguardC "newStakeLogic (mintEnd): spent input is not an entry" $
        hasListNft paramsF.assocListCs spentInputResolvedF.value
      pguardC
        "newStakeLogic (mintEnd): spent input is not the same as input in \
        \redeemer"
        $ spentInputF.outRef #== listEndOutRef
      -- End entry should point nowhere
      pguardC "newStakeLogic (mintEnd): end should point nowhere" $
        pointsNowhere endEntryF.next
      -- Updated end entry (no longer end) should point to new entry
      pguardC
        "newStakeLogic (mintEnd): updated end should point to new end \
        \entry"
        $ endEntryUpdated.next `pointsTo` newEntryF.key
      -- New entry (new end) should point nowhere
      pguardC "newStakeLogic (mintEnd): new end entry should not point anywhere" $
        pointsNowhere newEntryF.next
      -- Validate entries' order
      pguardC
        "newStakeLogic (mintEnd): new entry's key should come after end \
        \entry"
        $ pfromData endEntryUpdated.key #< pfromData newEntryF.key
      pure punit

withdrawActLogic ::
  forall (s :: S).
  PTxInfoHRec s ->
  PUnbondedPoolParamsHRec s ->
  Term s PScriptPurpose ->
  Term s PUnbondedStakingDatum ->
  HRec
    '[ HField s "pubKeyHash" PPubKeyHash
     , HField s "burningAction" PBurningAction
     ] ->
  Term s PPeriod ->
  TermCont s (Term s PUnit)
withdrawActLogic txInfoF paramsF purpose datum withdrawActParamsF period = do
  -- Construct some useful values for later
  holderKey <- pure $ pblake2b_256 # pto (pfromData withdrawActParamsF.pubKeyHash)
  entryTn <- pletC . pcon . PTokenName $ holderKey
  -- Get the input being spent
  spentInputF <-
    tcont . pletFields @PTxInInfoFields
      =<< getInput purpose txInfoF.inputs
  spentInputResolvedF <- tcont . pletFields @'["value"] $ spentInputF.resolved
  -- Validate holder's signature
  pguardC "withdrawActLogic: tx not exclusively signed by the stake-holder" $
    signedOnlyBy txInfoF.signatories withdrawActParamsF.pubKeyHash
  -- Get amount staked from output
  withdrawnAmt <-
    pure . getTokenCount paramsF.unbondedAssetClass
      <=< (\txOut -> pure $ pfield @"value" # txOut)
      <=< getOutputSignedBy withdrawActParamsF.pubKeyHash
      $ txInfoF.outputs
  -- Validate the asset input is effectively an asset UTXO
  let assetCheck :: Term s PUnit
      assetCheck = unTermCont $ do
        datum <-
          parseStakingDatum @PUnbondedStakingDatum
            <=< flip getDatum (getField @"data" txInfoF)
            <=< getDatumHash
            $ spentInputF.resolved
        pure $
          pmatch datum $ \case
            PAssetDatum _ -> punit
            _ -> ptraceError "withdrawActLogic: expected asset input"
      -- We validate the entry when consuming it
      entryCheck :: Term s PTxOutRef -> Term s PUnit
      entryCheck entryOutRef = unTermCont $ do
        pguardC
          "withdrawActLogic: spent entry is not an entry (no List \
          \NFT)"
          $ hasListNft paramsF.assocListCs spentInputResolvedF.value
        pguardC
          "withdrawActLogic: spent entry does not match redeemer \
          \TxOutRef"
          $ pdata entryOutRef #== spentInputF.outRef
        pguardC "withdrawActLogic: entry does not belong to user" $
          hasEntryToken
            spentInputResolvedF.value
            (paramsF.assocListCs, entryTn)
        pure punit
  -- Check business and inductive conditions depending on redeemer
  pure . pmatch (pfromData withdrawActParamsF.burningAction) $ \case
    PBurnHead outRefs' -> unTermCont $ do
      outRefsF <- tcont . pletFields @'["state", "headEntry"] $ outRefs'
      -- We check most conditions when consuming the state UTXO
      let withdrawHeadCheck =
            withdrawHeadActLogic
              spentInputF
              withdrawnAmt
              datum
              txInfoF
              paramsF
              period
              outRefsF.state
              outRefsF.headEntry
      pure $
        pnestedIf
          [ spentInputF.outRef #== outRefsF.state >: withdrawHeadCheck
          , spentInputF.outRef #== outRefsF.headEntry >: entryCheck outRefsF.headEntry
          ]
          assetCheck
    PBurnOther entries' -> unTermCont $ do
      entriesF <- tcont . pletFields @'["previousEntry", "burnEntry"] $ entries'
      let withdrawOtherCheck :: Term s PUnit
          withdrawOtherCheck =
            withdrawOtherActLogic
              spentInputF
              withdrawnAmt
              datum
              txInfoF
              paramsF
              period
              entriesF.burnEntry
      pure $
        pnestedIf
          [ spentInputF.outRef #== entriesF.previousEntry >: withdrawOtherCheck
          , spentInputF.outRef #== entriesF.burnEntry >: entryCheck entriesF.burnEntry
          ]
          $ assetCheck
    PBurnSingle entry' -> unTermCont $ do
      ---- FETCH DATUMS ----
      -- Get datum for entry
      let entryOutRef = pfield @"burnEntry" # entry'
      txInfoDataF <- pletC $ getField @"data" txInfoF
      entryF <- getInputEntry entryOutRef txInfoDataF txInfoF.inputs
      -- Burning action only valid if pool is closed
      pguardC "withdrawActLogic: Pool is not closed" $
        pnot # (toPBool # entryF.open)
      -- Validate period
      pguardC "withdrawActLogic: wrong period for PWithdrawAct redeemer" $
        period #== depositWithdrawPeriod
          #|| period #== adminUpdatePeriod
          #|| period #== bondingPeriod
      ---- BUSINESS LOGIC ----
      -- Validate that entry key matches the key in state UTxO
      pguardC "withdrawActLogic: consumed entry key does not match user's pkh" $
        entryF.key #== holderKey
      -- Validate withdrawn amount
      pguardC
        "withdrawActLogic: withdrawn amount does not match stake and \
        \rewards"
        $ withdrawnAmt
          #== roundDown (toNatRatio entryF.deposited #+ entryF.rewards)
      pure $
        pnestedIf
          [spentInputF.outRef #== pdata entryOutRef >: entryCheck entryOutRef]
          $ assetCheck

withdrawHeadActLogic ::
  forall (s :: S).
  HRec
    '[ HField s "outRef" PTxOutRef
     , HField s "resolved" PTxOut
     ] ->
  Term s PNatural ->
  Term s PUnbondedStakingDatum ->
  PTxInfoHRec s ->
  PUnbondedPoolParamsHRec s ->
  Term s PPeriod ->
  Term s PTxOutRef ->
  Term s PTxOutRef ->
  Term s PUnit
withdrawHeadActLogic spentInputF withdrawnAmt datum txInfoF paramsF period stateOutRef headEntryOutRef =
  unTermCont $ do
    -- Construct some useful values for later
    spentInputResolvedF <-
      tcont . pletFields @'["address", "value", "datumHash"] $ spentInputF.resolved
    let poolAddr :: Term s PAddress
        poolAddr = spentInputResolvedF.address
        stateTn :: Term s PTokenName
        stateTn = pconstant unbondedStakingTokenName
    stateTok <- pletC $ passetClass # paramsF.nftCs # stateTn
    txInfoDataF <- pletC $ getField @"data" txInfoF
    ---- FETCH DATUMS ----
    -- Get datum for next state
    nextStateTxOut <-
      getContinuingOutputWithNFT poolAddr stateTok txInfoF.outputs
    nextStateHash <- getDatumHash nextStateTxOut
    (nextEntryKey, nextEntryOpen) <-
      getStateData
        =<< parseStakingDatum
        =<< getDatum nextStateHash txInfoDataF
    -- Get datum for current state
    entryKey <- getKey =<< fst <$> getStateData datum
    -- Get datum for head entry
    headEntryF <- getInputEntry headEntryOutRef txInfoDataF txInfoF.inputs
    ---- BUSINESS LOGIC ----
    -- Burning action only valid if pool is open
    pguardC "withdrawHeadActLogic: Pool is not open" $ toPBool # headEntryF.open
    -- Validate that entry key matches the key in state UTxO
    pguardC "withdrawHeadActLogic: consumed entry key does not match user's pkh" $
      headEntryF.key #== entryKey
    -- Validate withdrawn amount
    withdrawRewardsGuard period withdrawnAmt paramsF txInfoF headEntryF
    ---- INDUCTIVE CONDITIONS ----
    -- Validate that spentOutRef is the state UTXO and matches redeemer
    pguardC "withdrawHeadActLogic: spent input is not the state UTXO" $
      spentInputResolvedF.value
        `hasStateToken` (paramsF.nftCs, pconstant unbondedStakingTokenName)
    pguardC "withdrawHeadActLogic: spent input does not match redeemer input" $
      spentInputF.outRef #== pdata stateOutRef
    -- Validate that consumed entry is head of the list
    pguardC "withdrawHeadActLogic: spent entry is not head of the list" $
      entryKey #== headEntryF.key
    -- Validate next state
    pguardC
      "withdrawHeadActLogic: next pool state does not point to same \
      \location as burned entry"
      $ pdata nextEntryKey #== headEntryF.next
    pguardC
      "withdrawHeadActLogic: next pool state cannot close pool"
      $ pdata nextEntryOpen #== headEntryF.open
    pure punit

withdrawOtherActLogic ::
  forall (s :: S).
  HRec
    '[ HField s "outRef" PTxOutRef
     , HField s "resolved" PTxOut
     ] ->
  Term s PNatural ->
  Term s PUnbondedStakingDatum ->
  PTxInfoHRec s ->
  PUnbondedPoolParamsHRec s ->
  Term s PPeriod ->
  Term s PTxOutRef ->
  Term s PUnit
withdrawOtherActLogic spentInputF withdrawnAmt datum txInfoF paramsF period burnEntryOutRef =
  unTermCont $ do
    -- Construct some useful values for later
    spentInputResolvedF <-
      tcont . pletFields @'["address", "value", "datumHash"] $ spentInputF.resolved
    let poolAddr :: Term s PAddress
        poolAddr = spentInputResolvedF.address
    txInfoData <- pletC $ getField @"data" txInfoF
    ---- FETCH DATUMS ----
    -- Get datum for previous entry
    prevEntryF <- tcont . pletFields @PEntryFields =<< getEntryData datum
    -- Get updated datum for previous entry
    let prevEntryTok :: Term s PAssetClass
        prevEntryTok = getTokenName paramsF.assocListCs spentInputResolvedF.value
    prevEntryUpdatedF <-
      getOutputEntry
        poolAddr
        prevEntryTok
        txInfoData
        txInfoF.outputs
    -- Get datum for burned entry
    burnEntryF <- getInputEntry burnEntryOutRef txInfoData txInfoF.inputs
    ---- BUSINESS LOGIC ----
    -- Burning action only valid if pool is open
    pguardC "withdrawHeadActLogic: Pool is not open" $ toPBool # burnEntryF.open
    -- Validate withdrawn amount
    withdrawRewardsGuard period withdrawnAmt paramsF txInfoF burnEntryF
    ---- INDUCTIVE CONDITIONS ----
    -- Validate that spentOutRef is the previous entry and matches redeemer
    pguardC "withdrawOtherActLogic: spent input is not an entry" $
      hasListNft paramsF.assocListCs spentInputResolvedF.value
    -- Validate that burn entry key matches the key in previous entry
    pguardC
      "withdrawOtherActLogic: consumed entry key does not match previous \
      \entry's key"
      $ prevEntryF.next `pointsTo` burnEntryF.key
    -- Validate updated entry
    pguardC
      "withdrawOtherActLogic: updated previous entry does not point to same \
      \location as burned entry"
      $ prevEntryUpdatedF.next #== pdata burnEntryF.next
    -- Validate other fields of updated entry (they should stay the same)
    equalEntriesGuard prevEntryUpdatedF prevEntryF

-- Helper functions for the different logics

-- Retrieves state fields from staking datum
getStateData ::
  forall (s :: S).
  Term s PUnbondedStakingDatum ->
  TermCont s (Term s (PMaybeData PByteString), Term s PBoolData)
getStateData datum = do
  record <- tcont . pletFields @["_0", "_1"] . pmatch datum $ \case
    PStateDatum record -> record
    _ -> ptraceError "getStateData: datum is not PStateDatum"
  pure (pfromData record._0, pfromData record._1)

-- Retrieves entry fields from staking datum
getEntryData ::
  forall (s :: S).
  Term s PUnbondedStakingDatum ->
  TermCont s (Term s PEntry)
getEntryData datum =
  pure . pmatch datum $ \case
    PEntryDatum entry -> pfield @"_0" # entry
    _ -> ptraceError "getEntryDatum: datum is not PEntryDatum"

getOutputEntry ::
  forall (s :: S).
  Term s PAddress ->
  Term s PAssetClass ->
  Term s (PBuiltinList (PAsData (PTuple PDatumHash PDatum))) ->
  Term s (PBuiltinList (PAsData PTxOut)) ->
  TermCont s (PEntryHRec s)
getOutputEntry poolAddr ac txInfoData =
  tcont . pletFields @PEntryFields
    <=< getEntryData
    <=< parseStakingDatum
    <=< flip getDatum txInfoData
    <=< getDatumHash
    <=< getContinuingOutputWithNFT poolAddr ac

getInputEntry ::
  forall (s :: S).
  Term s PTxOutRef ->
  Term s (PBuiltinList (PAsData (PTuple PDatumHash PDatum))) ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  TermCont s (PEntryHRec s)
getInputEntry inputOutRef txInfoData inputs = do
  entry <- flip pfind inputs $
    plam $ \input ->
      let outRef :: Term s PTxOutRef
          outRef = pfield @"outRef" # input
       in pdata outRef #== pdata inputOutRef
  tcont . pletFields @PEntryFields
    <=< getEntryData
    <=< parseStakingDatum
    <=< flip getDatum txInfoData
    <=< getDatumHash
    $ pfield @"resolved" # entry

-- This function validates the fields for a freshly minted entry
newEntryGuard ::
  forall (s :: S).
  PUnbondedPoolParamsHRec s ->
  PEntryHRec s ->
  Term s PNatural ->
  Term s PByteString ->
  TermCont s (Term s PUnit)
newEntryGuard paramsF newEntryF stakeAmt stakeHolderKey = do
  pguardC
    "newEntryGuard: incorrect init. of newDeposit and deposit fields \
    \in first stake"
    $ newEntryF.newDeposit #== stakeAmt
      #&& newEntryF.deposited #== stakeAmt
  pguardC "newEntryGuard: new entry does not have the stakeholder's key" $
    newEntryF.key #== stakeHolderKey
  pguardC "newEntryGuard: new entry's stake not within stake bounds" $
    pfromData paramsF.minStake #<= newEntryF.deposited
      #&& pfromData newEntryF.deposited #<= paramsF.maxStake
  pguardC
    "newEntryGuard: new entry's rewards, totalRewards, and totalDeposited \
    \fields not initialized to zero"
    $ newEntryF.rewards #== ratZero
      #&& newEntryF.totalRewards #== natZero
      #&& newEntryF.totalDeposited #== natZero
  pure punit

-- This function validates that two entries' fields are the same (with the
-- exception of fields related to the associative list)
equalEntriesGuard ::
  forall (s :: S).
  PEntryHRec s ->
  PEntryHRec s ->
  TermCont s (Term s PUnit)
equalEntriesGuard e1 e2 = do
  pguardC "equalEntriesGuard: some fields in the given entries are not equal" $
    e1.key #== e2.key
      #&& e1.deposited #== e2.deposited
      #&& e1.newDeposit #== e2.newDeposit
      #&& e1.rewards #== e2.rewards
      #&& e1.totalRewards #== e2.totalRewards
      #&& e1.totalDeposited #== e2.totalDeposited
      #&& e1.open #== e2.open
  pure punit

getKey ::
  forall (s :: S).
  Term s (PMaybeData PByteString) ->
  TermCont s (Term s PByteString)
getKey =
  pure
    . flip
      pmatch
      ( \case
          PDJust key -> pfield @"_0" # key
          PDNothing _ -> ptraceError "getKey: no key found"
      )

withdrawRewardsGuard ::
  forall (s :: S).
  Term s PPeriod ->
  Term s PNatural ->
  PUnbondedPoolParamsHRec s ->
  PTxInfoHRec s ->
  PEntryHRec s ->
  TermCont s (Term s PUnit)
withdrawRewardsGuard period amount paramsF txInfoF entryF =
  pure . pmatch period $ \case
    DepositWithdrawPeriod -> unTermCont $ do
      -- User can deposit then withdraw in the first cycle, resulting in
      -- division by zero in rewards calculation (totalDeposited = 0)
      pure $
        pif
          (entryF.totalDeposited #== natZero)
          (depositWithdrawGuardNoRewards amount entryF)
          (depositWithdrawGuardWithRewards amount entryF)
    BondingPeriod -> unTermCont $ do
      -- Calculate maximum withdrawal amount
      pguardC
        "withdrawRewardsGuard: totalDeposited is zero"
        $ natZero #< entryF.totalDeposited
      let flhs =
            mkNatRatioUnsafe
              (pto (entryF.totalRewards :: Term s PNatural))
              (pto (entryF.totalDeposited :: Term s PNatural))
          frhs = entryF.rewards #+ (toNatRatio entryF.deposited)
          f = roundDown $ frhs #* flhs
          rhsDenominator = paramsF.interest #+ (toNatRatio natOne)
          rhsDenominator' =
            roundUp $
              natPow
                # rhsDenominator
                # (getUnbondedBondingPeriodIncrement txInfoF.validRange paramsF)
          rhs = mkNatRatioUnsafe (pto f) (pto rhsDenominator')
          bondingRewards = roundDown $ entryF.rewards #+ rhs

      -- Validate amount is within bounds
      pguardC
        "withdrawRewardsGuard: reward withdrawal amount not within bounds"
        $ entryF.deposited #<= amount
          #&& amount #<= entryF.deposited #+ bondingRewards
      pure punit
    _ -> ptraceError "withdrawRewardsGuard: invalid withdraw period"
  where
    depositWithdrawGuardNoRewards ::
      forall (s :: S).
      Term s PNatural ->
      PEntryHRec s ->
      Term s PUnit
    depositWithdrawGuardNoRewards amount entryF = unTermCont $ do
      pguardC
        "withdrawRewardsGuard: reward withdrawal amount not within bounds"
        $ entryF.deposited #<= amount
          #&& amount #<= entryF.deposited
      pure punit
    depositWithdrawGuardWithRewards ::
      forall (s :: S).
      Term s PNatural ->
      PEntryHRec s ->
      Term s PUnit
    depositWithdrawGuardWithRewards amount entryF = unTermCont $ do
      let rewards =
            roundDown $
              calculateRewards
                entryF.rewards
                entryF.totalRewards
                entryF.deposited
                entryF.newDeposit
                entryF.totalDeposited
      pguardC
        "withdrawRewardsGuard: reward withdrawal amount not within bounds"
        $ entryF.deposited #<= amount
          #&& amount #<= entryF.deposited #+ rewards
      pure punit

calculateRewards ::
  forall (s :: S).
  Term s PNatRatio ->
  Term s PNatural ->
  Term s PNatural ->
  Term s PNatural ->
  Term s PNatural ->
  Term s PNatRatio
calculateRewards rewards totalRewards deposited newDeposit totalDeposited =
  unTermCont $ do
    pguardC
      "calculateRewards: totalDeposited is zero"
      $ natZero #< totalDeposited
    let lhs = mkNatRatioUnsafe (pto totalRewards) (pto totalDeposited)
        rhs = rewards #+ (toNatRatio deposited)
        rhs' = rhs #- (toNatRatio newDeposit)
    pure $
      pmatch rhs' $ \case
        PNothing ->
          ptraceError
            "calculateRewards: invalid deposit amount"
        PJust rhs'' -> unTermCont $ do
          let f = rhs'' #* lhs
          pure $ rewards #+ f
