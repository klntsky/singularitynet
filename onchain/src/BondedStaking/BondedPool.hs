{-# LANGUAGE UndecidableInstances #-}

module BondedStaking.BondedPool (
  pbondedPoolValidator,
  pbondedPoolValidatorUntyped,
) where

import BondedStaking.PTypes (
  PBondedPoolParams,
  PBondedPoolParamsFields,
  PBondedPoolParamsHRec,
  PBondedStakingAction (
    PAdminAct,
    PCloseAct,
    PStakeAct,
    PWithdrawAct
  ),
  PBondedStakingDatum (PAssetDatum, PEntryDatum, PStateDatum),
  PEntry,
  PEntryFields,
  PEntryHRec,
 )

import Control.Monad ((<=<))

import Plutarch.Api.V1 (
  PAddress,
  PCurrencySymbol,
  PDatumHash,
  PMaybeData (PDJust, PDNothing),
  PPubKeyHash,
  PScriptContext,
  PScriptPurpose,
  PTokenName (PTokenName),
  PTuple,
  PTxInInfo,
  PTxInfo,
  PTxOut,
  PTxOutRef,
  PValue,
 )
import Plutarch.Builtin (pforgetData)
import Plutarch.Unsafe (punsafeCoerce)

import PNatural (
  PNatural,
  PNonNegative ((#+)),
  natZero,
  ratZero,
  -- roundDown,
  -- toNatRatio,
 )

import PTypes (
  HField,
  PAssetClass,
  PBurningAction (PBurnHead, PBurnOther, PBurnSingle),
  PMintingAction (PMintEnd, PMintHead, PMintInBetween),
  PTxInInfoFields,
  PTxInInfoHRec,
  PTxInfoFields,
  PTxInfoHRec,
  bondingPeriod,
  closingPeriod,
  depositWithdrawPeriod,
  onlyWithdrawPeriod,
  passetClass,
 )

import PInterval (
  getBondedPeriod,
 )

import Utils (
  getCoWithDatum,
  getContinuingOutputWithNFT,
  getDatum,
  getDatumHash,
  getInput,
  getOutputsSignedBy,
  getTokenName,
  getTokenTotalOutputs,
  oneWith,
  parseStakingDatum,
  pconst,
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
  (>:),
 )

import GHC.Records (getField)
import InductiveLogic (
  doesNotConsumeBondedAssetGuard,
  hasEntryToken,
  hasListNft,
  hasStateToken,
  pointsNowhere,
  pointsTo,
 )
import Plutarch.Api.V1.Scripts (PDatum)
import Plutarch.Crypto (pblake2b_256)
import Plutarch.DataRepr (HRec)
import SingularityNet.Settings (bondedStakingTokenName)

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
pbondedPoolValidator ::
  forall (s :: S).
  Term
    s
    ( PBondedPoolParams
        :--> PBondedStakingDatum
        :--> PBondedStakingAction
        :--> PScriptContext
        :--> PUnit
    )
pbondedPoolValidator = phoistAcyclic $
  plam $ \params dat act ctx -> unTermCont $ do
    -- Retrieve fields from parameters
    ctxF <- tcont $ pletFields @'["txInfo", "purpose"] ctx
    txInfoF <- tcont $ pletFields @PTxInfoFields ctxF.txInfo
    paramsF <- tcont $ pletFields @PBondedPoolParamsFields params
    -- Match on redeemer, check period and minted value, execute the
    -- corresponding logic
    period <- pletC $ getBondedPeriod # txInfoF.validRange # params
    pure $
      pmatch act $ \case
        PAdminAct _ -> unTermCont $ do
          pguardC "pbondedPoolValidator: wrong period for PAdminAct redeemer" $
            period #== bondingPeriod
          pure $ adminActLogic txInfoF paramsF
        PStakeAct act -> unTermCont $ do
          pguardC
            "pbondedPoolValidator: wrong period for PStakeAct \
            \redeemer"
            $ period #== depositWithdrawPeriod
          pure
            . pletFields
              @'["stakeAmount", "pubKeyHash", "maybeMintingAction"]
              act
            $ \actF ->
              stakeActLogic
                txInfoF
                paramsF
                ctxF.purpose
                dat
                actF
        PWithdrawAct act' -> unTermCont $ do
          pguardC
            "pbondedPoolValidator: wrong period for PWithdrawAct \
            \redeemer"
            $ period #== depositWithdrawPeriod #|| period #== onlyWithdrawPeriod
          pguardC
            "pbondedPoolValidator: a token should be burned when using \
            \ PWithdrawAct"
            $ isBurningEntry txInfoF.mint paramsF.assocListCs
          act <- tcont . pletFields @'["pubKeyHash", "burningAction"] $ act'
          withdrawActLogic
            txInfoF
            paramsF
            ctxF.purpose
            dat
            act
        PCloseAct _ -> unTermCont $ do
          pguardC "pbondedPoolValidator: wrong period for PcloseAct redeemer" $
            period #== closingPeriod
          pure $ closeActLogic ctxF.txInfo params
  where
    isBurningEntry ::
      forall (s :: S).
      Term s PValue ->
      Term s PCurrencySymbol ->
      Term s PBool
    isBurningEntry val cs =
      oneWith # (peq # cs) # pconst ptrue # (peq # (-1)) # val

-- Untyped version to be serialised. This version is responsible for verifying
-- that the parameters (pool params, datum and redeemer) have the proper types.
-- The script context should always be safe.
pbondedPoolValidatorUntyped ::
  forall (s :: S).
  Term
    s
    ( PData
        :--> PData
        :--> PData
        :--> PData
        :--> PUnit
    )
pbondedPoolValidatorUntyped = plam $ \pparams dat act ctx ->
  pbondedPoolValidator
    # unTermCont (ptryFromUndata pparams)
    # unTermCont (ptryFromUndata dat)
    # unTermCont (ptryFromUndata act)
    # punsafeCoerce ctx

-- The pool operator updates the rewards for a given entry in the association
-- list
adminActLogic ::
  forall (s :: S).
  PTxInfoHRec s ->
  PBondedPoolParamsHRec s ->
  Term s PUnit
adminActLogic txInfo params = unTermCont $ do
  -- We check that the transaction was signed by the pool operator
  pguardC "transaction not signed by admin" $
    signedBy txInfo.signatories params.admin
  pure punit

stakeActLogic ::
  forall (s :: S).
  PTxInfoHRec s ->
  PBondedPoolParamsHRec s ->
  Term s PScriptPurpose ->
  Term s PBondedStakingDatum ->
  HRec
    '[ HField s "stakeAmount" PNatural
     , HField s "pubKeyHash" PPubKeyHash
     , HField s "maybeMintingAction" (PMaybeData PMintingAction)
     ] ->
  Term s PUnit
stakeActLogic txInfo params purpose datum act =
  unTermCont $ do
    -- Get the input being spent
    spentInput <-
      tcont . pletFields @PTxInInfoFields
        =<< getInput purpose txInfo.inputs
    let poolAddr :: Term s PAddress
        poolAddr = pfield @"address" # spentInput.resolved
    -- Check that input spent is not an asset UTXO (user cannot withdraw)
    doesNotConsumeBondedAssetGuard datum
    -- Validate holder's signature
    pguardC "stakeActLogic: tx not exclusively signed by the stake-holder" $
      signedOnlyBy txInfo.signatories act.pubKeyHash
    -- Check that amount is positive
    let stakeAmt :: Term s PNatural
        stakeAmt = pfromData act.stakeAmount
    pguardC "stakeActLogic: stake amount is not positive or within bounds" $
      natZero #<= stakeAmt
    -- Get asset output of the transaction (new locked stake)
    -- TODO: Fix this line right here
    assetOutput <-
      (tcont . pletFields @'["value"] . fst)
        =<< ( getCoWithDatum
                poolAddr
                isAssetDatum
                txInfo.outputs
                $ getField @"data" txInfo
            )
    -- Check that the correct amount of the assetclass is locked
    bondedAsset <-
      tcont . pletFields @'["currencySymbol", "tokenName"] $
        params.bondedAssetClass
    pguardC "stakeActLogic: amount deposited does not match redeemer's amount" $
      oneWith # (peq # bondedAsset.currencySymbol)
        # (peq # bondedAsset.tokenName)
        # (peq #$ pto $ pfromData act.stakeAmount)
        # assetOutput.value

    pure . pmatch act.maybeMintingAction $ \case
      -- If some minting action is provided, this is a new stake and inductive
      -- conditions must be checked
      PDJust mintAct -> unTermCont $ do
        -- Check that minted value is a list entry (minting policy is run)
        pguardC "stakeActLogic: failure when checking minted value in minting tx" $
          hasListNft params.assocListCs txInfo.mint
        -- Check inductive conditions and business logic
        newStakeLogic
          txInfo
          params
          spentInput
          datum
          act.pubKeyHash
          act.stakeAmount
          $ pfield @"_0" # mintAct
      -- If no minting action is provided, this is a stake update
      PDNothing _ -> unTermCont $ do
        -- A list token should *not* be minted
        pguardC
          "stakeActLogic: failure when checking minted value in non-minting \
          \ tx"
          $ pnot #$ hasListNft params.assocListCs txInfo.mint
        -- Check business logic
        updateStakeLogic
          txInfo
          params
          spentInput
          datum
          act.stakeAmount
          act.pubKeyHash
  where
    isAssetDatum :: Term s (PDatum :--> PBool)
    isAssetDatum = plam $ \dat' -> unTermCont $ do
      dat <-
        ptryFromUndata @PBondedStakingDatum
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
  PBondedPoolParamsHRec s ->
  PTxInInfoHRec s ->
  Term s PBondedStakingDatum ->
  Term s PNatural ->
  Term s PPubKeyHash ->
  TermCont s (Term s PUnit)
updateStakeLogic txInfo params spentInput datum stakeAmt holderPkh = do
  -- Construct some useful values for later
  stakeHolderKey <- pletC $ pblake2b_256 # pto holderPkh
  stakeHolderTn <- pletC $ pcon $ PTokenName $ stakeHolderKey
  spentInputResolved <-
    tcont . pletFields @'["address", "value", "datumHash"] $ spentInput.resolved
  let poolAddr :: Term s PAddress
      poolAddr = spentInputResolved.address
      txInfoData = getField @"data" txInfo
  newEntryTok <- pletC $ passetClass # params.assocListCs # stakeHolderTn
  ---- FETCH DATUMS ----
  entry <- tcont . pletFields @PEntryFields =<< getEntryData datum
  newEntry <- getOutputEntry poolAddr newEntryTok txInfoData txInfo.outputs
  ---- BUSINESS LOGIC ----
  pguardC "updateStakeLogic: spent entry's key does not match user's key" $
    entry.key #== stakeHolderKey
  pguardC "updateStakeLogic: new entry does not have the stakeholder's key" $
    newEntry.key #== stakeHolderKey
  pguardC "updateStakeLogic: incorrect update of newDeposit" $
    newEntry.newDeposit #== entry.newDeposit #+ stakeAmt
  pguardC "updateStakeLogic: incorrect update of deposit" $
    newEntry.deposited #== entry.deposited #+ stakeAmt
  pguardC "updateStakeLogic: update increases stake beyond allowed bounds" $
    pfromData params.minStake #<= newEntry.deposited
      #&& pfromData newEntry.deposited #<= params.maxStake
  pguardC
    "updateStakeLogic: update should not change staked, rewards or next \
    \fields"
    $ entry.staked #== newEntry.staked
      #&& entry.rewards #== newEntry.rewards
      #&& entry.next #== newEntry.next
  pure punit

{- | This function checks all inductive conditions and makes all necessary
 business logic validation on the state/entry updates and new entries
-}
newStakeLogic ::
  forall (s :: S).
  PTxInfoHRec s ->
  PBondedPoolParamsHRec s ->
  PTxInInfoHRec s ->
  Term s PBondedStakingDatum ->
  Term s PPubKeyHash ->
  Term s PNatural ->
  Term s PMintingAction ->
  TermCont s (Term s PUnit)
newStakeLogic txInfo params spentInput datum holderPkh stakeAmt mintAct =
  do
    -- Construct some useful values for later
    stakeHolderKey <- pletC $ pblake2b_256 # pto holderPkh
    stakeHolderTn <- pletC $ pcon $ PTokenName $ stakeHolderKey
    spentInputResolved <-
      tcont . pletFields @'["address", "value", "datumHash"] $ spentInput.resolved
    let poolAddr :: Term s PAddress
        poolAddr = spentInputResolved.address
        stateTn :: Term s PTokenName
        stateTn = pconstant bondedStakingTokenName
    stateTok <- pletC $ passetClass # params.nftCs # stateTn
    newEntryTok <- pletC $ passetClass # params.assocListCs # stakeHolderTn
    txInfoData <- pletC $ getField @"data" txInfo
    pure . pmatch mintAct $ \case
      PMintHead stateOutRef -> unTermCont $ do
        ---- FETCH DATUMS ----
        -- Get datum for next state
        nextStateTxOut <-
          getContinuingOutputWithNFT poolAddr stateTok txInfo.outputs
        nextStateHash <- getDatumHash nextStateTxOut
        nextEntryKey <-
          getStateData
            =<< parseStakingDatum
            =<< getDatum nextStateHash txInfoData
        -- Get datum for current state
        entryKey <- getStateData datum
        -- Get datum for new list entry
        newEntry <- getOutputEntry poolAddr newEntryTok txInfoData txInfo.outputs
        ---- BUSINESS LOGIC ----
        newEntryGuard params newEntry stakeAmt stakeHolderKey
        ---- INDUCTIVE CONDITIONS ----
        -- Validate that spentOutRef is the state UTXO and matches redeemer
        pguardC "newStakeLogic (mintHead): spent input is not the state UTXO" $
          spentInputResolved.value
            `hasStateToken` (params.nftCs, pconstant bondedStakingTokenName)
        pguardC
          "newStakeLogic (mintHead): spent input does not match redeemer \
          \input"
          $ spentInput.outRef #== pfield @"_0" # stateOutRef
        -- Validate next state
        pguardC
          "newStakeLogic (mintHead): next pool state does not point to new \
          \ entry"
          $ nextEntryKey `pointsTo` newEntry.key
        -- Validate list order and links
        pure . pmatch entryKey $ \case
          -- We are shifting the current head forwards
          PDJust currentEntryKey' -> unTermCont $ do
            currentEntryKey <- pletC $ pfromData $ pfield @"_0" # currentEntryKey'
            -- Validate order of entries
            pguardC
              "newStakeLogic (mintHead): new entry's key should be \
              \strictly less than  current entry"
              $ pfromData newEntry.key #< currentEntryKey
            -- The new entry should point to the current entry
            pguardC
              "newStakeLogic (mintHead): new entry should point to \
              \current entry"
              $ newEntry.next `pointsTo` currentEntryKey
            pure punit
          -- This is the first stake of the pool
          PDNothing _ -> unTermCont $ do
            -- The new entry should *not* point to anything
            pguardC
              "newStakeLogic (mintHead): new entry should not point to \
              \anything"
              $ pointsNowhere newEntry.next
            pure punit
      PMintInBetween outRefs -> unTermCont $ do
        ---- FETCH DATUMS ----
        entriesRefs <-
          tcont $ pletFields @'["previousEntry", "currentEntry"] outRefs
        -- Get datum for prevEntry
        prevEntry <- tcont . pletFields @PEntryFields =<< getEntryData datum
        -- Get datum for prevEntryUpdated
        let prevEntryTok :: Term s PAssetClass
            prevEntryTok =
              getTokenName
                params.assocListCs
                spentInputResolved.value
        prevEntryUpdated <-
          getOutputEntry poolAddr prevEntryTok txInfoData txInfo.outputs
        -- Get datum for new list entry
        newEntry <-
          getOutputEntry poolAddr newEntryTok txInfoData txInfo.outputs
        -- Get the current entry's key
        currEntryKey <- pure . pmatch prevEntry.next $ \case
          PDJust key -> pfield @"_0" # key
          PDNothing _ ->
            ptraceError
              "newStakeLogic (mintInBetween): the previous \
              \ entry does not point to another entry"
        ---- BUSINESS LOGIC ----
        -- Validate initialization of new entry
        newEntryGuard params newEntry stakeAmt stakeHolderKey
        -- Previous entry should keep the same values when updated
        equalEntriesGuard prevEntry prevEntryUpdated
        ---- INDUCTIVE CONDITIONS ----
        -- Validate that previousEntry is a list entry and matches redeemer
        pguardC "newStakeLogic (mintInBetween): spent input is not an entry" $
          hasListNft params.assocListCs spentInputResolved.value
        pguardC
          "newStakeLogic (mintInBetween): spent input is not the same as input in \
          \ redeemer"
          $ spentInput.outRef #== entriesRefs.previousEntry
        -- Previous entry should now point to the new entry
        pguardC
          "newStakeLogic (mintInBetween): the previous entry should point to the \
          \new entry"
          $ prevEntryUpdated.next `pointsTo` newEntry.key
        -- And new entry should point to the current entry
        pguardC
          "newStakeLogic (mintInBetween): the new entry should point to the \
          \current entry"
          $ newEntry.next `pointsTo` currEntryKey
        -- Validate entries' order
        pguardC
          "newStakeLogic (mintInBetween): failed to validate order in previous, \
          \current and new entry"
          $ pfromData prevEntry.key #< newEntry.key
            #&& newEntry.key #< currEntryKey
        pure punit
      PMintEnd listEndOutRef' -> unTermCont $ do
        ---- FETCH DATUMS ----
        listEndOutRef <- pletC $ pfield @"_0" # listEndOutRef'
        -- Get datum for endEntry
        endEntry <- tcont . pletFields @PEntryFields =<< getEntryData datum
        -- Get datum for endEntryUpdated
        let endEntryTok :: Term s PAssetClass
            endEntryTok =
              getTokenName
                params.assocListCs
                spentInputResolved.value
        endEntryUpdated <-
          getOutputEntry poolAddr endEntryTok txInfoData txInfo.outputs
        -- Get datum for new list entry
        newEntry <-
          getOutputEntry poolAddr newEntryTok txInfoData txInfo.outputs
        ---- BUSINESS LOGIC ----
        -- Validate initialization of new entry
        newEntryGuard params newEntry stakeAmt stakeHolderKey
        -- End entry should keep the same values when updated
        equalEntriesGuard endEntry endEntryUpdated
        ---- INDUCTIVE CONDITIONS ----
        -- Validate that endEntry is a list entry and matches redeemer
        pguardC "newStakeLogic (mintEnd): spent input is not an entry" $
          hasListNft params.assocListCs spentInputResolved.value
        pguardC
          "newStakeLogic (mintEnd): spent input is not the same as input in \
          \redeemer"
          $ spentInput.outRef #== listEndOutRef
        -- End entry should point nowhere
        pguardC "newStakeLogic (mintEnd): end should point nowhere" $
          pointsNowhere endEntry.next
        -- Updated end entry (no longer end) should point to new entry
        pguardC
          "newStakeLogic (mintEnd): updated end should point to new end \
          \entry"
          $ endEntryUpdated.next `pointsTo` newEntry.key
        -- New entry (new end) should point nowhere
        pguardC "newStakeLogic (mintEnd): new end entry should not point anywhere" $
          pointsNowhere newEntry.next
        -- Validate entries' order
        pguardC
          "newStakeLogic (mintEnd): new entry's key should come after end \
          \entry"
          $ pfromData endEntryUpdated.key #< pfromData newEntry.key
        pure punit

withdrawActLogic ::
  forall (s :: S).
  PTxInfoHRec s ->
  PBondedPoolParamsHRec s ->
  Term s PScriptPurpose ->
  Term s PBondedStakingDatum ->
  HRec
    '[ HField s "pubKeyHash" PPubKeyHash
     , HField s "burningAction" PBurningAction
     ] ->
  TermCont s (Term s PUnit)
withdrawActLogic
  txInfo
  params
  purpose
  datum
  act = do
    -- Construct some useful values for later
    let holderKey = pblake2b_256 # pto (pfromData act.pubKeyHash)
    entryTn <-
      pletC . pcon . PTokenName $ holderKey
    -- Get the input being spent
    spentInput <-
      tcont . pletFields @PTxInInfoFields
        =<< getInput purpose txInfo.inputs
    spentInputResolved <- tcont . pletFields @'["value"] $ spentInput.resolved
    -- Validate holder's signature
    pguardC "withdrawActLogic: tx not exclusively signed by the stake-holder" $
      signedOnlyBy txInfo.signatories act.pubKeyHash
    withdrawnAmt <-
      getTokenTotalOutputs params.bondedAssetClass
        <$> getOutputsSignedBy act.pubKeyHash txInfo.outputs
    -- Validate the asset input is effectively an asset UTXO
    let assetCheck :: Term s PUnit
        assetCheck = unTermCont $ do
          datum <-
            parseStakingDatum @PBondedStakingDatum
              <=< flip getDatum (getField @"data" txInfo)
              <=< getDatumHash
              $ spentInput.resolved
          pure $
            pmatch datum $ \case
              PAssetDatum _ -> punit
              _ -> ptraceError "withdrawActLogic: expected asset input"
        -- We validate the entry when consuming it
        entryCheck :: Term s PTxOutRef -> Term s PUnit
        entryCheck entryOutRef = unTermCont $ do
          pguardC
            "withdrawActLogic: spent entry is not an entry (no List NFT)"
            $ hasListNft params.assocListCs spentInputResolved.value
          pguardC
            "withdrawActLogic: spent entry does not match redeemer \
            \TxOutRef"
            $ pdata entryOutRef #== spentInput.outRef
          pguardC "withdrawActLogic: entry does not belong to user" $
            hasEntryToken
              spentInputResolved.value
              (params.assocListCs, entryTn)
          pure punit
    -- Check business and inductive conditions depending on redeemer
    pure . pmatch (pfromData act.burningAction) $ \case
      PBurnHead outRefs' -> unTermCont $ do
        outRefs <- tcont . pletFields @'["state", "headEntry"] $ outRefs'
        -- We check most conditions when consuming the state UTXO
        let withdrawHeadCheck =
              withdrawHeadActLogic
                spentInput
                withdrawnAmt
                datum
                txInfo
                params
                outRefs.state
                outRefs.headEntry
        pure $
          pnestedIf
            [ spentInput.outRef #== outRefs.state >: withdrawHeadCheck
            , spentInput.outRef #== outRefs.headEntry >: entryCheck outRefs.headEntry
            ]
            assetCheck
      PBurnOther entries' -> unTermCont $ do
        entries <- tcont . pletFields @'["previousEntry", "burnEntry"] $ entries'
        let withdrawOtherCheck :: Term s PUnit
            withdrawOtherCheck =
              withdrawOtherActLogic
                spentInput
                withdrawnAmt
                datum
                txInfo
                params
                entries.burnEntry
        pure $
          pnestedIf
            [ spentInput.outRef #== entries.previousEntry >: withdrawOtherCheck
            , spentInput.outRef #== entries.burnEntry >: entryCheck entries.burnEntry
            ]
            $ assetCheck
      PBurnSingle _ ->
        ptraceError
          "withdrawActLogic: burning action is invalid for bonded pool"

withdrawHeadActLogic ::
  forall (s :: S).
  HRec
    '[ HField s "outRef" PTxOutRef
     , HField s "resolved" PTxOut
     ] ->
  Term s PNatural ->
  Term s PBondedStakingDatum ->
  PTxInfoHRec s ->
  PBondedPoolParamsHRec s ->
  Term s PTxOutRef ->
  Term s PTxOutRef ->
  Term s PUnit
withdrawHeadActLogic spentInput withdrawnAmt datum txInfo params stateOutRef headEntryOutRef =
  unTermCont $ do
    -- Construct some useful values for later
    spentInputResolved <-
      tcont . pletFields @'["address", "value", "datumHash"] $ spentInput.resolved
    let poolAddr :: Term s PAddress
        poolAddr = spentInputResolved.address
        stateTn :: Term s PTokenName
        stateTn = pconstant bondedStakingTokenName
    stateTok <- pletC $ passetClass # params.nftCs # stateTn
    txInfoData <- pletC $ getField @"data" txInfo
    ---- FETCH DATUMS ----
    -- Get datum for next state
    nextStateTxOut <-
      getContinuingOutputWithNFT poolAddr stateTok txInfo.outputs
    nextStateHash <- getDatumHash nextStateTxOut
    nextEntryKey <-
      getStateData
        =<< parseStakingDatum
        =<< getDatum nextStateHash txInfoData
    -- Get datum for current state
    entryKey <- getKey =<< getStateData datum
    -- Get datum for head entry
    headEntry <- getInputEntry headEntryOutRef txInfoData txInfo.inputs
    ---- BUSINESS LOGIC ----
    -- Validate that entry key matches the key in state UTxO
    pguardC "withdrawHeadActLogic: consumed entry key does not match user's pkh" $
      headEntry.key #== entryKey
    pguardC
      "withdrawHeadActLogic: withdrawn amount does not match stake and \
      \rewards"
      $ pto withdrawnAmt #== pto (pfromData headEntry.deposited)
    ---- INDUCTIVE CONDITIONS ----
    -- Validate that spentOutRef is the state UTXO and matches redeemer
    pguardC "withdrawHeadActLogic: spent input is not the state UTXO" $
      spentInputResolved.value
        `hasStateToken` (params.nftCs, pconstant bondedStakingTokenName)
    pguardC "withdrawHeadActLogic: spent input does not match redeemer input" $
      spentInput.outRef #== pdata stateOutRef
    -- Validate that consumed entry is head of the list
    pguardC "withdrawHeadActLogic: spent entry is not head of the list" $
      entryKey #== headEntry.key
    -- Validate next state
    pguardC
      "withdrawHeadActLogic: next pool state does not point to same \
      \location as burned entry"
      $ pdata nextEntryKey #== headEntry.next
    pure punit

withdrawOtherActLogic ::
  forall (s :: S).
  HRec
    '[ HField s "outRef" PTxOutRef
     , HField s "resolved" PTxOut
     ] ->
  Term s PNatural ->
  Term s PBondedStakingDatum ->
  PTxInfoHRec s ->
  PBondedPoolParamsHRec s ->
  Term s PTxOutRef ->
  Term s PUnit
withdrawOtherActLogic spentInput withdrawnAmt datum txInfo params burnEntryOutRef =
  unTermCont $ do
    -- Construct some useful values for later
    spentInputResolved <-
      tcont . pletFields @'["address", "value", "datumHash"] $ spentInput.resolved
    let poolAddr :: Term s PAddress
        poolAddr = spentInputResolved.address
    txInfoData <- pletC $ getField @"data" txInfo
    ---- FETCH DATUMS ----
    -- Get datum for previous entry
    prevEntry <- tcont . pletFields @PEntryFields =<< getEntryData datum
    -- Get updated datum for previous entry
    let prevEntryTok :: Term s PAssetClass
        prevEntryTok = getTokenName params.assocListCs spentInputResolved.value
    prevEntryUpdated <-
      getOutputEntry
        poolAddr
        prevEntryTok
        txInfoData
        txInfo.outputs
    -- Get datum for burned entry
    burnEntry <- getInputEntry burnEntryOutRef txInfoData txInfo.inputs
    ---- BUSINESS LOGIC ----
    -- Validate withdrawn amount
    pguardC
      "withdrawOtherActLogic: withdrawn amount does not match stake and \
      \rewards"
      $ withdrawnAmt #== burnEntry.deposited
    ----INDUCTIVE CONDITIONS ----
    -- Validate that spentOutRef is the previous entry and matches redeemer
    pguardC "withdrawOtherActLogic: spent input is not an entry" $
      hasListNft params.assocListCs spentInputResolved.value
    -- Validate that burn entry key matches the key in previous entry
    pguardC
      "withdrawOtherActLogic: consumed entry key does not match previous \
      \entry's key"
      $ prevEntry.next `pointsTo` burnEntry.key
    -- Validate updated entry
    pguardC
      "withdrawOtherActLogic: updated previous entry does not point to same \
      \location as burned entry"
      $ prevEntryUpdated.next #== pdata burnEntry.next
    -- Validate other fields of updated entry (they should stay the same)
    equalEntriesGuard prevEntryUpdated prevEntry

closeActLogic ::
  forall (s :: S).
  Term s PTxInfo ->
  Term s PBondedPoolParams ->
  Term s PUnit
closeActLogic txInfo params = unTermCont $ do
  -- Retrieve fields from parameters
  txInfoF <-
    tcont $
      pletFields
        @'["signatories", "validRange"]
        txInfo
  paramsF <- tcont $ pletFields @'["admin"] params
  -- We check that the transaction was signed by the pool operator
  pguardC "transaction not signed by admin" $
    signedBy txInfoF.signatories paramsF.admin
  pure punit

-- Helper functions for the different logics

-- Retrieves state fields from staking datum
getStateData ::
  forall (s :: S).
  Term s PBondedStakingDatum ->
  TermCont s (Term s (PMaybeData PByteString))
getStateData datum = do
  record <- tcont . pletFields @'["_0"] . pmatch datum $ \case
    PStateDatum record -> record
    _ -> ptraceError "getStateData: datum is not PStateDatum"
  pure (pfromData record._0)

-- Retrieves entry fields from staking datum
getEntryData ::
  forall (s :: S).
  Term s PBondedStakingDatum ->
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
  PBondedPoolParamsHRec s ->
  PEntryHRec s ->
  Term s PNatural ->
  Term s PByteString ->
  TermCont s (Term s PUnit)
newEntryGuard params newEntry stakeAmt stakeHolderKey = do
  pguardC
    "newEntryGuard: incorrect init. of newDeposit and deposit fields \
    \in first stake"
    $ newEntry.newDeposit #== stakeAmt
      #&& newEntry.deposited #== stakeAmt
  pguardC "newEntryGuard: new entry does not have the stakeholder's key" $
    newEntry.key #== stakeHolderKey
  pguardC "newEntryGuard: new entry's stake not within stake bounds" $
    pfromData params.minStake #<= newEntry.deposited
      #&& pfromData newEntry.deposited #<= params.maxStake
  pguardC
    "newEntryGuard: new entry's staked and rewards fields not \
    \initialized to zero"
    $ newEntry.staked #== natZero
      #&& newEntry.rewards #== ratZero
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
      #&& e1.newDeposit #== e2.newDeposit
      #&& e1.deposited #== e2.deposited
      #&& e1.staked #== e2.staked
      #&& e1.rewards #== e2.rewards
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
