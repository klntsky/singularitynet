module ListNFT (
  plistNFTPolicy,
  plistNFTPolicyUntyped,
) where

import Plutarch.Api.V1 (
  PCurrencySymbol,
  PPubKeyHash,
  PScriptContext,
  PTokenName (PTokenName),
  PTxInInfo,
  PTxOutRef,
  PValue,
 )
import Plutarch.Api.V1.Scripts ()
import Plutarch.Unsafe (punsafeCoerce)

import PTypes (
  PBurningAction (PBurnHead, PBurnOther, PBurnSingle),
  PListAction (PListInsert, PListRemove),
  PMintingAction (PMintEnd, PMintHead, PMintInBetween),
 )

import InductiveLogic (
  consumesEntriesGuard,
  consumesEntryGuard,
  consumesStateUtxoAndEntryGuard,
  consumesStateUtxoGuard,
  hasNoNft,
  inputPredicate,
 )
import Plutarch.Crypto (pblake2b_256)
import Utils (
  getCs,
  getOnlySignatory,
  oneOf,
  oneWith,
  pconst,
  peq,
  pflip,
  pguardC,
  pletC,
  porList,
  ptrue,
  ptryFromUndata,
  punit,
 )

{-
    This module implements the minting policy for the NFTs used as entries for
    the user's stakes in the on-chain association list.

    The policy checks that:

    1. The required NFTs are being consumed for the specified list action and
       *only* these, no more, no less

    2. The minted/burnt entries' `TokenName`s correspond to the transaction's
       signatory

    Due to how the system is designed, this minting policy is only run when
    a user stakes *for the first time* (minting a new NFT) or when it withdraws
    their stake (burning an NFT).

    Invariants related to the amount staked, global or local limits, and other
    things, are delegated to the pool validator and not treated here.
    Importantly, inductive conditions of the association list are delegated to
    the pool validator.
-}

plistNFTPolicy ::
  forall (s :: S).
  Term
    s
    ( PCurrencySymbol
        :--> PTokenName
        :--> PListAction
        :--> PScriptContext
        :--> PUnit
    )
plistNFTPolicy = plam $ \stateNftCs stateNftTn listAct ctx' -> unTermCont $ do
  -- This CurrencySymbol is only used for parametrization
  _cs <- pletC stateNftCs
  ctx <- tcont $ pletFields @'["txInfo", "purpose"] ctx'
  -- Own CurrencySymbol
  listNftCs <- getCs ctx.purpose
  txInfo <- tcont $ pletFields @'["signatories", "mint", "inputs"] ctx.txInfo
  -- Get a *single* signatory or fail
  signatory <- getOnlySignatory txInfo.signatories
  -- Make token name from signatory's public key hash
  let entryTn = mkEntryTn signatory
      -- Save state and list tokens for later
      stateTok = (stateNftCs, stateNftTn)
      listTok = (listNftCs, entryTn)
  -- Dispatch to appropiate handler based on redeemer
  pure $
    pmatch listAct $ \case
      PListInsert mintAct' ->
        let mintAct = pfield @"_0" # mintAct'
         in listInsertCheck txInfo.inputs txInfo.mint stateTok listTok mintAct
      PListRemove burnAct' ->
        let burnAct = pfield @"_0" # burnAct'
         in listRemoveCheck txInfo.inputs txInfo.mint stateTok listTok burnAct

plistNFTPolicyUntyped ::
  forall (s :: S). Term s (PData :--> PData :--> PData :--> PData :--> PUnit)
plistNFTPolicyUntyped = plam $ \stateNftCs stateNftTn mintAct ctx ->
  plistNFTPolicy # unTermCont (ptryFromUndata stateNftCs)
    # unTermCont (ptryFromUndata stateNftTn)
    # unTermCont (ptryFromUndata mintAct)
    # punsafeCoerce ctx

listInsertCheck ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s PValue ->
  (Term s PCurrencySymbol, Term s PTokenName) ->
  (Term s PCurrencySymbol, Term s PTokenName) ->
  Term s PMintingAction ->
  Term s PUnit
listInsertCheck
  inputs
  mintVal
  (stateNftCs, stateNftTn)
  (listNftCs, entryTn)
  mintAct = unTermCont $ do
    -- Validate stake-holder's signature and token count
    mintGuard listNftCs entryTn mintVal
    pure . pmatch mintAct $ \case
      PMintHead stateOutRef' -> unTermCont $ do
        let stateOutRef :: Term s PTxOutRef
            stateOutRef = pfromData $ pfield @"_0" # stateOutRef'
        mintHeadGuard stateNftCs listNftCs stateNftTn stateOutRef inputs
      PMintInBetween entries -> unTermCont $ do
        entriesF <- tcont $ pletFields @["previousEntry", "currentEntry"] entries
        let prevEntry = entriesF.previousEntry
            currEntry = entriesF.currentEntry
        mintInBetweenGuard stateNftCs listNftCs prevEntry currEntry inputs
      PMintEnd lastEntry' -> unTermCont $ do
        let lastEntry :: Term s PTxOutRef
            lastEntry = pfromData $ pfield @"_0" # lastEntry'
        mintEndGuard stateNftCs listNftCs lastEntry inputs

listRemoveCheck ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s PValue ->
  (Term s PCurrencySymbol, Term s PTokenName) ->
  (Term s PCurrencySymbol, Term s PTokenName) ->
  Term s PBurningAction ->
  Term s PUnit
listRemoveCheck
  inputs
  mintVal
  stateTok
  listTok@(listNftCs, entryTn)
  burnAct = unTermCont $ do
    -- Validate stake-holder's signature and token count
    burnGuard listNftCs entryTn mintVal
    pure . pmatch burnAct $ \case
      PBurnHead outRefs' -> unTermCont $ do
        outRefs <- tcont . pletFields @'["state", "headEntry"] $ outRefs'
        burnHeadGuard
          stateTok
          listTok
          (pfromData outRefs.state)
          (pfromData outRefs.headEntry)
          inputs
      PBurnOther outRefs' -> unTermCont $ do
        outRefs <- tcont . pletFields @'["previousEntry", "burnEntry"] $ outRefs'
        burnOtherGuard
          stateTok
          listTok
          (pfromData outRefs.previousEntry)
          (pfromData outRefs.burnEntry)
          inputs
      PBurnSingle outRef' -> unTermCont $ do
        let outRef = pfield @"burnEntry" # outRef'
        burnSingleGuard
          stateTok
          listTok
          (pfromData outRef)
          inputs

-- Build the token name from the signatory's `PPubKeyHash`
mkEntryTn :: Term s PPubKeyHash -> Term s PTokenName
mkEntryTn pkh = pcon . PTokenName $ pblake2b_256 # pto pkh

-- | Fails if the TX does *not* mint the appropriate list token
mintGuard ::
  forall (s :: S).
  Term s PCurrencySymbol ->
  Term s PTokenName ->
  Term s PValue ->
  TermCont s (Term s PUnit)
mintGuard listNftCs entryTn mint = do
  pguardC "mintGuard: failed when checking minted value" $
    oneOf # listNftCs # entryTn # mint #&& oneWith # (peq # listNftCs) # (pconst ptrue) # (pconst ptrue) # mint
  pure punit

-- | Fails if the TX does *not* burn the appropriate list oken
burnGuard ::
  forall (s :: S).
  Term s PCurrencySymbol ->
  Term s PTokenName ->
  Term s PValue ->
  TermCont s (Term s PUnit)
burnGuard listNftCs entryTn mint = do
  pguardC "mintGuard: failed when checking minted value" $
    burnCheck listNftCs entryTn mint
  pure punit

-- Functions for validating the inputs
mintHeadGuard ::
  forall (s :: S).
  Term s PCurrencySymbol ->
  Term s PCurrencySymbol ->
  Term s PTokenName ->
  Term s PTxOutRef ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  TermCont s (Term s PUnit)
mintHeadGuard stateNftCs listNftCs stateNftTn stateOutRef inputs = do
  -- We check that the state UTXO is consumed
  consumesStateUtxoGuard stateOutRef inputs (stateNftCs, stateNftTn)
  -- We check that the other inputs are not state nor list UTXOs
  noNftGuard stateNftCs listNftCs [stateOutRef] inputs

mintInBetweenGuard ::
  forall (s :: S).
  Term s PCurrencySymbol ->
  Term s PCurrencySymbol ->
  Term s PTxOutRef ->
  Term s PTxOutRef ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  TermCont s (Term s PUnit)
mintInBetweenGuard stateNftCs listNftCs prevEntry currEntry inputs = do
  -- We check that `prevEntry` is consumed
  consumesEntryGuard prevEntry inputs listNftCs
  -- We check that the other inputs are not state nor list UTXOs
  noNftGuard stateNftCs listNftCs [prevEntry, currEntry] inputs

mintEndGuard ::
  forall (s :: S).
  Term s PCurrencySymbol ->
  Term s PCurrencySymbol ->
  Term s PTxOutRef ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  TermCont s (Term s PUnit)
mintEndGuard stateNftCs listNftCs lastEntry inputs = do
  -- Find entry UTXO in inputs and fail if it does not have the list NFT
  consumesEntryGuard lastEntry inputs listNftCs
  -- We check that the other inputs are not state nor list UTXOs
  noNftGuard stateNftCs listNftCs [lastEntry] inputs

burnHeadGuard ::
  forall (s :: S).
  (Term s PCurrencySymbol, Term s PTokenName) ->
  (Term s PCurrencySymbol, Term s PTokenName) ->
  Term s PTxOutRef ->
  Term s PTxOutRef ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  TermCont s (Term s PUnit)
burnHeadGuard
  stateTok@(stateNftCs, _)
  entryTok@(listNftCs, _)
  stateOutRef
  entryOutRef
  inputs = do
    -- We check that the state UTXO and entry are consumed
    consumesStateUtxoAndEntryGuard
      stateOutRef
      entryOutRef
      inputs
      stateTok
      entryTok
    -- We check that the other inputs are not state nor list UTXOs
    noNftGuard stateNftCs listNftCs [stateOutRef, entryOutRef] inputs

burnOtherGuard ::
  forall (s :: S).
  (Term s PCurrencySymbol, Term s PTokenName) ->
  (Term s PCurrencySymbol, Term s PTokenName) ->
  Term s PTxOutRef ->
  Term s PTxOutRef ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  TermCont s (Term s PUnit)
burnOtherGuard
  (stateNftCs, _)
  (listNftCs, _)
  prevEntryOutRef
  burnEntryOutRef
  inputs = do
    -- We check that the two entries are consumed
    consumesEntriesGuard prevEntryOutRef burnEntryOutRef inputs listNftCs
    -- We check that the other inputs are not state nor list UTXOs
    noNftGuard stateNftCs listNftCs [prevEntryOutRef, burnEntryOutRef] inputs

burnSingleGuard ::
  forall (s :: S).
  (Term s PCurrencySymbol, Term s PTokenName) ->
  (Term s PCurrencySymbol, Term s PTokenName) ->
  Term s PTxOutRef ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  TermCont s (Term s PUnit)
burnSingleGuard
  (stateNftCs, _)
  (listNftCs, _)
  burnEntryOutRef
  inputs = do
    -- We check that the entry is consumed
    consumesEntryGuard burnEntryOutRef inputs listNftCs
    -- We check that the burn entry is not a state nor list UTXO
    noNftGuard stateNftCs listNftCs [burnEntryOutRef] inputs

-- Check that all unprotected inputs contain no state nor list NFTs
noNftGuard ::
  forall (s :: S).
  Term s PCurrencySymbol ->
  Term s PCurrencySymbol ->
  [Term s PTxOutRef] ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  TermCont s (Term s PUnit)
noNftGuard stateNftCs listNftCs protectedInputs inputs = do
  pguardC
    "noNftGuard: unallowed input in the transaction contains state or list\
    \ NFT"
    $ pflip pall inputs . inputPredicate $ \outRef val ->
      let equalToSome :: [Term s PTxOutRef] -> Term s PBool
          equalToSome = porList . fmap (\o -> pdata outRef #== pdata o)
       in equalToSome protectedInputs #|| hasNoNft stateNftCs listNftCs val
  pure punit

-- Check that the token was burnt once
burnCheck ::
  forall (s :: S).
  Term s PCurrencySymbol ->
  Term s PTokenName ->
  Term s PValue ->
  Term s PBool
burnCheck cs tn mintVal = burnCheck' # cs # tn # mintVal
  where
    burnCheck' ::
      Term s (PCurrencySymbol :--> PTokenName :--> PValue :--> PBool)
    burnCheck' = phoistAcyclic $
      plam $ \cs tn mintVal ->
        oneWith
          # (peq # cs)
          # (peq # tn)
          # (plam $ \n -> n #== -1)
          # mintVal
