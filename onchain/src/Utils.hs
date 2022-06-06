module Utils (
  parseStakingDatum,
  peq,
  pneq,
  pxor,
  plt,
  ple,
  pge,
  pgt,
  ptrue,
  pfalse,
  punit,
  pifC,
  pnestedIf,
  pandList,
  porList,
  pfind,
  pmapMaybe,
  ppartition,
  pfstData,
  ppairData,
  psndData,
  ptraceBool,
  getTokenName,
  getTokenCount,
  oneOf,
  noneOf,
  allWith,
  someWith,
  oneWith,
  pletC,
  pletDataC,
  pmatchC,
  pconstantC,
  guardC,
  ptryFromData,
  ptryFromUndata,
  getCs,
  getInput,
  getDatum,
  getDatumHash,
  getContinuingOutputWithNFT,
  getOutputSignedBy,
  getTokenTotal,
  getOnlySignatory,
  signedBy,
  signedOnlyBy,
  pconst,
  pflip,
  toPBool,
  mkPubKeyCredential,
  (>:),
  PTxInfoFields,
  PTxInfoHRec,
  PBondedPoolParamsHRec,
  PBondedPoolParamsFields,
  PTxInInfoFields,
  PEntryFields,
  PEntryHRec,
  PTxInInfoHRec,
  HField,
) where

import GHC.TypeLits (Symbol)
import SingularityNet.Natural(Natural(Natural))
import PNatural (PNatRatio, PNatural(PNatural), PNonNegative ((#+)))
import PTypes (PAssetClass, passetClass)
import Plutarch.Api.V1 (
  PAddress,
  PCurrencySymbol,
  PDCert,
  PDatum,
  PDatumHash,
  PMap,
  PMaybeData (PDJust, PDNothing),
  PPOSIXTime,
  PPOSIXTimeRange,
  PPubKeyHash,
  PScriptPurpose (PMinting, PSpending),
  PStakingCredential,
  PTokenName,
  PTuple,
  PTxId,
  PValue,
  PCredential(PPubKeyCredential),
  ptuple,
 )
import Plutarch.Api.V1.Tx (PTxInInfo, PTxOut, PTxOutRef)
import Plutarch.Bool (pand, por)
import Plutarch.Builtin (pforgetData)
import Plutarch.DataRepr (HRec)
import Plutarch.DataRepr.Internal.Field (Labeled)
import Plutarch.Lift (
  PLifted,
  PUnsafeLiftDecl,
 )
import Plutarch.TryFrom (PTryFrom, ptryFrom)

import UnbondedStaking.PTypes (PBoolData (PDFalse, PDTrue))

-- Term-level boolean functions
peq :: forall (s :: S) (a :: PType). PEq a => Term s (a :--> a :--> PBool)
peq = phoistAcyclic $ plam $ \x y -> x #== y

pneq :: forall (s :: S) (a :: PType). PEq a => Term s (a :--> a :--> PBool)
pneq = phoistAcyclic $ plam $ \x y -> pnot #$ x #== y

pxor :: forall (s :: S). Term s (PBool :--> PBool :--> PBool)
pxor = phoistAcyclic $ plam $ \x y -> pnot #$ pdata x #== pdata y

plt ::
  forall (s :: S) (a :: PType).
  POrd a =>
  Term s (a :--> a :--> PBool)
plt = phoistAcyclic $ plam $ \lim x -> x #< lim

ple ::
  forall (s :: S) (a :: PType).
  POrd a =>
  Term s (a :--> a :--> PBool)
ple = phoistAcyclic $ plam $ \lim x -> x #<= lim

pge ::
  forall (s :: S) (a :: PType).
  POrd a =>
  Term s (a :--> a :--> PBool)
pge = phoistAcyclic $ plam $ \lim x -> pnot #$ x #< lim

pgt ::
  forall (s :: S) (a :: PType).
  POrd a =>
  Term s (a :--> a :--> PBool)
pgt = phoistAcyclic $ plam $ \lim x -> pnot #$ x #<= lim

ptrue :: forall (s :: S). Term s PBool
ptrue = pconstant True

pfalse :: forall (s :: S). Term s PBool
pfalse = pconstant False

punit :: forall (s :: S). Term s PUnit
punit = pconstant ()

-- Functions for checking conditions

{- | Build nested conditions. It takes an association list of conditions and
 and results. It evaluates the conditions in order: whenever a condition
 is satisfied, its associated result is returned.

 Expands to a nested `pif` structure.
-}
pnestedIf ::
  forall (s :: S) (a :: PType).
  [(Term s PBool, Term s a)] ->
  Term s a ->
  Term s a
pnestedIf [] def = def
pnestedIf ((cond, x) : conds) def = pif cond x $ pnestedIf conds def

-- `and` function for `[Term s PBool]`. Expands to a sequence of `(#&&)`
pandList :: forall (s :: S). [Term s PBool] -> Term s PBool
pandList = foldr (#&&) ptrue

-- `or` function for `[Term s PBool]`. Expands to a sequence of `(#||)`
porList :: forall (s :: S). [Term s PBool] -> Term s PBool
porList = foldr (#||) pfalse

-- | Lifts `pif` result into the `TermCont` monad
pifC ::
  forall (s :: S) (a :: PType).
  Term s PBool ->
  Term s a ->
  Term s a ->
  TermCont s (Term s a)
pifC cond trueResult = pure . pif cond trueResult

-- | A pair builder useful for avoiding parentheses
infixr 1 >:

(>:) :: forall (a :: Type) (b :: Type). a -> b -> (a, b)
(>:) = (,)

-- Convenient functions for accessing a pair's elements

-- | Access both elements of a `PBuiltinPair` and apply `pfromData` to each
ppairData ::
  forall (s :: S) (a :: PType) (b :: PType).
  (PIsData a, PIsData b) =>
  Term s (PBuiltinPair (PAsData a) (PAsData b)) ->
  TermCont s (Term s a, Term s b)
ppairData p = pure (pfromData $ pfstBuiltin # p, pfromData $ psndBuiltin # p)

-- | Access the first element of a `PBuiltinPair` and apply `pfromData`
pfstData ::
  forall (s :: S) (a :: PType) (b :: PType).
  PIsData a =>
  Term s (PBuiltinPair (PAsData a) b) ->
  Term s a
pfstData x = pfromData $ pfstBuiltin # x

-- | Access the second element of a `PBuiltinPair` and apply `pfromData`
psndData ::
  forall (s :: S) (a :: PType) (b :: PType).
  PIsData b =>
  Term s (PBuiltinPair a (PAsData b)) ->
  Term s b
psndData x = pfromData $ psndBuiltin # x

-- Functions for lists

{- | Returns the first element that matches the predicate. If no element matches
 the predicate, it throws an error
-}
pfind ::
  forall (s :: S) (a :: PType).
  PIsData a =>
  Term s (PAsData a :--> PBool) ->
  Term s (PBuiltinList (PAsData a)) ->
  TermCont s (Term s (PAsData a))
pfind pred ls = pure $ (pfix #$ go # pred) # ls
  where
    go ::
      forall (s :: S) (a :: PType).
      PIsData a =>
      Term
        s
        ( (PAsData a :--> PBool)
            :--> (PBuiltinList (PAsData a) :--> PAsData a)
            :--> PBuiltinList (PAsData a)
            :--> PAsData a
        )
    go = phoistAcyclic $
      plam $ \pred self ls -> pmatch ls $ \case
        PNil -> ptraceError "pfind: could not find element in list"
        PCons x xs -> pif (pred # x) x (self # xs)

-- | Equivalent to `mapMaybe` for plutarch
pmapMaybe ::
  forall (s :: S) (a :: PType) (b :: PType).
  (PUnsafeLiftDecl a, PUnsafeLiftDecl b) =>
  Term s (a :--> PMaybe b) ->
  Term s (PBuiltinList a) ->
  Term s (PBuiltinList b)
pmapMaybe f as = pfix # pmapMaybe' # f # as
  where
    pmapMaybe' ::
      Term
        s
        ( ((a :--> PMaybe b) :--> PBuiltinList a :--> PBuiltinList b)
            :--> (a :--> PMaybe b)
            :--> PBuiltinList a
            :--> PBuiltinList b
        )
    pmapMaybe' = phoistAcyclic $
      plam $ \self f ls -> pmatch ls $ \case
        PCons x xs -> pmatch (f # x) $ \case
          PJust x' -> pcon $ PCons x' $ self # f # xs
          PNothing -> self # f # xs
        PNil -> pcon PNil

{- | Returns the pair of lists of elements that match and don't match the
 predicate
-}
ppartition ::
  forall (s :: S) (a :: PType).
  PIsData a =>
  Term s (a :--> PBool) ->
  Term s (PBuiltinList (PAsData a)) ->
  Term s (PTuple (PBuiltinList (PAsData a)) (PBuiltinList (PAsData a)))
ppartition pred ls = pfix # go # pred # ls # pnil # pnil
  where
    go ::
      forall (s :: S) (a :: PType).
      PIsData a =>
      Term
        s
        ( ( (a :--> PBool)
              :--> PBuiltinList (PAsData a)
              :--> PBuiltinList (PAsData a)
              :--> PBuiltinList (PAsData a)
              :--> PTuple (PBuiltinList (PAsData a)) (PBuiltinList (PAsData a))
          )
            :--> (a :--> PBool)
            :--> (PBuiltinList (PAsData a))
            :--> (PBuiltinList (PAsData a))
            :--> (PBuiltinList (PAsData a))
            :--> (PTuple (PBuiltinList (PAsData a)) (PBuiltinList (PAsData a)))
        )
    go = phoistAcyclic $
      plam $ \self pred trueElems falseElems ls ->
        pmatch ls $ \case
          PNil -> ptuple # pdata trueElems # pdata falseElems
          PCons x xs ->
            pif
              (pred # pfromData x)
              (go # self # pred # (pcons # x # trueElems) # falseElems # xs)
              (go # self # pred # trueElems # (pcons # x # falseElems) # xs)

-- Functions for debugging

-- | Print one message or the other depending on the boolean condition.
ptraceBool ::
  forall (s :: S).
  Term s PString ->
  -- |^ The common message
  Term s PString ->
  -- |^ The `ptrue` part
  Term s PString ->
  -- |^ The `pfalse` part
  Term s PBool ->
  Term s PBool
ptraceBool common trueMsg falseMsg cond = ptrace msg cond
  where
    msg :: Term s PString
    msg = pif cond (common <> " " <> trueMsg) (common <> " " <> falseMsg)

-- Functions for working with `PValue`s

-- | Result of `pto v`, where `v :: Term s PValue`
type PValueOuter =
  PBuiltinList
    ( PBuiltinPair
        (PAsData PCurrencySymbol)
        (PAsData (PMap PTokenName PInteger))
    )

{- | Retrieves the `PTokenName` for a given `PCurrencySymbol`. It fetches the
 only the first token and it fails if no token is present
-}
getTokenName ::
  forall (s :: S).
  Term s PCurrencySymbol ->
  Term s PValue ->
  Term s PAssetClass
getTokenName cs val = pfix # getTokenName' # cs # pto (pto val)
  where
    getTokenName' ::
      forall (s :: S).
      Term
        s
        ( (PCurrencySymbol :--> PValueOuter :--> PAssetClass)
            :--> PCurrencySymbol
            :--> PValueOuter
            :--> PAssetClass
        )
    getTokenName' = phoistAcyclic $
      plam $ \self listCs val ->
        pmatch val $ \case
          PCons csMap vals -> unTermCont $ do
            (cs, tnMap) <- ppairData csMap
            pure $
              pif
                (pnot #$ cs #== listCs)
                (self # listCs # vals)
                $ pmatch (pto tnMap) $ \case
                  PCons tnAmt _ -> passetClass # listCs # pfstData tnAmt
                  PNil -> ptraceError "getTokenName: empty tnMap"
          PNil ->
            ptraceError
              "getTokenName: the token with given \
              \CurrencySymbol was not found"

-- | Gets the amount of `PAssetClass` found in the passed `PValue`
getTokenCount ::
  forall (s :: S).
  Term s PAssetClass ->
  Term s PValue ->
  Term s PNatural
getTokenCount ac val =
  getTokenCount' # ac # pto (pto val)
  where getTokenCount' ::
          forall (s :: S).
          Term s (
            PAssetClass :-->
            PValueOuter :-->
            PNatural)
        getTokenCount' = phoistAcyclic $ plam $ \ac' val -> unTermCont $ do
          ac <- tcont . pletFields @'["currencySymbol", "tokenName"] $ ac'
          let cs = pfromData ac.currencySymbol
              tn = pfromData ac.tokenName 
          pure $ pfoldl # (foldCsMap # cs # tn) # pconstant (Natural 0) # val
        foldCsMap :: forall (s :: S) .
          Term s (
            PCurrencySymbol :-->
            PTokenName :-->
            PNatural :-->
            PBuiltinPair (PAsData PCurrencySymbol)
                         (PAsData (PMap PTokenName PInteger)) :-->
            PNatural)
        foldCsMap = phoistAcyclic $ plam $ \cs tn acc pair ->
          runTermCont (ppairData pair) $ \(cs', tnMap) ->
            pif (cs #== cs')
                (pfoldl # (foldTnMap # tn) # acc # pto tnMap)
                acc
        foldTnMap :: forall (s :: S).
          Term s (
            PTokenName :-->
            PNatural :-->
            PBuiltinPair (PAsData PTokenName)
                         (PAsData PInteger) :-->
            PNatural
          )
        foldTnMap = phoistAcyclic $ plam $ \tn acc pair ->
          runTermCont (ppairData pair) $ \(tn', n) ->
            pif (tn #== tn')
                (acc #+ (pcon $ PNatural n))
                acc

-- go self ls cont = pmatch ls $ \case
--  PNil -> pconstant False
--  PCons p ps -> boolOp # (cont # p) #$ self # ps # cont

-- Functions for evaluating predicates on `PValue`s

{- | Returns `PTrue` if the token described by its `PCurrencySymbol` and
 `PTokenName` is present only *once* in the `PValue`.
-}
oneOf ::
  forall (s :: S).
  Term
    s
    ( PCurrencySymbol :--> PTokenName :--> PValue :--> PBool
    )
oneOf = phoistAcyclic $
  plam $ \cs tn val ->
    oneWith
      # (peq # cs)
      # (peq # tn)
      # (ple # 1)
      #$ val

{- | Returns `PTrue` if the token described by its `PCurrencySymbol` and
 `PTokenName` is *not* present in the `PValue
-}
noneOf ::
  forall (s :: S).
  Term
    s
    ( PCurrencySymbol :--> PTokenName :--> PValue :--> PBool
    )
noneOf = phoistAcyclic $
  plam $ \cs tn val ->
    pnot #$ someWith
      # (peq # cs)
      # (peq # tn)
      # (pge # 1)
      # val

{- | Returns `PTrue` if *some* token (at least one) present in `PValue`
 satisfies *all* the predicates given as parameters
-}
someWith ::
  forall (s :: S).
  Term
    s
    ( (PCurrencySymbol :--> PBool)
        :--> (PTokenName :--> PBool)
        :--> (PInteger :--> PBool)
        :--> PValue
        :--> PBool
    )
someWith = phoistAcyclic $
  plam $ \csPred tnPred nPred ->
    tokenPredicate por csPred tnPred nPred

{- | Returns `PTrue` if *all* tokens present in `PValue` satisfy *all* of
  the predicates given as parameters
-}
allWith ::
  forall (s :: S).
  Term
    s
    ( (PCurrencySymbol :--> PBool)
        :--> (PTokenName :--> PBool)
        :--> (PInteger :--> PBool)
        :--> PValue
        :--> PBool
    )
allWith = phoistAcyclic $
  plam $ \csPred tnPred nPred ->
    tokenPredicate pand csPred tnPred nPred

{- | Returns `PTrue` if only *one* token present in `PValue` satisfies *all* the
 predicates given as parameters
-}
oneWith ::
  forall (s :: S).
  Term
    s
    ( (PCurrencySymbol :--> PBool)
        :--> (PTokenName :--> PBool)
        :--> (PInteger :--> PBool)
        :--> PValue
        :--> PBool
    )
oneWith = phoistAcyclic $
  plam $ \csPred tnPred nPred ->
    tokenPredicate' pxor csPred tnPred nPred

{- | Assigns a boolean to each token in the value based on the the result of:

 > csPred cs `pand` tnPred cs tn `pand` amountPred cs tn n

 where each token is a tuple `(cs, tn, n)`.

 Then, all the booleans are combined according to the boolean operator `boolOp`.

 This allows short-circuiting evaluation (e.g: a failure in `csPred`
 avoids evaluating the remaining predicates). `op` can be any binary boolean
 operator, like `pxor` (if only one token needs to satisfy all predicates) or
 `pand` (if all tokens must satisfy the predicates).
-}
tokenPredicate ::
  forall (s :: S).
  Term s (PBool :--> PDelayed PBool :--> PDelayed PBool) ->
  Term s (PCurrencySymbol :--> PBool) ->
  Term s (PTokenName :--> PBool) ->
  Term s (PInteger :--> PBool) ->
  Term s (PValue :--> PBool)
tokenPredicate boolOp csPred tnPred nPred = plam $ \val -> unTermCont $ do
  let csMap = pto $ pto val
  csTnPair <- tcont $ matchPair boolOp csMap
  tnMap <- tcont $ evalCs csPred csTnPair
  tnAmountPair <- tcont $ matchPair boolOp $ pto tnMap
  pure $ evalTnAndAmount tnPred nPred tnAmountPair

{- | Same as `tokenPredicate`, but `boolOp` is strict on both arguments. This
 means that the function cannot short-circuit evaluation.
-}
tokenPredicate' ::
  forall (s :: S).
  Term s (PBool :--> PBool :--> PBool) ->
  Term s (PCurrencySymbol :--> PBool) ->
  Term s (PTokenName :--> PBool) ->
  Term s (PInteger :--> PBool) ->
  Term s (PValue :--> PBool)
tokenPredicate' boolOp csPred tnPred nPred = plam $ \val -> unTermCont $ do
  -- Map of CurrencySymbols
  let csMap = pto $ pto val
  csTnPair <- tcont $ matchPair' boolOp csMap
  tnMap <- tcont $ evalCs csPred csTnPair
  tnAmountPair <- tcont $ matchPair' boolOp $ pto tnMap
  pure $ evalTnAndAmount tnPred nPred tnAmountPair

-- Auxiliary functions for `tokenPredicate` and `tokenPredicate'`

-- Pattern match on a list of pairs, evaluate the continuation on each pair
-- and combine the results with `boolOp`.
matchPair ::
  forall (s :: S) (a :: PType).
  PLift a =>
  Term s (PBool :--> PDelayed PBool :--> PDelayed PBool) ->
  Term s (PBuiltinList a) ->
  (Term s a -> Term s PBool) ->
  Term s PBool
matchPair boolOp ls cont = (pfix # plam go) # ls # plam cont
  where
    -- Recurring function for the Y combinator
    go ::
      forall (a :: PType).
      PLift a =>
      Term s (PBuiltinList a :--> (a :--> PBool) :--> PBool) ->
      Term s (PBuiltinList a) ->
      Term s (a :--> PBool) ->
      Term s PBool
    go self ls cont = pmatch ls $ \case
      PNil -> pconstant False
      PCons p ps -> pforce $ boolOp # (cont # p) # pdelay (self # ps # cont)

-- Strict version of `matchPair`
matchPair' ::
  forall (s :: S) (a :: PType).
  PLift a =>
  Term s (PBool :--> PBool :--> PBool) ->
  Term s (PBuiltinList a) ->
  (Term s a -> Term s PBool) ->
  Term s PBool
matchPair' boolOp ls cont = (pfix # plam go) # ls # plam cont
  where
    -- Recurring function for the Y combinator
    go ::
      forall (a :: PType).
      PLift a =>
      Term s (PBuiltinList a :--> (a :--> PBool) :--> PBool) ->
      Term s (PBuiltinList a) ->
      Term s (a :--> PBool) ->
      Term s PBool
    go self ls cont = pmatch ls $ \case
      PNil -> pconstant False
      PCons p ps -> boolOp # (cont # p) #$ self # ps # cont

-- Evaluate condition on CurrencySymbol
evalCs ::
  forall (s :: S) (a :: PType).
  PIsData a =>
  Term s (PCurrencySymbol :--> PBool) ->
  Term s (PBuiltinPair (PAsData PCurrencySymbol) (PAsData a)) ->
  (Term s a -> Term s PBool) ->
  (Term s PBool)
evalCs csPred pair cont =
  pif
    (csPred # pfstData pair)
    (ptrace "evalCs OK" $ cont $ psndData pair)
    ( ptrace "predicate on CurrencySymbol not satisfied" $
        pconstant False
    )

-- Evaluate conditions on TokenName and token amount
evalTnAndAmount ::
  forall (s :: S).
  Term s (PTokenName :--> PBool) ->
  Term s (PInteger :--> PBool) ->
  Term s (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)) ->
  Term s PBool
evalTnAndAmount tnPred nPred pair =
  pif
    (tnPred # pfstData pair #&& nPred # psndData pair)
    (ptrace "evalTnAndAmount OK" $ pconstant True)
    ( ptrace "predicate on TokenName/amount not satisfied" $
        pconstant False
    )

-- Functions for working with `TermCont`

-- | Makes `a` constant and wraps it in a `TermCont`
pconstantC ::
  forall (s :: S) (a :: PType).
  PUnsafeLiftDecl a =>
  PLifted a ->
  TermCont s (Term s a)
pconstantC = pure . pconstant

-- | `pmatch` for the `TermCont` monad
pmatchC ::
  forall (s :: S) (a :: PType).
  PlutusType a =>
  Term s a ->
  TermCont s (a s)
pmatchC = tcont . pmatch

-- | `plet` for the `TermCont` monad
pletC :: forall (s :: S) (a :: PType). Term s a -> TermCont s (Term s a)
pletC = tcont . plet

-- | Converts from `Data` and binds result with `pletC`
pletDataC ::
  forall (s :: S) (a :: PType).
  PIsData a =>
  Term s (PAsData a) ->
  TermCont s (Term s a)
pletDataC = pletC . pfromData

-- | Boolean guard for the `TermCont` monad
guardC ::
  forall (s :: S).
  Term s PString ->
  Term s PBool ->
  TermCont s (Term s PUnit)
guardC errMsg cond = pure $ pif cond (pconstant ()) $ ptraceError errMsg

-- Functions for working with the `PTryFrom` class

-- | Copied from plutarch-extra
ptryFromData ::
  forall (a :: PType) (s :: S).
  PTryFrom PData (PAsData a) =>
  Term s PData ->
  TermCont s (Term s (PAsData a))
ptryFromData x = fst <$> tcont (ptryFrom @(PAsData a) x)

ptryFromUndata ::
  forall (a :: PType) (s :: S).
  (PIsData a, PTryFrom PData (PAsData a)) =>
  Term s PData ->
  TermCont s (Term s a)
ptryFromUndata x'' = do
  x' <- ptryFromData @a x''
  pure $ pfromData x'

-- Helper functions for retrieving data in a validator

-- | Gets the currency symbol of the script (equivalent to ownCurrencySymbol)
getCs ::
  forall (s :: S).
  Term s PScriptPurpose ->
  TermCont s (Term s PCurrencySymbol)
getCs purpose = pure $
  pmatch purpose $ \case
    PMinting cs -> pfield @"_0" # cs
    _ -> ptraceError "getCs: not a minting transaction"

{- | Gets the input being spent. If not available, it will fail with
 an error.
-}
getInput ::
  forall (s :: S).
  Term s PScriptPurpose ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  TermCont s (Term s PTxInInfo)
getInput purpose txInInfos = pure $ getInput' # purpose # txInInfos
  where
    getInput' ::
      forall (s :: S).
      Term
        s
        ( PScriptPurpose
            :--> PBuiltinList (PAsData PTxInInfo)
            :--> PTxInInfo
        )
    getInput' = phoistAcyclic $
      plam $ \purpose txInInfos -> unTermCont $ do
        let inputOutRef = getSpendingRef purpose
        pfromData <$> pfind (predicate # inputOutRef) txInInfos
    predicate ::
      forall (s :: S).
      Term s (PTxOutRef :--> PAsData PTxInInfo :--> PBool)
    predicate = phoistAcyclic $
      plam $ \inputOutRef txInInfo ->
        pdata inputOutRef #== pdata (pfield @"outRef" # pfromData txInInfo)
    getSpendingRef ::
      forall (s :: S).
      Term s PScriptPurpose ->
      Term s PTxOutRef
    getSpendingRef = flip pmatch $ \case
      PSpending outRef -> pfield @"_0" # outRef
      _ ->
        ptraceError
          "getInput: cannot get input because tx is not of \
          \spending type"

{- | Gets the continuing output that shares the same address and contains the
 the given NFT. If no such output exists, it will fail with an error.
-}
getContinuingOutputWithNFT ::
  forall (s :: S).
  Term s PAddress ->
  Term s PAssetClass ->
  Term s (PBuiltinList (PAsData PTxOut)) ->
  TermCont s (Term s PTxOut)
getContinuingOutputWithNFT addr ac outputs =
  pure $ getContinuingOutputWithNFT' # addr # ac # outputs
  where
    getContinuingOutputWithNFT' ::
      forall (s :: S).
      Term
        s
        ( PAddress
            :--> PAssetClass
            :--> PBuiltinList (PAsData PTxOut)
            :--> PTxOut
        )
    getContinuingOutputWithNFT' = phoistAcyclic $
      plam $ \addr ac outputs ->
        unTermCont $
          pfromData
            <$> pfind
              (sameAddrAndNFT addr ac)
              outputs
    sameAddrAndNFT ::
      forall (s :: S).
      Term s PAddress ->
      Term s PAssetClass ->
      Term s (PAsData PTxOut :--> PBool)
    sameAddrAndNFT addr ac = plam $ \output -> unTermCont $ do
      outputF <- tcont $ pletFields @'["address", "value"] output
      acF <- tcont $ pletFields @'["currencySymbol", "tokenName"] ac
      pure $
        pdata outputF.address #== pdata addr
          #&& oneOf # acF.currencySymbol # acF.tokenName # outputF.value
          
getOutputSignedBy ::
  forall (s :: S).
  Term s PPubKeyHash ->
  Term s (PBuiltinList (PAsData PTxOut)) ->
  TermCont s (Term s PTxOut)
getOutputSignedBy pkh outputs = do
  credential <- pletC $ mkPubKeyCredential pkh
  output <- flip pfind outputs $ plam $ \output -> unTermCont $ do
    let credential' = pfield @"credential" #$ pfield @"address" # output
    pure $ pdata credential #== pdata credential'
  pure $ pfromData output
  
getTokenTotal ::
  forall (s :: S).
  Term s PAssetClass ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s PNatural
getTokenTotal ac inputs =
  pfoldl # (sumAmount # ac) # pconstant (Natural 0) # inputs
  where sumAmount :: Term s (
          PAssetClass :-->
          PNatural :-->
          PAsData PTxInInfo :-->
          PNatural)
        sumAmount = phoistAcyclic $ plam $ \ac acc input ->
          let val = pfield @"value" #$ pfield @"resolved" # pfromData input
          in acc #+ getTokenCount ac val
          
{- | Gets the `DatumHash` from a `PTxOut`. If not available, it will fail with
 an error.
-}
getDatumHash ::
  forall (s :: S).
  Term s PTxOut ->
  TermCont s (Term s PDatumHash)
getDatumHash txOut = pure $ getDatumHash' # txOut
  where
    getDatumHash' :: forall (s :: S). Term s (PTxOut :--> PDatumHash)
    getDatumHash' = phoistAcyclic $
      plam $ \txOut ->
        pmatch (pfield @"datumHash" # txOut) $ \case
          PDJust datumHash' -> pfield @"_0" # datumHash'
          PDNothing _ ->
            ptraceError
              "getDatumHash: could not find datum hash \
              \in txOut"

{- | Gets `Datum` by its `DatumHash`. If not available, it will fail with an
 error.
-}
getDatum ::
  forall (s :: S).
  Term s PDatumHash ->
  Term s (PBuiltinList (PAsData (PTuple PDatumHash PDatum))) ->
  TermCont s (Term s PDatum)
getDatum datHash dats = pure $ getDatum' # datHash # dats
  where
    getDatum' ::
      forall (s :: S).
      Term
        s
        ( PDatumHash
            :--> ( PBuiltinList (PAsData (PTuple PDatumHash PDatum))
                    :--> PDatum
                 )
        )
    getDatum' = phoistAcyclic $
      plam $ \datHash dats -> unTermCont $ do
        datHashAndDat <- pfind (checkHash # datHash) dats
        pure $ pfield @"_1" # datHashAndDat
    checkHash ::
      forall (s :: S).
      Term s (PDatumHash :--> PAsData (PTuple PDatumHash PDatum) :--> PBool)
    checkHash = phoistAcyclic $
      plam $ \datHash tup ->
        pfield @"_0" # tup #== datHash

-- | Gets the only signatory of the TX or fail
getOnlySignatory ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PPubKeyHash)) ->
  TermCont s (Term s PPubKeyHash)
getOnlySignatory ls = pure . pmatch ls $ \case
  PCons pkh ps ->
    pif
      (pnull # ps)
      (pfromData pkh)
      (ptraceError "getSignatory: transaction has more than one signatory")
  PNil -> ptraceError "getSignatory: empty list of signatories"

-- Functions for checking signatures

-- | Verifies that a signature is in the signature list
signedBy ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PPubKeyHash)) ->
  Term s PPubKeyHash ->
  Term s PBool
signedBy ls pkh = pelem # pdata pkh # ls

-- | Verifies that a signature is the *only* one in the list
signedOnlyBy ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PPubKeyHash)) ->
  Term s PPubKeyHash ->
  Term s PBool
signedOnlyBy ls pkh = signedBy ls pkh #&& plength # ls #== 1

-- Useful type family for reducing boilerplate in HRec types
type family HField (s :: S) (field :: Symbol) (ptype :: PType) where
  HField s field ptype = Labeled field (Term s (PAsData ptype))

-- | HRec with all of `PTxInfo`'s fields
type PTxInfoHRec (s :: S) =
  HRec
    '[ HField s "inputs" (PBuiltinList (PAsData PTxInInfo))
     , HField s "outputs" (PBuiltinList (PAsData PTxOut))
     , HField s "fee" PValue
     , HField s "mint" PValue
     , HField s "dcert" (PBuiltinList (PAsData PDCert))
     , HField s "wdrl" (PBuiltinList (PAsData (PTuple PStakingCredential PInteger)))
     , HField s "validRange" PPOSIXTimeRange
     , HField s "signatories" (PBuiltinList (PAsData PPubKeyHash))
     , HField s "data" (PBuiltinList (PAsData (PTuple PDatumHash PDatum)))
     , HField s "id" PTxId
     ]

-- | HRec with all of `PBondedPoolParams`'s fields
type PBondedPoolParamsHRec (s :: S) =
  HRec
    '[ HField s "iterations" PNatural
     , HField s "start" PPOSIXTime
     , HField s "end" PPOSIXTime
     , HField s "userLength" PPOSIXTime
     , HField s "bondingLength" PPOSIXTime
     , HField s "interest" PNatRatio
     , HField s "minStake" PNatural
     , HField s "maxStake" PNatural
     , HField s "admin" PPubKeyHash
     , HField s "bondedAssetClass" PAssetClass
     , HField s "nftCs" PCurrencySymbol
     , HField s "assocListCs" PCurrencySymbol
     ]

-- | HRec with all of `PTxInInfo`'s fields
type PTxInInfoHRec (s :: S) =
  HRec
    '[ HField s "outRef" PTxOutRef
     , HField s "resolved" PTxOut
     ]

-- | HRec with all of `PEntry`'s fields
type PEntryHRec (s :: S) =
  HRec
    '[ HField s "key" PByteString
     , HField s "newDeposit" PNatural
     , HField s "deposited" PNatural
     , HField s "staked" PNatural
     , HField s "rewards" PNatRatio
     , HField s "next" (PMaybeData PByteString)
     ]

-- | Type level list with all of `PBondedPoolParams's field names
type PBondedPoolParamsFields =
  '[ "iterations"
   , "start"
   , "end"
   , "userLength"
   , "bondingLength"
   , "interest"
   , "minStake"
   , "maxStake"
   , "admin"
   , "bondedAssetClass"
   , "nftCs"
   , "assocListCs"
   ]

-- | Type level list with all of `PTxInfo`'s fields
type PTxInfoFields =
  '[ "inputs"
   , "outputs"
   , "fee"
   , "mint"
   , "dcert"
   , "wdrl"
   , "validRange"
   , "signatories"
   , "data"
   , "id"
   ]

-- | Type level list with all of `PTxInInfo`'s fields
type PTxInInfoFields =
  '[ "outRef"
   , "resolved"
   ]

-- | Type level list with all of `PEntry`'s fields
type PEntryFields =
  '[ "key"
   , "newDeposit"
   , "deposited"
   , "staked"
   , "rewards"
   , "next"
   ]

-- Other functions

{- | Returns a new Plutarch function that ignores the first paramter and returns
 `b`
-}
pconst ::
  forall (s :: S) (a :: PType) (b :: PType).
  Term s b ->
  Term s (a :--> b)
pconst b = plam $ const b

{- | Flips the order of the function's parameters for convenience, no plutarch
 function is created
-}
pflip ::
  forall (s :: S) (a :: PType) (b :: PType) (c :: PType).
  Term s (a :--> b :--> c) ->
  Term s b ->
  Term s a ->
  Term s c
pflip f b a = f # a # b

-- | Returns the staking datum record with the provided type
parseStakingDatum ::
  forall (a :: PType) (s :: S).
  PIsData a =>
  PTryFrom PData (PAsData a) =>
  Term s PDatum ->
  TermCont s (Term s a)
parseStakingDatum =
  ptryFromUndata @a . pforgetData . pdata

-- Helper functions for working with Plutarch synonyms types

-- | Returns a Plutarch-level bool from a `PBoolData` type
toPBool :: forall (s :: S). Term s (PBoolData :--> PBool)
toPBool = phoistAcyclic $
  plam $ \pbd ->
    pmatch pbd $ \case
      PDFalse _ -> pfalse
      PDTrue _ -> ptrue

-- | Build a `PCredential` from a `PPubKeyHash`
mkPubKeyCredential :: forall (s :: S). Term s PPubKeyHash -> Term s PCredential
mkPubKeyCredential pkh = pcon . PPubKeyCredential $ pdcons # pdata pkh # pdnil