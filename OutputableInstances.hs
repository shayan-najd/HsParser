{-# LANGUAGE ScopedTypeVariables, ExplicitForAll #-}
module OutputableInstances (pprHsContext,fsFromRole) where

import U.Outputable

import Language.Haskell.Syntax.HsSyn
import Language.Haskell.Syntax.ForeignCall
import Language.Haskell.Syntax.FieldLabel
import Language.Haskell.Syntax.BasicTypes
import Language.Haskell.Syntax.BooleanFormula
import Language.Haskell.Syntax.SrcLoc

import RdrName
import OccName
import ApiAnnotation
import Language.Haskell.Syntax.Module

import Language.Haskell.Utility.FastString
import U.Panic
import Language.Haskell.Utility.Util
import Language.Haskell.Utility.Bag
import U.Unique
import U.UniqFM
import U.DynFlags
import U.OrdList

import Data.Maybe
import Data.List
import Data.Ord

-- ApiAnnotation
-------------------------------------------------------------------------------

instance Outputable AnnKeywordId where
  ppr x = text (show x)

instance Outputable AnnotationComment where
  ppr x = text (show x)

-- BasicTypes
-------------------------------------------------------------------------------

instance Outputable CompilerPhase where
   ppr (Phase n)    = int n
   ppr InitialPhase = text "InitialPhase"

instance Outputable FunctionOrData where
    ppr IsFunction = text "(function)"
    ppr IsData     = text "(data)"

instance Outputable WarningTxt where
    ppr (WarningTxt    _ ws)
                         = doubleQuotes (vcat (map (ftext . sl_fs . unLoc) ws))
    ppr (DeprecatedTxt _ ds)
                         = text "Deprecated:" <+>
                           doubleQuotes (vcat (map (ftext . sl_fs . unLoc) ds))

instance Outputable Fixity where
    ppr (Fixity _ prec dir) = hcat [ppr dir, space, int prec]

instance Outputable FixityDirection where
    ppr InfixL = text "infixl"
    ppr InfixR = text "infixr"
    ppr InfixN = text "infix"

instance Outputable TopLevelFlag where
  ppr TopLevel    = text "<TopLevel>"
  ppr NotTopLevel = text "<NotTopLevel>"

instance Outputable Boxity where
  ppr Boxed   = text "Boxed"
  ppr Unboxed = text "Unboxed"

instance Outputable RecFlag where
  ppr Recursive    = text "Recursive"
  ppr NonRecursive = text "NonRecursive"

instance Outputable Origin where
  ppr FromSource  = text "FromSource"
  ppr Generated   = text "Generated"

instance Outputable OverlapFlag where
   ppr flag = ppr (overlapMode flag) <+> pprSafeOverlap (isSafeOverlap flag)

instance Outputable OverlapMode where
   ppr (NoOverlap    _) = empty
   ppr (Overlappable _) = text "[overlappable]"
   ppr (Overlapping  _) = text "[overlapping]"
   ppr (Overlaps     _) = text "[overlap ok]"
   ppr (Incoherent   _) = text "[incoherent]"

pprSafeOverlap :: Bool -> SDoc
pprSafeOverlap True  = text "[safe]"
pprSafeOverlap False = empty

tupleParens :: TupleSort -> SDoc -> SDoc
tupleParens BoxedTuple      p = parens p
tupleParens UnboxedTuple    p = text "(#" <+> p <+> ptext (sLit "#)")
tupleParens ConstraintTuple p = parens p

instance Outputable OccInfo where
  -- only used for debugging; never parsed.  KSW 1999-07
  ppr NoOccInfo            = empty
  ppr (IAmALoopBreaker ro) = text "LoopBreaker" <> if ro then char '!' else empty
  ppr IAmDead              = text "Dead"
  ppr (OneOcc inside_lam one_branch int_cxt)
        = text "Once" <> pp_lam <> pp_br <> pp_args
        where
          pp_lam | inside_lam = char 'L'
                 | otherwise  = empty
          pp_br  | one_branch = empty
                 | otherwise  = char '*'
          pp_args | int_cxt   = char '!'
                  | otherwise = empty

instance Outputable Activation where
   ppr AlwaysActive       = brackets (text "ALWAYS")
   ppr NeverActive        = brackets (text "NEVER")
   ppr (ActiveBefore _ n) = brackets (char '~' <> int n)
   ppr (ActiveAfter  _ n) = brackets (int n)

instance Outputable RuleMatchInfo where
   ppr ConLike = text "CONLIKE"
   ppr FunLike = text "FUNLIKE"

instance Outputable InlineSpec where
   ppr Inline          = text "INLINE"
   ppr NoInline        = text "NOINLINE"
   ppr Inlinable       = text "INLINABLE"
   ppr EmptyInlineSpec = empty

instance Outputable InlinePragma where
  ppr (InlinePragma { inl_inline = inline, inl_act = activation
                    , inl_rule = info, inl_sat = mb_arity })
    = ppr inline <> pp_act inline activation <+> pp_sat <+> pp_info
    where
      pp_act Inline   AlwaysActive = empty
      pp_act NoInline NeverActive  = empty
      pp_act _        act          = ppr act

      pp_sat | Just ar <- mb_arity = parens (text "sat-args=" <> int ar)
             | otherwise           = empty
      pp_info | isFunLike info = empty
              | otherwise      = ppr info

isFunLike :: RuleMatchInfo -> Bool
isFunLike FunLike = True
isFunLike _            = False


instance Outputable FractionalLit where
  ppr = text . fl_text

-- BooleanFormula
-------------------------------------------------------------------------------

-- Pretty print a BooleanFormula,
-- using the arguments as pretty printers for Var, And and Or respectively
pprBooleanFormula' :: (Rational -> a -> SDoc)
                   -> (Rational -> [SDoc] -> SDoc)
                   -> (Rational -> [SDoc] -> SDoc)
                   -> Rational -> BooleanFormula a -> SDoc
pprBooleanFormula' pprVar pprAnd pprOr = go
  where
  go p (Var x)  = pprVar p x
  go p (And []) = cparen (p > 0) $ empty
  go p (And xs) = pprAnd p (map (go 3 . unLoc) xs)
  go _ (Or  []) = keyword $ text "FALSE"
  go p (Or  xs) = pprOr p (map (go 2 . unLoc) xs)
  go p (Parens x) = go p (unLoc x)

-- Pretty print in source syntax, "a | b | c,d,e"
pprBooleanFormula :: (Rational -> a -> SDoc) -> Rational -> BooleanFormula a -> SDoc
pprBooleanFormula pprVar = pprBooleanFormula' pprVar pprAnd pprOr
  where
  pprAnd p = cparen (p > 3) . fsep . punctuate comma
  pprOr  p = cparen (p > 2) . fsep . intersperse vbar

instance Outputable a => Outputable (BooleanFormula a) where
  pprPrec = pprBooleanFormula pprPrec

-- FieldLabel
-------------------------------------------------------------------------------

instance Outputable a => Outputable (FieldLbl a) where
    ppr fl = ppr (flLabel fl) <> braces (ppr (flSelector fl))


-- ForeignCall
-------------------------------------------------------------------------------

-- We may need more clues to distinguish foreign calls
-- but this simple printer will do for now
instance Outputable ForeignCall where
  ppr (CCall cc)  = ppr cc

instance Outputable Safety where
  ppr PlaySafe = text "safe"
  ppr PlayInterruptible = text "interruptible"
  ppr PlayRisky = text "unsafe"

instance Outputable CCallConv where
  ppr StdCallConv = text "stdcall"
  ppr CCallConv   = text "ccall"
  ppr CApiConv    = text "capi"
  ppr PrimCallConv = text "prim"
  ppr JavaScriptCallConv = text "javascript"

pprCLabelString :: CLabelString -> SDoc
pprCLabelString lbl = ftext lbl


instance Outputable CExportSpec where
  ppr (CExportStatic _ str _) = pprCLabelString str

instance Outputable CCallSpec where
  ppr (CCallSpec fun cconv safety)
    = hcat [ ifPprDebug callconv, ppr_fun fun ]
    where
      callconv = text "{-" <> ppr cconv <> text "-}"

      gc_suf | playSafe safety = text "_GC"
             | otherwise       = empty

      ppr_fun (StaticTarget _ fn mPkgId isFun)
        = text (if isFun then "__pkg_ccall"
                         else "__pkg_ccall_value")
       <> gc_suf
       <+> (case mPkgId of
            Nothing -> empty
            Just pkgId -> ppr pkgId)
       <+> pprCLabelString fn

      ppr_fun DynamicTarget
        = text "__dyn_ccall" <> gc_suf <+> text "\"\""

playSafe :: Safety -> Bool
playSafe PlaySafe = True
playSafe PlayInterruptible = True
playSafe PlayRisky = False

instance Outputable Header where
    ppr (Header _ h) = quotes $ ppr h

instance Outputable CType where
    ppr (CType _ mh (_,ct)) = hDoc <+> ftext ct
        where hDoc = case mh of
                     Nothing -> empty
                     Just h -> ppr h

-- HsLit
-------------------------------------------------------------------------------

instance Outputable HsLit where
    ppr (HsChar _ c)       = pprHsChar c
    ppr (HsCharPrim _ c)   = pprPrimChar c
    ppr (HsString _ s)     = pprHsString s
    ppr (HsStringPrim _ s) = pprHsBytes s
    ppr (HsFloatPrim f)    = ppr f <> primFloatSuffix
    ppr (HsDoublePrim d)   = ppr d <> primDoubleSuffix
    ppr (HsIntPrim _ i)    = pprPrimInt i
    ppr (HsWordPrim _ w)   = pprPrimWord w
    ppr (HsInt64Prim _ i)  = pprPrimInt64 i
    ppr (HsWord64Prim _ w) = pprPrimWord64 w

-- in debug mode, print the expression that it's resolved to, too
instance (OutputableBndr id) => Outputable (HsOverLit id) where
  ppr (OverLit {ol_val=val})
        = ppr val

instance Outputable OverLitVal where
  ppr (HsIntegral _ i)   = integer i
  ppr (HsFractional f)   = ppr f
  ppr (HsIsString _ s)   = pprHsString s

-- HsTypes
-------------------------------------------------------------------------------

instance (OutputableBndr name) => Outputable (HsType name) where
    ppr ty = pprHsType ty

instance Outputable HsTyLit where
    ppr = ppr_tylit

instance (OutputableBndr name) => Outputable (LHsQTyVars name) where
    ppr (HsQTvs { hsq_explicit = tvs }) = interppSP tvs

instance (OutputableBndr name) => Outputable (HsTyVarBndr name) where
    ppr (UserTyVar n)     = ppr n
    ppr (KindedTyVar n k) = parens $ hsep [ppr n, dcolon, ppr k]

instance (Outputable thing) => Outputable (HsImplicitBndrs name thing) where
    ppr (HsIB { hsib_body = ty }) = ppr ty

instance (Outputable thing) => Outputable (HsWildCardBndrs name thing) where
    ppr (HsWC { hswc_body = ty }) = ppr ty

instance Outputable (HsWildCardInfo name) where
    ppr (AnonWildCard)  = char '_'

instance (OutputableBndr name) => Outputable (HsAppType name) where
  ppr = ppr_app_ty TopPrec

pprHsForAll :: (OutputableBndr name)
            => [LHsTyVarBndr name] -> LHsContext name -> SDoc
pprHsForAll = pprHsForAllExtra Nothing

-- | Version of 'pprHsForAll' that can also print an extra-constraints
-- wildcard, e.g. @_ => a -> Bool@ or @(Show a, _) => a -> String@. This
-- underscore will be printed when the 'Maybe SrcSpan' argument is a 'Just'
-- containing the location of the extra-constraints wildcard. A special
-- function for this is needed, as the extra-constraints wildcard is removed
-- from the actual context and type, and stored in a separate field, thus just
-- printing the type will not print the extra-constraints wildcard.
pprHsForAllExtra :: (OutputableBndr name)
                 => Maybe SrcSpan -> [LHsTyVarBndr name] -> LHsContext name
                 -> SDoc
pprHsForAllExtra extra qtvs cxt
  = pprHsForAllTvs qtvs <+> pprHsContextExtra show_extra (unLoc cxt)
  where
    show_extra = isJust extra

pprHsForAllTvs :: (OutputableBndr name) => [LHsTyVarBndr name] -> SDoc
pprHsForAllTvs qtvs
  | show_forall = forAllLit <+> interppSP qtvs <> dot
  | otherwise   = empty
  where
    show_forall = not (null qtvs)

pprHsContext :: (OutputableBndr name) => HsContext name -> SDoc
pprHsContext = maybe empty (<+> darrow) . pprHsContextMaybe

pprHsContextMaybe :: (OutputableBndr name) => HsContext name -> Maybe SDoc
pprHsContextMaybe []         = Nothing
pprHsContextMaybe [L _ pred] = Just $ ppr_mono_ty FunPrec pred
pprHsContextMaybe cxt        = Just $ parens (interpp'SP cxt)

-- True <=> print an extra-constraints wildcard, e.g. @(Show a, _) =>@
pprHsContextExtra :: (OutputableBndr name) => Bool -> HsContext name -> SDoc
pprHsContextExtra show_extra ctxt
  | not show_extra
  = pprHsContext ctxt
  | null ctxt
  = char '_' <+> darrow
  | otherwise
  = parens (sep (punctuate comma ctxt')) <+> darrow
  where
    ctxt' = map ppr ctxt ++ [char '_']

pprConDeclFields :: (OutputableBndr name) => [LConDeclField name] -> SDoc
pprConDeclFields fields = braces (sep (punctuate comma (map ppr_fld fields)))
  where
    ppr_fld (L _ (ConDeclField { cd_fld_names = ns, cd_fld_type = ty,
                                 cd_fld_doc = doc }))
        = ppr_names ns <+> dcolon <+> ppr ty <+> ppr_mbDoc doc
    ppr_names [n] = ppr n
    ppr_names ns = sep (punctuate comma (map ppr ns))

{-
Note [Printing KindedTyVars]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Trac #3830 reminded me that we should really only print the kind
signature on a KindedTyVar if the kind signature was put there by the
programmer.  During kind inference GHC now adds a PostTcKind to UserTyVars,
rather than converting to KindedTyVars as before.

(As it happens, the message in #3830 comes out a different way now,
and the problem doesn't show up; but having the flag on a KindedTyVar
seems like the Right Thing anyway.)
-}

-- Printing works more-or-less as for Types

pprHsType, pprParendHsType :: (OutputableBndr name) => HsType name -> SDoc

pprHsType ty       = ppr_mono_ty TopPrec (prepare ty)
pprParendHsType ty = ppr_mono_ty TyConPrec ty

-- Before printing a type, remove outermost HsParTy parens
prepare :: HsType name -> HsType name
prepare (HsParTy ty)                            = prepare (unLoc ty)
prepare (HsAppsTy [L _ (HsAppPrefix (L _ ty))]) = prepare ty
prepare ty                                      = ty

ppr_mono_lty :: (OutputableBndr name) => TyPrec -> LHsType name -> SDoc
ppr_mono_lty ctxt_prec ty = ppr_mono_ty ctxt_prec (unLoc ty)

ppr_mono_ty :: (OutputableBndr name) => TyPrec -> HsType name -> SDoc
ppr_mono_ty ctxt_prec (HsForAllTy { hst_bndrs = tvs, hst_body = ty })
  = maybeParen ctxt_prec FunPrec $
    sep [pprHsForAllTvs tvs, ppr_mono_lty TopPrec ty]

ppr_mono_ty ctxt_prec (HsQualTy { hst_ctxt = L _ ctxt, hst_body = ty })
  = maybeParen ctxt_prec FunPrec $
    sep [pprHsContext ctxt, ppr_mono_lty TopPrec ty]

ppr_mono_ty _    (HsBangTy b ty)     = ppr b <> ppr_mono_lty TyConPrec ty
ppr_mono_ty _    (HsRecTy flds)      = pprConDeclFields flds
ppr_mono_ty _    (HsTyVar (L _ name))= pprPrefixOcc name
ppr_mono_ty prec (HsFunTy ty1 ty2)   = ppr_fun_ty prec ty1 ty2
ppr_mono_ty _    (HsTupleTy con tys) = tupleParens std_con (pprWithCommas ppr tys)
  where std_con = case con of
                    HsUnboxedTuple -> UnboxedTuple
                    _              -> BoxedTuple
ppr_mono_ty _    (HsKindSig ty kind) = parens (ppr_mono_lty TopPrec ty <+> dcolon <+> ppr kind)
ppr_mono_ty _    (HsListTy ty)       = brackets (ppr_mono_lty TopPrec ty)
ppr_mono_ty _    (HsPArrTy ty)       = paBrackets (ppr_mono_lty TopPrec ty)
ppr_mono_ty prec (HsIParamTy n ty)   = maybeParen prec FunPrec (ppr n <+> dcolon <+> ppr_mono_lty TopPrec ty)
ppr_mono_ty _    (HsSpliceTy s)      = pprSplice s
ppr_mono_ty _    (HsExplicitListTy tys) = quote $ brackets (interpp'SP tys)
ppr_mono_ty _    (HsExplicitTupleTy tys) = quote $ parens (interpp'SP tys)
ppr_mono_ty _    (HsTyLit t)         = ppr_tylit t
ppr_mono_ty _    (HsWildCardTy (AnonWildCard))     = char '_'

ppr_mono_ty ctxt_prec (HsEqTy ty1 ty2)
  = maybeParen ctxt_prec TyOpPrec $
    ppr_mono_lty TyOpPrec ty1 <+> char '~' <+> ppr_mono_lty TyOpPrec ty2

ppr_mono_ty ctxt_prec (HsAppsTy tys)
  = maybeParen ctxt_prec TyConPrec $
    hsep (map (ppr_app_ty TopPrec . unLoc) tys)

ppr_mono_ty ctxt_prec (HsAppTy fun_ty arg_ty)
  = maybeParen ctxt_prec TyConPrec $
    hsep [ppr_mono_lty FunPrec fun_ty, ppr_mono_lty TyConPrec arg_ty]

ppr_mono_ty ctxt_prec (HsOpTy ty1 (L _ op) ty2)
  = maybeParen ctxt_prec TyOpPrec $
    sep [ ppr_mono_lty TyOpPrec ty1
        , sep [pprInfixOcc op, ppr_mono_lty TyOpPrec ty2 ] ]

ppr_mono_ty _         (HsParTy ty)
  = parens (ppr_mono_lty TopPrec ty)
  -- Put the parens in where the user did
  -- But we still use the precedence stuff to add parens because
  --    toHsType doesn't put in any HsParTys, so we may still need them

ppr_mono_ty ctxt_prec (HsDocTy ty doc)
  = maybeParen ctxt_prec TyOpPrec $
    ppr_mono_lty TyOpPrec ty <+> ppr (unLoc doc)
  -- we pretty print Haddock comments on types as if they were
  -- postfix operators

--------------------------
ppr_fun_ty :: (OutputableBndr name)
           => TyPrec -> LHsType name -> LHsType name -> SDoc
ppr_fun_ty ctxt_prec ty1 ty2
  = let p1 = ppr_mono_lty FunPrec ty1
        p2 = ppr_mono_lty TopPrec ty2
    in
    maybeParen ctxt_prec FunPrec $
    sep [p1, text "->" <+> p2]

--------------------------
ppr_app_ty :: (OutputableBndr name) => TyPrec -> HsAppType name -> SDoc
ppr_app_ty _    (HsAppInfix (L _ n))                  = pprInfixOcc n
ppr_app_ty _    (HsAppPrefix (L _ (HsTyVar (L _ n)))) = pprPrefixOcc n
ppr_app_ty ctxt (HsAppPrefix ty)                      = ppr_mono_lty ctxt ty

--------------------------
ppr_tylit :: HsTyLit -> SDoc
ppr_tylit (HsNumTy _ i) = integer i
ppr_tylit (HsStrTy _ s) = text (show s)

instance Outputable HsSrcBang where
    ppr (HsSrcBang _ prag mark) = ppr prag <+> ppr mark

instance Outputable HsImplBang where
    ppr HsLazy                  = text "Lazy"
    ppr HsUnpack                = text "Unpacked"
    ppr HsStrict                = text "StrictNotUnpacked"

instance Outputable SrcStrictness where
    ppr SrcLazy     = char '~'
    ppr SrcStrict   = char '!'
    ppr NoSrcStrict = empty

instance Outputable SrcUnpackedness where
    ppr SrcUnpack   = text "{-# UNPACK #-}"
    ppr SrcNoUnpack = text "{-# NOUNPACK #-}"
    ppr NoSrcUnpack = empty

maybeParen :: TyPrec -> TyPrec -> SDoc -> SDoc
maybeParen ctxt_prec inner_prec pretty
  | ctxt_prec < inner_prec = pretty
  | otherwise              = parens pretty

instance Outputable HsIPName where
    ppr (HsIPName n) = char '?' <> ftext n -- Ordinary implicit parameters

instance OutputableBndr HsIPName where
    pprBndr _ n   = ppr n         -- Simple for now
    pprInfixOcc  n = ppr n
    pprPrefixOcc n = ppr n

instance (OutputableBndr name) => Outputable (ConDeclField name) where
  ppr (ConDeclField fld_n fld_ty _) = ppr fld_n <+> dcolon <+> ppr fld_ty

instance (Outputable arg, Outputable rec)
         => Outputable (HsConDetails arg rec) where
  ppr (PrefixCon args) = text "PrefixCon" <+> ppr args
  ppr (RecCon rec)     = text "RecCon:" <+> ppr rec
  ppr (InfixCon l r)   = text "InfixCon:" <+> ppr [l, r]

instance Outputable name => Outputable (FieldOcc name) where
  ppr = ppr . rdrNameFieldOcc

instance Outputable name => Outputable (AmbiguousFieldOcc name) where
  ppr = ppr . rdrNameAmbiguousFieldOcc

instance OutputableBndr name => OutputableBndr (AmbiguousFieldOcc name) where
  pprInfixOcc  = pprInfixOcc . rdrNameAmbiguousFieldOcc
  pprPrefixOcc = pprPrefixOcc . rdrNameAmbiguousFieldOcc

rdrNameAmbiguousFieldOcc :: AmbiguousFieldOcc name -> name
rdrNameAmbiguousFieldOcc (Unambiguous (L _ rdr)) = rdr
rdrNameAmbiguousFieldOcc (Ambiguous   (L _ rdr)) = rdr


-- HsBinds
-------------------------------------------------------------------------------

instance (OutputableBndr idL, OutputableBndr idR)
        => Outputable (HsLocalBindsLR idL idR) where
  ppr (HsValBinds bs) = ppr bs
  ppr (HsIPBinds bs)  = ppr bs
  ppr EmptyLocalBinds = empty

instance (OutputableBndr idL, OutputableBndr idR)
        => Outputable (HsValBindsLR idL idR) where
  ppr (ValBindsIn binds sigs)
   = pprDeclList (pprLHsBindsForUser binds sigs)


pprLHsBindsForUser :: (OutputableBndr idL, OutputableBndr idR,
                       OutputableBndr id2)
                   => LHsBindsLR idL idR -> [LSig id2] -> [SDoc]
--  pprLHsBindsForUser is different to pprLHsBinds because
--  a) No braces: 'let' and 'where' include a list of HsBindGroups
--     and we don't want several groups of bindings each
--     with braces around
--  b) Sort by location before printing
--  c) Include signatures
pprLHsBindsForUser binds sigs
  = map snd (sort_by_loc decls)
  where

    decls :: [(SrcSpan, SDoc)]
    decls = [(loc, ppr sig)  | L loc sig <- sigs] ++
            [(loc, ppr bind) | L loc bind <- bagToList binds]

    sort_by_loc decls = sortBy (comparing fst) decls

pprDeclList :: [SDoc] -> SDoc   -- Braces with a space
-- Print a bunch of declarations
-- One could choose  { d1; d2; ... }, using 'sep'
-- or      d1
--         d2
--         ..
--    using vcat
-- At the moment we chose the latter
-- Also we do the 'pprDeeperList' thing.
pprDeclList ds = pprDeeperList vcat ds

instance (OutputableBndr idL, OutputableBndr idR)
         => Outputable (HsBindLR idL idR) where
    ppr mbind = ppr_monobind mbind

ppr_monobind :: (OutputableBndr idL, OutputableBndr idR)
             => HsBindLR idL idR -> SDoc

ppr_monobind (PatBind { pat_lhs = pat, pat_rhs = grhss })
  = pprPatBind pat grhss
ppr_monobind (FunBind { fun_id = fun,
                        fun_matches = matches
                        })
  = pprFunBind  matches
ppr_monobind (PatSynBind psb) = ppr psb


instance (OutputableBndr idL, OutputableBndr idR)
          => Outputable (PatSynBind idL idR) where
  ppr (PSB{ psb_id = (L _ psyn), psb_args = details, psb_def = pat,
            psb_dir = dir })
      = ppr_lhs <+> ppr_rhs
    where
      ppr_lhs = text "pattern" <+> ppr_details
      ppr_simple syntax = syntax <+> ppr pat

      ppr_details = case details of
          InfixPatSyn v1 v2 -> hsep [ppr v1, pprInfixOcc psyn, ppr v2]
          PrefixPatSyn vs   -> hsep (pprPrefixOcc psyn : map ppr vs)
          RecordPatSyn vs   ->
            pprPrefixOcc psyn
                      <> braces (sep (punctuate comma (map ppr vs)))

      ppr_rhs = case dir of
          Unidirectional           -> ppr_simple (text "<-")
          ImplicitBidirectional    -> ppr_simple equals
          ExplicitBidirectional mg -> ppr_simple (text "<-") <+> ptext (sLit "where") $$
                                      (nest 2 $ pprFunBind mg)

pprTicks :: SDoc -> SDoc -> SDoc
-- Print stuff about ticks only when -dppr-debug is on, to avoid
-- them appearing in error messages (from the desugarer); see Trac # 3263
-- Also print ticks in dumpStyle, so that -ddump-hpc actually does
-- something useful.
pprTicks pp_no_debug pp_when_debug
  = getPprStyle (\ sty -> if debugStyle sty || dumpStyle sty
                             then pp_when_debug
                             else pp_no_debug)

instance (OutputableBndr id) => Outputable (HsIPBinds id) where
  ppr (IPBinds bs) = pprDeeperList vcat (map ppr bs)


instance (OutputableBndr id) => Outputable (IPBind id) where
  ppr (IPBind lr rhs) = name <+> equals <+> pprExpr (unLoc rhs)
    where name = case lr of
                   Left (L _ ip) -> pprBndr LetBind ip
                   Right     id  -> pprBndr LetBind id


instance (OutputableBndr name) => Outputable (Sig name) where
    ppr sig = ppr_sig sig

ppr_sig :: (OutputableBndr name) => Sig name -> SDoc
ppr_sig (TypeSig vars ty)    = pprVarSig (map unLoc vars) (ppr ty)
ppr_sig (ClassOpSig is_deflt vars ty)
  | is_deflt                 = text "default" <+> pprVarSig (map unLoc vars) (ppr ty)
  | otherwise                = pprVarSig (map unLoc vars) (ppr ty)
ppr_sig (FixSig fix_sig)     = ppr fix_sig
ppr_sig (SpecSig var ty inl)
  = pragBrackets (pprSpec (unLoc var) (interpp'SP ty) inl)
ppr_sig (InlineSig var inl)       = pragBrackets (ppr inl <+> pprPrefixOcc (unLoc var))
ppr_sig (SpecInstSig _ ty)
  = pragBrackets (text "SPECIALIZE instance" <+> ppr ty)
ppr_sig (MinimalSig _ bf)         = pragBrackets (pprMinimalSig bf)
ppr_sig (PatSynSig name sig_ty)
  = text "pattern" <+> pprPrefixOcc (unLoc name) <+> dcolon
                           <+> ppr sig_ty

instance OutputableBndr name => Outputable (FixitySig name) where
  ppr (FixitySig names fixity) = sep [ppr fixity, pprops]
    where
      pprops = hsep $ punctuate comma (map (pprInfixOcc . unLoc) names)

pragBrackets :: SDoc -> SDoc
pragBrackets doc = text "{-#" <+> doc <+> ptext (sLit "#-}")

pprVarSig :: (OutputableBndr id) => [id] -> SDoc -> SDoc
pprVarSig vars pp_ty = sep [pprvars <+> dcolon, nest 2 pp_ty]
  where
    pprvars = hsep $ punctuate comma (map pprPrefixOcc vars)

pprSpec :: (OutputableBndr id) => id -> SDoc -> InlinePragma -> SDoc
pprSpec var pp_ty inl = text "SPECIALIZE" <+> pp_inl <+> pprVarSig [var] pp_ty
  where
    pp_inl | isDefaultInlinePragma inl = empty
           | otherwise = ppr inl

-- A DFun has an always-active inline activation so that
-- exprIsConApp_maybe can "see" its unfolding
-- (However, its actual Unfolding is a DFunUnfolding, which is
--  never inlined other than via exprIsConApp_maybe.)
isDefaultInlinePragma :: InlinePragma -> Bool
isDefaultInlinePragma (InlinePragma { inl_act = activation
                                    , inl_rule = match_info
                                    , inl_inline = inline })
  = isEmptyInlineSpec inline && isAlwaysActive activation && isFunLike match_info

isAlwaysActive :: Activation -> Bool
isAlwaysActive AlwaysActive = True
isAlwaysActive _            = False

isEmptyInlineSpec :: InlineSpec -> Bool
isEmptyInlineSpec EmptyInlineSpec = True
isEmptyInlineSpec _               = False

pprMinimalSig :: OutputableBndr name => LBooleanFormula (Located name) -> SDoc
pprMinimalSig (L _ bf) = text "MINIMAL" <+> ppr (fmap unLoc bf)

instance Outputable a => Outputable (RecordPatSynField a) where
    ppr (RecordPatSynField { recordPatSynSelectorId = v }) = ppr v


-- HsPat
-------------------------------------------------------------------------------

instance (OutputableBndr name) => Outputable (Pat name) where
    ppr = pprPat

pprPatBndr :: OutputableBndr name => name -> SDoc
pprPatBndr var                  -- Print with type info if -dppr-debug is on
  = getPprStyle $ \ sty ->
    if debugStyle sty then
        parens (pprBndr LambdaBind var)         -- Could pass the site to pprPat
                                                -- but is it worth it?
    else
        pprPrefixOcc var

pprParendLPat :: (OutputableBndr name) => LPat name -> SDoc
pprParendLPat (L _ p) = pprParendPat p

pprParendPat :: (OutputableBndr name) => Pat name -> SDoc
pprParendPat p = sdocWithDynFlags $ \ dflags ->
                 if need_parens dflags p
                 then parens (pprPat p)
                 else  pprPat p
  where
    need_parens dflags p
      | otherwise     = hsPatNeedsParens p
      -- For a CoPat we need parens if we are going to show it, which
      -- we do if -fprint-typechecker-elaboration is on (c.f. pprHsWrapper)
      -- But otherwise the CoPat is discarded, so it
      -- is the pattern inside that matters.  Sigh.

hsPatNeedsParens :: Pat a -> Bool
hsPatNeedsParens (NPlusKPat {})      = True
hsPatNeedsParens (SplicePat {})      = False
hsPatNeedsParens (ConPatIn _ ds)     = conPatNeedsParens ds
hsPatNeedsParens (SigPatIn {})       = True
hsPatNeedsParens (ViewPat {})        = True
hsPatNeedsParens (WildPat {})        = False
hsPatNeedsParens (VarPat {})         = False
hsPatNeedsParens (LazyPat {})        = False
hsPatNeedsParens (BangPat {})        = False
hsPatNeedsParens (ParPat {})         = False
hsPatNeedsParens (AsPat {})          = False
hsPatNeedsParens (TuplePat {})       = False
hsPatNeedsParens (ListPat {})        = False
hsPatNeedsParens (PArrPat {})        = False
hsPatNeedsParens (LitPat {})         = False

conPatNeedsParens :: HsConDetails a b -> Bool
conPatNeedsParens (PrefixCon args) = not (null args)
conPatNeedsParens (InfixCon {})    = True
conPatNeedsParens (RecCon {})      = True

pprPat :: (OutputableBndr name) => Pat name -> SDoc
pprPat (VarPat (L _ var))     = pprPatBndr var
pprPat (WildPat)              = char '_'
pprPat (LazyPat pat)          = char '~' <> pprParendLPat pat
pprPat (BangPat pat)          = char '!' <> pprParendLPat pat
pprPat (AsPat name pat)       = hcat [pprPrefixOcc (unLoc name), char '@', pprParendLPat pat]
pprPat (ViewPat expr pat)     = hcat [pprLExpr expr, text " -> ", ppr pat]
pprPat (ParPat pat)           = parens (ppr pat)
pprPat (LitPat s)             = ppr s
pprPat (NPlusKPat n k)        = hcat [ppr n, char '+', ppr k]
pprPat (SplicePat splice)     = pprSplice splice
pprPat (SigPatIn pat ty)      = ppr pat <+> dcolon <+> ppr ty
pprPat (ListPat pats)         = brackets (interpp'SP pats)
pprPat (PArrPat pats)         = paBrackets (interpp'SP pats)
pprPat (TuplePat pats bx)     = tupleParens (boxityTupleSort bx) (pprWithCommas ppr pats)
pprPat (ConPatIn con details) = pprUserCon (unLoc con) details

boxityTupleSort :: Boxity -> TupleSort
boxityTupleSort Boxed   = BoxedTuple
boxityTupleSort Unboxed = UnboxedTuple

pprUserCon :: (OutputableBndr con, OutputableBndr id)
           => con -> HsConPatDetails id -> SDoc
pprUserCon c (InfixCon p1 p2) = ppr p1 <+> pprInfixOcc c <+> ppr p2
pprUserCon c details          = pprPrefixOcc c <+> pprConArgs details

pprConArgs :: (OutputableBndr id) => HsConPatDetails id -> SDoc
pprConArgs (PrefixCon pats) = sep (map pprParendLPat pats)
pprConArgs (InfixCon p1 p2) = sep [pprParendLPat p1, pprParendLPat p2]
pprConArgs (RecCon rpats)   = ppr rpats

instance (Outputable id, Outputable arg)
      => Outputable (HsRecFields id arg) where
  ppr (HsRecFields { rec_flds = flds, rec_dotdot = Nothing })
        = braces (fsep (punctuate comma (map ppr flds)))
  ppr (HsRecFields { rec_flds = flds, rec_dotdot = Just n })
        = braces (fsep (punctuate comma (map ppr (take n flds) ++ [dotdot])))
        where
          dotdot = text ".." <+> ifPprDebug (ppr (drop n flds))

instance (Outputable id, Outputable arg)
      => Outputable (HsRecField' id arg) where
  ppr (HsRecField { hsRecFieldLbl = f, hsRecFieldArg = Just arg})
    = ppr f <+> (equals <+> ppr arg)
  ppr (HsRecField { hsRecFieldLbl = f, hsRecFieldArg = Nothing})
    = ppr f <+> empty


-- HsDecls
-------------------------------------------------------------------------------

instance Outputable Role where
  ppr = ftext . fsFromRole

fsFromRole :: Role -> FastString
fsFromRole Nominal          = fsLit "nominal"
fsFromRole Representational = fsLit "representational"
fsFromRole Phantom          = fsLit "phantom"

pprFundeps :: Outputable a => [FunDep a] -> SDoc
pprFundeps []  = empty
pprFundeps fds = hsep (vbar : punctuate comma (map pprFunDep fds))

pprFunDep :: Outputable a => FunDep a -> SDoc
pprFunDep (us, vs) = hsep [interppSP us, arrow, interppSP vs]

instance (OutputableBndr name) => Outputable (HsDecl name) where
    ppr (TyClD dcl)             = ppr dcl
    ppr (ValD binds)            = ppr binds
    ppr (DefD def)              = ppr def
    ppr (InstD inst)            = ppr inst
    ppr (DerivD deriv)          = ppr deriv
    ppr (ForD fd)               = ppr fd
    ppr (SigD sd)               = ppr sd
    ppr (RuleD rd)              = ppr rd
    ppr (VectD vect)            = ppr vect
    ppr (WarningD wd)           = ppr wd
    ppr (AnnD ad)               = ppr ad
    ppr (SpliceD dd)            = ppr dd
    ppr (DocD doc)              = ppr doc
    ppr (RoleAnnotD ra)         = ppr ra

instance (OutputableBndr name) => Outputable (HsGroup name) where
    ppr (HsGroup { hs_valds  = val_decls,
                   hs_tyclds = tycl_decls,
                   hs_derivds = deriv_decls,
                   hs_fixds  = fix_decls,
                   hs_warnds = deprec_decls,
                   hs_annds  = ann_decls,
                   hs_fords  = foreign_decls,
                   hs_defds  = default_decls,
                   hs_ruleds = rule_decls,
                   hs_vects  = vect_decls })
        = vcat_mb empty
            [ppr_ds fix_decls, ppr_ds default_decls,
             ppr_ds deprec_decls, ppr_ds ann_decls,
             ppr_ds rule_decls,
             ppr_ds vect_decls,
             if isEmptyValBinds val_decls
                then Nothing
                else Just (ppr val_decls),
             ppr_ds (tyClGroupTyClDecls tycl_decls),
             ppr_ds (tyClGroupInstDecls tycl_decls),
             ppr_ds deriv_decls,
             ppr_ds foreign_decls]
        where
          ppr_ds :: Outputable a => [a] -> Maybe SDoc
          ppr_ds [] = Nothing
          ppr_ds ds = Just (vcat (map ppr ds))

          vcat_mb :: SDoc -> [Maybe SDoc] -> SDoc
          -- Concatenate vertically with white-space between non-blanks
          vcat_mb _    []             = empty
          vcat_mb gap (Nothing : ds) = vcat_mb gap ds
          vcat_mb gap (Just d  : ds) = gap $$ d $$ vcat_mb blankLine ds

tyClGroupTyClDecls :: [TyClGroup name] -> [LTyClDecl name]
tyClGroupTyClDecls = concatMap group_tyclds

tyClGroupInstDecls :: [TyClGroup name] -> [LInstDecl name]
tyClGroupInstDecls = concatMap group_instds


instance (OutputableBndr name) => Outputable (SpliceDecl name) where
   ppr (SpliceDecl (L _ e) _) = pprSplice e

instance (OutputableBndr name) => Outputable (TyClDecl name) where

    ppr (FamDecl { tcdFam = decl }) = ppr decl
    ppr (SynDecl { tcdLName = ltycon, tcdTyVars = tyvars, tcdRhs = rhs })
      = hang (text "type" <+>
              pp_vanilla_decl_head ltycon tyvars [] <+> equals)
          4 (ppr rhs)

    ppr (DataDecl { tcdLName = ltycon, tcdTyVars = tyvars, tcdDataDefn = defn })
      = pp_data_defn (pp_vanilla_decl_head ltycon tyvars) defn

    ppr (ClassDecl {tcdCtxt = context, tcdLName = lclas, tcdTyVars = tyvars,
                    tcdFDs  = fds,
                    tcdSigs = sigs, tcdMeths = methods,
                    tcdATs = ats, tcdATDefs = at_defs})
      | null sigs && isEmptyBag methods && null ats && null at_defs -- No "where" part
      = top_matter

      | otherwise       -- Laid out
      = vcat [ top_matter <+> text "where"
             , nest 2 $ pprDeclList (map (pprFamilyDecl NotTopLevel . unLoc) ats ++
                                     map ppr_fam_deflt_eqn at_defs ++
                                     pprLHsBindsForUser methods sigs) ]
      where
        top_matter = text "class"
                     <+> pp_vanilla_decl_head lclas tyvars (unLoc context)
                     <+> pprFundeps (map unLoc fds)

instance (OutputableBndr name) => Outputable (TyClGroup name) where
  ppr (TyClGroup { group_tyclds = tyclds
                 , group_roles = roles
                 , group_instds = instds
                 }
      )
    = ppr tyclds $$
      ppr roles $$
      ppr instds

pp_vanilla_decl_head :: (OutputableBndr name)
   => Located name
   -> LHsQTyVars name
   -> HsContext name
   -> SDoc
pp_vanilla_decl_head thing tyvars context
 = hsep [pprHsContext context, pprPrefixOcc (unLoc thing), ppr tyvars]

instance (OutputableBndr name) => Outputable (FamilyDecl name) where
  ppr = pprFamilyDecl TopLevel

pprFamilyDecl :: (OutputableBndr name)
              => TopLevelFlag -> FamilyDecl name -> SDoc
pprFamilyDecl top_level (FamilyDecl { fdInfo = info, fdLName = ltycon
                                    , fdTyVars = tyvars
                                    , fdResultSig = L _ result
                                    , fdInjectivityAnn = mb_inj })
  = vcat [ pprFlavour info <+> pp_top_level <+>
           pp_vanilla_decl_head ltycon tyvars [] <+>
           pp_kind <+> pp_inj <+> pp_where
         , nest 2 $ pp_eqns ]
  where
    pp_top_level = case top_level of
                     TopLevel    -> text "family"
                     NotTopLevel -> empty

    pp_kind = case result of
                NoSig            -> empty
                KindSig  kind    -> dcolon <+> ppr kind
                TyVarSig tv_bndr -> text "=" <+> ppr tv_bndr
    pp_inj = case mb_inj of
               Just (L _ (InjectivityAnn lhs rhs)) ->
                 hsep [ vbar, ppr lhs, text "->", hsep (map ppr rhs) ]
               Nothing -> empty
    (pp_where, pp_eqns) = case info of
      ClosedTypeFamily mb_eqns ->
        ( text "where"
        , case mb_eqns of
            Nothing   -> text ".."
            Just eqns -> vcat $ map ppr_fam_inst_eqn eqns )
      _ -> (empty, empty)

pprFlavour :: FamilyInfo name -> SDoc
pprFlavour DataFamily            = text "data"
pprFlavour OpenTypeFamily        = text "type"
pprFlavour (ClosedTypeFamily {}) = text "type"

instance Outputable (FamilyInfo name) where
  ppr info = pprFlavour info <+> text "family"

pp_data_defn :: (OutputableBndr name)
                  => (HsContext name -> SDoc)   -- Printing the header
                  -> HsDataDefn name
                  -> SDoc
pp_data_defn pp_hdr (HsDataDefn { dd_ND = new_or_data, dd_ctxt = L _ context
                                , dd_kindSig = mb_sig
                                , dd_cons = condecls, dd_derivs = derivings })
  | null condecls
  = ppr new_or_data <+> pp_hdr context <+> pp_sig

  | otherwise
  = hang (ppr new_or_data <+> pp_hdr context <+> pp_sig)
       2 (pp_condecls condecls $$ pp_derivings)
  where
    pp_sig = case mb_sig of
               Nothing   -> empty
               Just kind -> dcolon <+> ppr kind
    pp_derivings = case derivings of
                     Nothing -> empty
                     Just (L _ ds) -> hsep [ text "deriving"
                                           , parens (interpp'SP ds)]

instance (OutputableBndr name) => Outputable (HsDataDefn name) where
   ppr d = pp_data_defn (\_ -> text "Naked HsDataDefn") d

instance Outputable NewOrData where
  ppr NewType  = text "newtype"
  ppr DataType = text "data"

pp_condecls :: (OutputableBndr name) => [LConDecl name] -> SDoc
pp_condecls cs@(L _ ConDeclGADT{} : _) -- In GADT syntax
  = hang (text "where") 2 (vcat (map ppr cs))
pp_condecls cs                    -- In H98 syntax
  = equals <+> sep (punctuate (text " |") (map ppr cs))

instance (OutputableBndr name) => Outputable (ConDecl name) where
    ppr = pprConDecl

pprConDecl :: (OutputableBndr name) => ConDecl name -> SDoc
pprConDecl (ConDeclH98 { con_name = L _ con
                       , con_qvars = mtvs
                       , con_cxt = mcxt
                       , con_details = details
                       , con_doc = doc })
  = sep [ppr_mbDoc doc, pprHsForAll tvs cxt,         ppr_details details]
  where
    ppr_details (InfixCon t1 t2) = hsep [ppr t1, pprInfixOcc con, ppr t2]
    ppr_details (PrefixCon tys)  = hsep (pprPrefixOcc con
                                   : map (pprParendHsType . unLoc) tys)
    ppr_details (RecCon fields)  = pprPrefixOcc con
                                 <+> pprConDeclFields (unLoc fields)
    tvs = case mtvs of
      Nothing -> []
      Just (HsQTvs { hsq_explicit = tvs }) -> tvs

    cxt = fromMaybe (noLoc []) mcxt

pprConDecl (ConDeclGADT { con_names = cons, con_type = res_ty, con_doc = doc })
  = sep [ppr_mbDoc doc <+> ppr_con_names cons <+> dcolon
         <+> ppr res_ty]

ppr_con_names :: (OutputableBndr name) => [Located name] -> SDoc
ppr_con_names = pprWithCommas (pprPrefixOcc . unLoc)

instance (OutputableBndr name) => Outputable (TyFamInstDecl name) where
  ppr = pprTyFamInstDecl TopLevel

pprTyFamInstDecl :: (OutputableBndr name)
                 => TopLevelFlag -> TyFamInstDecl name -> SDoc
pprTyFamInstDecl top_lvl (TyFamInstDecl { tfid_eqn = eqn })
   = text "type" <+> ppr_instance_keyword top_lvl <+> ppr_fam_inst_eqn eqn

ppr_instance_keyword :: TopLevelFlag -> SDoc
ppr_instance_keyword TopLevel    = text "instance"
ppr_instance_keyword NotTopLevel = empty

ppr_fam_inst_eqn :: (OutputableBndr name) => LTyFamInstEqn name -> SDoc
ppr_fam_inst_eqn (L _ (TyFamEqn { tfe_tycon = tycon
                                , tfe_pats  = pats
                                , tfe_rhs   = rhs }))
    = pp_fam_inst_lhs tycon pats [] <+> equals <+> ppr rhs

ppr_fam_deflt_eqn :: (OutputableBndr name) => LTyFamDefltEqn name -> SDoc
ppr_fam_deflt_eqn (L _ (TyFamEqn { tfe_tycon = tycon
                                 , tfe_pats  = tvs
                                 , tfe_rhs   = rhs }))
    = text "type" <+> pp_vanilla_decl_head tycon tvs [] <+> equals <+> ppr rhs

instance (OutputableBndr name) => Outputable (DataFamInstDecl name) where
  ppr = pprDataFamInstDecl TopLevel

pprDataFamInstDecl :: (OutputableBndr name)
                   => TopLevelFlag -> DataFamInstDecl name -> SDoc
pprDataFamInstDecl top_lvl (DataFamInstDecl { dfid_tycon = tycon
                                            , dfid_pats  = pats
                                            , dfid_defn  = defn })
  = pp_data_defn pp_hdr defn
  where
    pp_hdr ctxt = ppr_instance_keyword top_lvl <+> pp_fam_inst_lhs tycon pats ctxt


pp_fam_inst_lhs :: (OutputableBndr name)
   => Located name
   -> HsTyPats name
   -> HsContext name
   -> SDoc
pp_fam_inst_lhs thing (HsIB { hsib_body = typats }) context -- explicit type patterns
   = hsep [ pprHsContext context, pprPrefixOcc (unLoc thing)
          , hsep (map (pprParendHsType.unLoc) typats)]

instance (OutputableBndr name) => Outputable (ClsInstDecl name) where
    ppr (ClsInstDecl { cid_poly_ty = inst_ty, cid_binds = binds
                     , cid_sigs = sigs, cid_tyfam_insts = ats
                     , cid_overlap_mode = mbOverlap
                     , cid_datafam_insts = adts })
      | null sigs, null ats, null adts, isEmptyBag binds  -- No "where" part
      = top_matter

      | otherwise       -- Laid out
      = vcat [ top_matter <+> text "where"
             , nest 2 $ pprDeclList $
               map (pprTyFamInstDecl NotTopLevel . unLoc)   ats ++
               map (pprDataFamInstDecl NotTopLevel . unLoc) adts ++
               pprLHsBindsForUser binds sigs ]
      where
        top_matter = text "instance" <+> ppOverlapPragma mbOverlap
                                             <+> ppr inst_ty

ppOverlapPragma :: Maybe (Located OverlapMode) -> SDoc
ppOverlapPragma mb =
  case mb of
    Nothing           -> empty
    Just (L _ (NoOverlap _))    -> text "{-# NO_OVERLAP #-}"
    Just (L _ (Overlappable _)) -> text "{-# OVERLAPPABLE #-}"
    Just (L _ (Overlapping _))  -> text "{-# OVERLAPPING #-}"
    Just (L _ (Overlaps _))     -> text "{-# OVERLAPS #-}"
    Just (L _ (Incoherent _))   -> text "{-# INCOHERENT #-}"


instance (OutputableBndr name) => Outputable (InstDecl name) where
    ppr (ClsInstD     { cid_inst  = decl }) = ppr decl
    ppr (TyFamInstD   { tfid_inst = decl }) = ppr decl
    ppr (DataFamInstD { dfid_inst = decl }) = ppr decl

instance (OutputableBndr name) => Outputable (DerivDecl name) where
    ppr (DerivDecl ty o)
        = hsep [text "deriving instance", ppOverlapPragma o, ppr ty]

instance (OutputableBndr name) => Outputable (DefaultDecl name) where

    ppr (DefaultDecl tys)
      = text "default" <+> parens (interpp'SP tys)

instance (OutputableBndr name) => Outputable (ForeignDecl name) where
  ppr (ForeignImport { fd_name = n, fd_sig_ty = ty, fd_fi = fimport })
    = hang (text "foreign import" <+> ppr fimport <+> ppr n)
         2 (dcolon <+> ppr ty)
  ppr (ForeignExport { fd_name = n, fd_sig_ty = ty, fd_fe = fexport }) =
    hang (text "foreign export" <+> ppr fexport <+> ppr n)
       2 (dcolon <+> ppr ty)

instance Outputable ForeignImport where
  ppr (CImport  cconv safety mHeader spec _) =
    ppr cconv <+> ppr safety <+>
    char '"' <> pprCEntity spec <> char '"'
    where
      pp_hdr = case mHeader of
               Nothing -> empty
               Just (Language.Haskell.Syntax.ForeignCall.Header _ header) -> ftext header

      pprCEntity (CLabel lbl) =
        text "static" <+> pp_hdr <+> char '&' <> ppr lbl
      pprCEntity (CFunction (StaticTarget _ lbl _ isFun)) =
            text "static"
        <+> pp_hdr
        <+> (if isFun then empty else text "value")
        <+> ppr lbl
      pprCEntity (CFunction (DynamicTarget)) =
        text "dynamic"
      pprCEntity (CWrapper) = text "wrapper"

instance Outputable ForeignExport where
  ppr (CExport  (L _ (CExportStatic _ lbl cconv)) _) =
    ppr cconv <+> char '"' <> ppr lbl <> char '"'


pprFullRuleName :: Located (SourceText, RuleName) -> SDoc
pprFullRuleName (L _ (_, n)) = doubleQuotes $ ftext n

instance (OutputableBndr name) => Outputable (RuleDecls name) where
  ppr (HsRules _ rules) = ppr rules

instance (OutputableBndr name) => Outputable (RuleDecl name) where
  ppr (HsRule name act ns lhs rhs)
        = sep [text "{-# RULES" <+> pprFullRuleName name
                                <+> ppr act,
               nest 4 (pp_forall <+> pprExpr (unLoc lhs)),
               nest 4 (equals <+> pprExpr (unLoc rhs) <+> text "#-}") ]
        where
          pp_forall | null ns   = empty
                    | otherwise = forAllLit <+> fsep (map ppr ns) <> dot

instance (OutputableBndr name) => Outputable (RuleBndr name) where
   ppr (RuleBndr name) = ppr name
   ppr (RuleBndrSig name ty) = ppr name <> dcolon <> ppr ty

instance (OutputableBndr name) => Outputable (VectDecl name) where
  ppr (HsVect _ v rhs)
    = sep [text "{-# VECTORISE" <+> ppr v,
           nest 4 $
             pprExpr (unLoc rhs) <+> text "#-}" ]
  ppr (HsNoVect _ v)
    = sep [text "{-# NOVECTORISE" <+> ppr v <+> text "#-}" ]
  ppr (HsVectTypeIn _ False t Nothing)
    = sep [text "{-# VECTORISE type" <+> ppr t <+> text "#-}" ]
  ppr (HsVectTypeIn _ False t (Just t'))
    = sep [text "{-# VECTORISE type" <+> ppr t, text "=", ppr t', text "#-}" ]
  ppr (HsVectTypeIn _ True t Nothing)
    = sep [text "{-# VECTORISE SCALAR type" <+> ppr t <+> text "#-}" ]
  ppr (HsVectTypeIn _ True t (Just t'))
    = sep [text "{-# VECTORISE SCALAR type" <+> ppr t, text "=", ppr t', text "#-}" ]
  ppr (HsVectClassIn _ c)
    = sep [text "{-# VECTORISE class" <+> ppr c <+> text "#-}" ]
  ppr (HsVectInstIn ty)
    = sep [text "{-# VECTORISE SCALAR instance" <+> ppr ty <+> text "#-}" ]

-- Okay, I need to reconstruct the document comments, but for now:
instance Outputable DocDecl where
  ppr _ = text "<document comment>"

instance OutputableBndr name => Outputable (WarnDecls name) where
    ppr (Warnings _ decls) = ppr decls

instance OutputableBndr name => Outputable (WarnDecl name) where
    ppr (Warning thing txt)
      = hsep [text "{-# DEPRECATED", ppr thing, doubleQuotes (ppr txt), text "#-}"]


instance (OutputableBndr name) => Outputable (AnnDecl name) where
    ppr (HsAnnotation _ provenance expr)
      = hsep [text "{-#", pprAnnProvenance provenance, pprExpr (unLoc expr), text "#-}"]

pprAnnProvenance :: OutputableBndr name => AnnProvenance name -> SDoc
pprAnnProvenance ModuleAnnProvenance       = text "ANN module"
pprAnnProvenance (ValueAnnProvenance (L _ name))
  = text "ANN" <+> ppr name
pprAnnProvenance (TypeAnnProvenance (L _ name))
  = text "ANN type" <+> ppr name

instance OutputableBndr name => Outputable (RoleAnnotDecl name) where
  ppr (RoleAnnotDecl ltycon roles)
    = text "type role" <+> ppr ltycon <+>
      hsep (map (pp_role . unLoc) roles)
    where
      pp_role Nothing  = underscore
      pp_role (Just r) = ppr r

-- HsExpr
--------------------------------------------------------------------------------

instance (OutputableBndr id) => Outputable (HsExpr id) where
    ppr expr = pprExpr expr

-----------------------
-- pprExpr, pprLExpr, pprBinds call pprDeeper;
-- the underscore versions do not
pprLExpr :: (OutputableBndr id) => LHsExpr id -> SDoc
pprLExpr (L _ e) = pprExpr e

isAtomicHsExpr :: HsExpr id -> Bool
-- True of a single token
isAtomicHsExpr (HsVar {})        = True
isAtomicHsExpr (HsLit {})        = True
isAtomicHsExpr (HsOverLit {})    = True
isAtomicHsExpr (HsIPVar {})      = True
isAtomicHsExpr (HsOverLabel {})  = True
isAtomicHsExpr (HsPar e)         = isAtomicHsExpr (unLoc e)
isAtomicHsExpr (HsRecFld{})      = True
isAtomicHsExpr _                 = False

isQuietHsExpr :: HsExpr id -> Bool
-- Parentheses do display something, but it gives little info and
-- if we go deeper when we go inside them then we get ugly things
-- like (...)
isQuietHsExpr (HsPar _)          = True
-- applications don't display anything themselves
isQuietHsExpr (HsApp _ _)        = True
isQuietHsExpr (HsAppType _ _)    = True
isQuietHsExpr (OpApp _ _ _)      = True
isQuietHsExpr _ = False


pprExpr :: (OutputableBndr id) => HsExpr id -> SDoc
pprExpr e | isAtomicHsExpr e || isQuietHsExpr e =            ppr_expr e
          | otherwise                           = pprDeeper (ppr_expr e)


pprBinds :: (OutputableBndr idL, OutputableBndr idR)
         => HsLocalBindsLR idL idR -> SDoc
pprBinds b = pprDeeper (ppr b)

-----------------------
ppr_lexpr :: (OutputableBndr id) => LHsExpr id -> SDoc
ppr_lexpr e = ppr_expr (unLoc e)

ppr_expr :: (OutputableBndr id) => HsExpr id -> SDoc
ppr_expr (HsVar (L _ v))  = pprPrefixOcc v
ppr_expr (HsIPVar v)      = ppr v
ppr_expr (HsOverLabel l)  = char '#' <> ppr l
ppr_expr (HsLit lit)      = ppr lit
ppr_expr (HsOverLit lit)  = ppr lit
ppr_expr (HsPar e)        = parens (ppr_lexpr e)

ppr_expr (HsCoreAnn _ (StringLiteral _ s) e)
  = vcat [text "HsCoreAnn" <+> ftext s, ppr_lexpr e]

ppr_expr e@(HsApp {})        = ppr_apps e []
ppr_expr e@(HsAppType {})    = ppr_apps e []

ppr_expr (OpApp e1 op e2)
  = case unLoc op of
      HsVar (L _ v) -> pp_infixly v
      HsRecFld f    -> pp_infixly f
      _             -> pp_prefixly
  where
    pp_e1 = pprDebugParendExpr e1   -- In debug mode, add parens
    pp_e2 = pprDebugParendExpr e2   -- to make precedence clear

    pp_prefixly
      = hang (ppr op) 2 (sep [pp_e1, pp_e2])

    pp_infixly v
      = sep [pp_e1, sep [pprInfixOcc v, nest 2 pp_e2]]

ppr_expr (NegApp e) = char '-' <+> pprDebugParendExpr e

ppr_expr (SectionL expr op)
  = case unLoc op of
      HsVar (L _ v) -> pp_infixly v
      _             -> pp_prefixly
  where
    pp_expr = pprDebugParendExpr expr

    pp_prefixly = hang (hsep [text " \\ x_ ->", ppr op])
                       4 (hsep [pp_expr, text "x_ )"])
    pp_infixly v = (sep [pp_expr, pprInfixOcc v])

ppr_expr (SectionR op expr)
  = case unLoc op of
      HsVar (L _ v) -> pp_infixly v
      _             -> pp_prefixly
  where
    pp_expr = pprDebugParendExpr expr

    pp_prefixly = hang (hsep [text "( \\ x_ ->", ppr op, text "x_"])
                       4 (pp_expr <> rparen)
    pp_infixly v = sep [pprInfixOcc v, pp_expr]

ppr_expr (ExplicitTuple exprs boxity)
  = tupleParens (boxityTupleSort boxity) (fcat (ppr_tup_args $ map unLoc exprs))
  where
    ppr_tup_args []               = []
    ppr_tup_args (Present e : es) = (ppr_lexpr e <> punc es) : ppr_tup_args es
    ppr_tup_args (Missing   : es) = punc es : ppr_tup_args es

    punc (Present {} : _) = comma <> space
    punc (Missing {} : _) = comma
    punc []               = empty

ppr_expr (HsLam matches)
  = pprMatches matches

ppr_expr (HsLamCase matches)
  = sep [ sep [text "\\case {"],
          nest 2 (pprMatches matches <+> char '}') ]

ppr_expr (HsCase expr matches)
  = sep [ sep [text "case", nest 4 (ppr expr), ptext (sLit "of {")],
          nest 2 (pprMatches matches <+> char '}') ]

ppr_expr (HsIf e1 e2 e3)
  = sep [hsep [text "if", nest 2 (ppr e1), ptext (sLit "then")],
         nest 4 (ppr e2),
         text "else",
         nest 4 (ppr e3)]

ppr_expr (HsMultiIf alts)
  = sep $ text "if" : map ppr_alt alts
  where ppr_alt (L _ (GRHS guards expr)) =
          sep [ vbar <+> interpp'SP guards
              , text "->" <+> pprDeeper (ppr expr) ]

-- special case: let ... in let ...
ppr_expr (HsLet (L _ binds) expr@(L _ (HsLet _ _)))
  = sep [hang (text "let") 2 (hsep [pprBinds binds, ptext (sLit "in")]),
         ppr_lexpr expr]

ppr_expr (HsLet (L _ binds) expr)
  = sep [hang (text "let") 2 (pprBinds binds),
         hang (text "in")  2 (ppr expr)]

ppr_expr (HsDo do_or_list_comp (L _ stmts)) = pprDo do_or_list_comp stmts

ppr_expr (ExplicitList exprs)
  = brackets (pprDeeperList fsep (punctuate comma (map ppr_lexpr exprs)))

ppr_expr (ExplicitPArr exprs)
  = paBrackets (pprDeeperList fsep (punctuate comma (map ppr_lexpr exprs)))

ppr_expr (RecordCon { rcon_con_name = con_id, rcon_flds = rbinds })
  = hang (ppr con_id) 2 (ppr rbinds)

ppr_expr (RecordUpd { rupd_expr = L _ aexp, rupd_flds = rbinds })
  = hang (pprParendExpr aexp) 2 (braces (fsep (punctuate comma (map ppr rbinds))))

ppr_expr (ExprWithTySig expr sig)
  = hang (nest 2 (ppr_lexpr expr) <+> dcolon)
         4 (ppr sig)

ppr_expr (ArithSeq info) = brackets (ppr info)
ppr_expr (PArrSeq  info) = paBrackets (ppr info)

ppr_expr EWildPat       = char '_'
ppr_expr (ELazyPat e)   = char '~' <> pprParendLExpr e
ppr_expr (EAsPat v e)   = ppr v <> char '@' <> pprParendLExpr e
ppr_expr (EViewPat p e) = ppr p <+> text "->" <+> ppr e

ppr_expr (HsSCC _ (StringLiteral _ lbl) expr)
  = sep [ text "{-# SCC" <+> doubleQuotes (ftext lbl) <+> ptext (sLit "#-}"),
          pprParendLExpr expr ]

ppr_expr (HsSpliceE s)         = pprSplice s
ppr_expr (HsBracket b)         = pprHsBracket b

ppr_expr (HsProc pat (L _ (HsCmdTop cmd)))
  = hsep [text "proc", ppr pat, ptext (sLit "->"), ppr cmd]

ppr_expr (HsStatic e)
  = hsep [text "static", pprParendLExpr e]

ppr_expr (HsBinTick tickIdTrue tickIdFalse exp)
  = pprTicks (ppr exp) $
    hcat [text "bintick<",
          ppr tickIdTrue,
          text ",",
          ppr tickIdFalse,
          text ">(",
          ppr exp, text ")"]
ppr_expr (HsTickPragma _ externalSrcLoc _ exp)
  = pprTicks (ppr exp) $
    hcat [text "tickpragma<",
          pprExternalSrcLoc externalSrcLoc,
          text ">(",
          ppr exp,
          text ")"]

ppr_expr (HsArrApp arrow arg HsFirstOrderApp True)
  = hsep [ppr_lexpr arrow, larrowt, ppr_lexpr arg]
ppr_expr (HsArrApp arrow arg HsFirstOrderApp False)
  = hsep [ppr_lexpr arg, arrowt, ppr_lexpr arrow]
ppr_expr (HsArrApp arrow arg HsHigherOrderApp True)
  = hsep [ppr_lexpr arrow, larrowtt, ppr_lexpr arg]
ppr_expr (HsArrApp arrow arg HsHigherOrderApp False)
  = hsep [ppr_lexpr arg, arrowtt, ppr_lexpr arrow]

ppr_expr (HsArrForm (L _ (HsVar (L _ v))) (Just _) [arg1, arg2])
  = sep [pprCmdArg (unLoc arg1), hsep [pprInfixOcc v, pprCmdArg (unLoc arg2)]]
ppr_expr (HsArrForm op _ args)
  = hang (text "(|" <+> ppr_lexpr op)
         4 (sep (map (pprCmdArg.unLoc) args) <+> text "|)")
ppr_expr (HsRecFld f) = ppr f

ppr_apps :: (OutputableBndr id)
         => HsExpr id
         -> [Either (LHsExpr id) (LHsWcType id)]
         -> SDoc
ppr_apps (HsApp (L _ fun) arg)        args
  = ppr_apps fun (Left arg : args)
ppr_apps (HsAppType (L _ fun) arg)    args
  = ppr_apps fun (Right arg : args)
ppr_apps fun args = hang (ppr_expr fun) 2 (sep (map pp args))
  where
    pp (Left arg)                             = pprParendLExpr arg
    pp (Right (HsWC { hswc_body = L _ arg }))
      = char '@' <> pprParendHsType arg

pprExternalSrcLoc :: (StringLiteral,(Int,Int),(Int,Int)) -> SDoc
pprExternalSrcLoc (StringLiteral _ src,(n1,n2),(n3,n4))
  = ppr (src,(n1,n2),(n3,n4))

{-
HsSyn records exactly where the user put parens, with HsPar.
So generally speaking we print without adding any parens.
However, some code is internally generated, and in some places
parens are absolutely required; so for these places we use
pprParendLExpr (but don't print double parens of course).

For operator applications we don't add parens, because the operator
fixities should do the job, except in debug mode (-dppr-debug) so we
can see the structure of the parse tree.
-}

pprDebugParendExpr :: (OutputableBndr id) => LHsExpr id -> SDoc
pprDebugParendExpr expr
  = getPprStyle (\sty ->
    if debugStyle sty then pprParendLExpr expr
                      else pprLExpr      expr)

pprParendLExpr :: (OutputableBndr id) => LHsExpr id -> SDoc
pprParendLExpr (L _ e) = pprParendExpr e

hsExprNeedsParens :: HsExpr id -> Bool
-- True of expressions for which '(e)' and 'e'
-- mean the same thing
hsExprNeedsParens (ArithSeq {})       = False
hsExprNeedsParens (PArrSeq {})        = False
hsExprNeedsParens (HsLit {})          = False
hsExprNeedsParens (HsOverLit {})      = False
hsExprNeedsParens (HsVar {})          = False
hsExprNeedsParens (HsIPVar {})        = False
hsExprNeedsParens (HsOverLabel {})    = False
hsExprNeedsParens (ExplicitTuple {})  = False
hsExprNeedsParens (ExplicitList {})   = False
hsExprNeedsParens (ExplicitPArr {})   = False
hsExprNeedsParens (HsPar {})          = False
hsExprNeedsParens (HsBracket {})      = False
hsExprNeedsParens (HsDo sc _)
       | isListCompExpr sc            = False
hsExprNeedsParens (HsRecFld{})        = False
hsExprNeedsParens _ = True

isListCompExpr :: HsStmtContext id -> Bool
-- Uses syntax [ e | quals ]
isListCompExpr ListComp          = True
isListCompExpr PArrComp          = True
isListCompExpr MonadComp         = True
isListCompExpr (ParStmtCtxt c)   = isListCompExpr c
isListCompExpr (TransStmtCtxt c) = isListCompExpr c
isListCompExpr _                 = False

isQuietHsCmd :: HsCmd id -> Bool
-- Parentheses do display something, but it gives little info and
-- if we go deeper when we go inside them then we get ugly things
-- like (...)
isQuietHsCmd (HsCmdPar _) = True
-- applications don't display anything themselves
isQuietHsCmd (HsCmdApp _ _) = True
isQuietHsCmd _ = False

pprParendExpr :: (OutputableBndr id) => HsExpr id -> SDoc
pprParendExpr expr
  | hsExprNeedsParens expr = parens (pprExpr expr)
  | otherwise              = pprExpr expr
        -- Using pprLExpr makes sure that we go 'deeper'
        -- I think that is usually (always?) right

instance (OutputableBndr id) => Outputable (HsCmd id) where
    ppr cmd = pprCmd cmd

pprCmd :: (OutputableBndr id) => HsCmd id -> SDoc
pprCmd c | isQuietHsCmd c =            ppr_cmd c
         | otherwise      = pprDeeper (ppr_cmd c)

-----------------------
ppr_lcmd :: (OutputableBndr id) => LHsCmd id -> SDoc
ppr_lcmd c = ppr_cmd (unLoc c)

ppr_cmd :: (OutputableBndr id) => HsCmd id -> SDoc
ppr_cmd (HsCmdPar c) = parens (ppr_lcmd c)

ppr_cmd (HsCmdApp c e)
  = let (fun, args) = collect_args c [e] in
    hang (ppr_lcmd fun) 2 (sep (map pprParendLExpr args))
  where
    collect_args (L _ (HsCmdApp fun arg)) args = collect_args fun (arg:args)
    collect_args fun args = (fun, args)

ppr_cmd (HsCmdLam matches)
  = pprMatches matches

ppr_cmd (HsCmdCase expr matches)
  = sep [ sep [text "case", nest 4 (ppr expr), ptext (sLit "of {")],
          nest 2 (pprMatches matches <+> char '}') ]

ppr_cmd (HsCmdIf e ct ce)
  = sep [hsep [text "if", nest 2 (ppr e), ptext (sLit "then")],
         nest 4 (ppr ct),
         text "else",
         nest 4 (ppr ce)]

-- special case: let ... in let ...
ppr_cmd (HsCmdLet (L _ binds) cmd@(L _ (HsCmdLet _ _)))
  = sep [hang (text "let") 2 (hsep [pprBinds binds, ptext (sLit "in")]),
         ppr_lcmd cmd]

ppr_cmd (HsCmdLet (L _ binds) cmd)
  = sep [hang (text "let") 2 (pprBinds binds),
         hang (text "in")  2 (ppr cmd)]

ppr_cmd (HsCmdDo (L _ stmts))  = pprDo ArrowExpr stmts

ppr_cmd (HsCmdArrApp arrow arg HsFirstOrderApp True)
  = hsep [ppr_lexpr arrow, larrowt, ppr_lexpr arg]
ppr_cmd (HsCmdArrApp arrow arg HsFirstOrderApp False)
  = hsep [ppr_lexpr arg, arrowt, ppr_lexpr arrow]
ppr_cmd (HsCmdArrApp arrow arg HsHigherOrderApp True)
  = hsep [ppr_lexpr arrow, larrowtt, ppr_lexpr arg]
ppr_cmd (HsCmdArrApp arrow arg HsHigherOrderApp False)
  = hsep [ppr_lexpr arg, arrowtt, ppr_lexpr arrow]

ppr_cmd (HsCmdArrForm (L _ (HsVar (L _ v))) (Just _) [arg1, arg2])
  = sep [pprCmdArg (unLoc arg1), hsep [pprInfixOcc v, pprCmdArg (unLoc arg2)]]
ppr_cmd (HsCmdArrForm op _ args)
  = hang (text "(|" <> ppr_lexpr op)
         4 (sep (map (pprCmdArg.unLoc) args) <> text "|)")

pprCmdArg :: (OutputableBndr id) => HsCmdTop id -> SDoc
pprCmdArg (HsCmdTop cmd@(L _ (HsCmdArrForm _ Nothing [])))
  = ppr_lcmd cmd
pprCmdArg (HsCmdTop cmd)
  = parens (ppr_lcmd cmd)

instance (OutputableBndr id) => Outputable (HsCmdTop id) where
    ppr = pprCmdArg

-- We know the list must have at least one @Match@ in it.

pprMatches :: (OutputableBndr idR, Outputable body)
           => MatchGroup idR body -> SDoc
pprMatches MG { mg_alts = matches }
    = vcat (map pprMatch (map unLoc (unLoc matches)))
      -- Don't print the type; it's only a place-holder before typechecking

-- Exported to HsBinds, which can't see the defn of HsMatchContext
pprFunBind :: (OutputableBndr idR, Outputable body)
           => MatchGroup idR body -> SDoc
pprFunBind matches = pprMatches matches

-- Exported to HsBinds, which can't see the defn of HsMatchContext
pprPatBind :: forall bndr id body. (OutputableBndr bndr,
                                    OutputableBndr id, Outputable body)
           => LPat bndr -> GRHSs id body -> SDoc
pprPatBind pat (grhss)
 = sep [ppr pat, nest 2 (pprGRHSs (PatBindRhs :: HsMatchContext id) grhss)]

pprMatch :: (OutputableBndr idR, Outputable body) => Match idR body -> SDoc
pprMatch match
  = sep [ sep (herald : map (nest 2 . pprParendLPat) other_pats)
        , nest 2 ppr_maybe_ty
        , nest 2 (pprGRHSs ctxt (m_grhss match)) ]
  where
    ctxt = m_ctxt match
    (herald, other_pats)
        = case ctxt of
            FunRhs (L _ fun) fixity
                | fixity == Prefix -> (pprPrefixOcc fun, m_pats match)
                        -- f x y z = e
                        -- Not pprBndr; the AbsBinds will
                        -- have printed the signature

                | null pats2 -> (pp_infix, [])
                        -- x &&& y = e

                | otherwise -> (parens pp_infix, pats2)
                        -- (x &&& y) z = e
                where
                  pp_infix = pprParendLPat pat1 <+> pprInfixOcc fun <+> pprParendLPat pat2

            LambdaExpr -> (char '\\', m_pats match)

            _  -> (ppr pat1, [])        -- No parens around the single pat

    (pat1:pats1) = m_pats match
    (pat2:pats2) = pats1
    ppr_maybe_ty = case m_type match of
                        Just ty -> dcolon <+> ppr ty
                        Nothing -> empty


pprGRHSs :: (OutputableBndr idR, Outputable body)
         => HsMatchContext idL -> GRHSs idR body -> SDoc
pprGRHSs ctxt (GRHSs grhss (L _ binds))
  = vcat (map (pprGRHS ctxt . unLoc) grhss)
 $$ ppUnless (isEmptyLocalBinds binds)
      (text "where" $$ nest 4 (pprBinds binds))

isEmptyLocalBinds :: HsLocalBindsLR a b -> Bool
isEmptyLocalBinds (HsValBinds ds) = isEmptyValBinds ds
isEmptyLocalBinds (HsIPBinds ds)  = isEmptyIPBinds ds
isEmptyLocalBinds EmptyLocalBinds = True

isEmptyIPBinds :: HsIPBinds id -> Bool
isEmptyIPBinds (IPBinds is) = null is

isEmptyValBinds :: HsValBindsLR a b -> Bool
isEmptyValBinds (ValBindsIn ds sigs)  = isEmptyLHsBinds ds && null sigs

isEmptyLHsBinds :: LHsBindsLR idL idR -> Bool
isEmptyLHsBinds = isEmptyBag

pprGRHS :: (OutputableBndr idR, Outputable body)
        => HsMatchContext idL -> GRHS idR body -> SDoc
pprGRHS ctxt (GRHS [] body)
 =  pp_rhs ctxt body

pprGRHS ctxt (GRHS guards body)
 = sep [vbar <+> interpp'SP guards, pp_rhs ctxt body]

pp_rhs :: Outputable body => HsMatchContext idL -> body -> SDoc
pp_rhs ctxt rhs = matchSeparator ctxt <+> pprDeeper (ppr rhs)

instance (OutputableBndr idL) => Outputable (ParStmtBlock idL idR) where
  ppr (ParStmtBlock stmts _) = interpp'SP stmts

instance (OutputableBndr idL, OutputableBndr idR, Outputable body)
         => Outputable (StmtLR idL idR body) where
    ppr stmt = pprStmt stmt

pprStmt :: forall idL idR body . (OutputableBndr idL, OutputableBndr idR,
                                  Outputable body)
        => (StmtLR idL idR body) -> SDoc
pprStmt (LastStmt expr ret_stripped)
  = ifPprDebug (text "[last]") <+>
       (if ret_stripped then text "return" else empty) <+>
       ppr expr
pprStmt (BindStmt pat expr) = hsep [ppr pat, larrow, ppr expr]
pprStmt (LetStmt (L _ binds))     = hsep [text "let", pprBinds binds]
pprStmt (BodyStmt expr)     = ppr expr
pprStmt (ParStmt stmtss )      = sep (punctuate (text " | ") (map ppr stmtss))

pprStmt (TransStmt { trS_stmts = stmts, trS_by = by, trS_using = using, trS_form = form })
  = sep $ punctuate comma (map ppr stmts ++ [pprTransStmt by using form])

pprStmt (RecStmt { recS_stmts = segment, recS_rec_ids = rec_ids
                 , recS_later_ids = later_ids })
  = text "rec" <+>
    vcat [ ppr_do_stmts segment
         , ifPprDebug (vcat [ text "rec_ids=" <> ppr rec_ids
                            , text "later_ids=" <> ppr later_ids])]


pprTransStmt :: Outputable body => Maybe body -> body -> TransForm -> SDoc
pprTransStmt by using ThenForm
  = sep [ text "then", nest 2 (ppr using), nest 2 (pprBy by)]
pprTransStmt by using GroupForm
  = sep [ text "then group", nest 2 (pprBy by), nest 2 (ptext (sLit "using") <+> ppr using)]

pprBy :: Outputable body => Maybe body -> SDoc
pprBy Nothing  = empty
pprBy (Just e) = text "by" <+> ppr e

pprDo :: (OutputableBndr id, Outputable body)
      => HsStmtContext any -> [LStmt id body] -> SDoc
pprDo DoExpr        stmts = text "do"  <+> ppr_do_stmts stmts
pprDo GhciStmtCtxt  stmts = text "do"  <+> ppr_do_stmts stmts
pprDo ArrowExpr     stmts = text "do"  <+> ppr_do_stmts stmts
pprDo MDoExpr       stmts = text "mdo" <+> ppr_do_stmts stmts
pprDo ListComp      stmts = brackets    $ pprComp stmts
pprDo PArrComp      stmts = paBrackets  $ pprComp stmts
pprDo MonadComp     stmts = brackets    $ pprComp stmts
pprDo _             _     = panic "pprDo" -- PatGuard, ParStmtCxt

ppr_do_stmts :: (OutputableBndr idL, OutputableBndr idR, Outputable body)
             => [LStmtLR idL idR body] -> SDoc
-- Print a bunch of do stmts, with explicit braces and semicolons,
-- so that we are not vulnerable to layout bugs
ppr_do_stmts stmts
  = lbrace <+> pprDeeperList vcat (punctuate semi (map ppr stmts))
           <+> rbrace

pprComp :: (OutputableBndr id, Outputable body)
        => [LStmt id body] -> SDoc
pprComp quals     -- Prints:  body | qual1, ..., qualn
  | not (null quals)
  , L _ (LastStmt body _) <- last quals
  = hang (ppr body <+> vbar) 2 (pprQuals (dropTail 1 quals))
  | otherwise
  = pprPanic "pprComp" (pprQuals quals)

pprQuals :: (OutputableBndr id, Outputable body)
        => [LStmt id body] -> SDoc
-- Show list comprehension qualifiers separated by commas
pprQuals quals = interpp'SP quals

instance (OutputableBndr id) => Outputable (HsSplice id) where
  ppr s = pprSplice s

pprSplice :: (OutputableBndr id) => HsSplice id -> SDoc
pprSplice (HsTypedSplice   n e)  = ppr_splice (text "$$") n e
pprSplice (HsUntypedSplice n e)  = ppr_splice (text "$")  n e
pprSplice (HsQuasiQuote n q _ s) = ppr_quasi n q s

ppr_quasi :: OutputableBndr id => id -> id -> FastString -> SDoc
ppr_quasi n quoter quote = ifPprDebug (brackets (ppr n)) <>
                           char '[' <> ppr quoter <> vbar <>
                           ppr quote <> text "|]"

ppr_splice :: (OutputableBndr id) => SDoc -> id -> LHsExpr id -> SDoc
ppr_splice herald n e
    = herald <> ifPprDebug (brackets (ppr n)) <> eDoc
    where
          -- We use pprLExpr to match pprParendLExpr:
          --     Using pprLExpr makes sure that we go 'deeper'
          --     I think that is usually (always?) right
          pp_as_was = pprLExpr e
          eDoc = case unLoc e of
                 HsPar _ -> pp_as_was
                 HsVar _ -> pp_as_was
                 _ -> parens pp_as_was


instance (OutputableBndr id) => Outputable (HsBracket id) where
  ppr = pprHsBracket


pprHsBracket :: (OutputableBndr id) => HsBracket id -> SDoc
pprHsBracket (ExpBr e)   = thBrackets empty (ppr e)
pprHsBracket (PatBr p)   = thBrackets (char 'p') (ppr p)
pprHsBracket (DecBrG gp) = thBrackets (char 'd') (ppr gp)
pprHsBracket (DecBrL ds) = thBrackets (char 'd') (vcat (map ppr ds))
pprHsBracket (TypBr t)   = thBrackets (char 't') (ppr t)
pprHsBracket (VarBr True n)  = char '\''         <> ppr n
pprHsBracket (VarBr False n) = text "''" <> ppr n
pprHsBracket (TExpBr e)  = thTyBrackets (ppr e)

thBrackets :: SDoc -> SDoc -> SDoc
thBrackets pp_kind pp_body = char '[' <> pp_kind <> vbar <+>
                             pp_body <+> text "|]"

thTyBrackets :: SDoc -> SDoc
thTyBrackets pp_body = text "[||" <+> pp_body <+> ptext (sLit "||]")

instance (OutputableBndr id) => Outputable (ArithSeqInfo id) where
    ppr (From e1)             = hcat [ppr e1, pp_dotdot]
    ppr (FromThen e1 e2)      = hcat [ppr e1, comma, space, ppr e2, pp_dotdot]
    ppr (FromTo e1 e3)        = hcat [ppr e1, pp_dotdot, ppr e3]
    ppr (FromThenTo e1 e2 e3)
      = hcat [ppr e1, comma, space, ppr e2, pp_dotdot, ppr e3]

pp_dotdot :: SDoc
pp_dotdot = text " .. "

instance Outputable FunctionFixity where
  ppr Prefix = text "Prefix"
  ppr Infix  = text "Infix"


matchSeparator :: HsMatchContext id -> SDoc
matchSeparator (FunRhs {})  = text "="
matchSeparator CaseAlt      = text "->"
matchSeparator IfAlt        = text "->"
matchSeparator LambdaExpr   = text "->"
matchSeparator ProcExpr     = text "->"
matchSeparator PatBindRhs   = text "="
matchSeparator (StmtCtxt _) = text "<-"
matchSeparator RecUpd       = panic "unused"
matchSeparator ThPatSplice  = panic "unused"
matchSeparator ThPatQuote   = panic "unused"
matchSeparator PatSyn       = panic "unused"

-- HsDoc
--------------------------------------------------------------------------------
instance Outputable HsDocString where
  ppr (HsDocString fs) = ftext fs

ppr_mbDoc :: Maybe LHsDocString -> SDoc
ppr_mbDoc (Just doc) = ppr doc
ppr_mbDoc Nothing    = empty

-- HsImpExp
--------------------------------------------------------------------------------

instance (OutputableBndr name, HasOccName name) => Outputable (ImportDecl name) where
    ppr (ImportDecl { ideclName = mod', ideclPkgQual = pkg
                    , ideclSource = from, ideclSafe = safe
                    , ideclQualified = qual, ideclImplicit = implicit
                    , ideclAs = as, ideclHiding = spec })
      = hang (hsep [text "import", ppr_imp from, pp_implicit implicit, pp_safe safe,
                    pp_qual qual, pp_pkg pkg, ppr mod', pp_as as])
             4 (pp_spec spec)
      where
        pp_implicit False = empty
        pp_implicit True = ptext (sLit ("(implicit)"))

        pp_pkg Nothing                     = empty
        pp_pkg (Just (StringLiteral _ p)) = doubleQuotes (ftext p)

        pp_qual False   = empty
        pp_qual True    = text "qualified"

        pp_safe False   = empty
        pp_safe True    = text "safe"

        pp_as Nothing   = empty
        pp_as (Just a)  = text "as" <+> ppr a

        ppr_imp True  = text "{-# SOURCE #-}"
        ppr_imp False = empty

        pp_spec Nothing             = empty
        pp_spec (Just (False, (L _ ies))) = ppr_ies ies
        pp_spec (Just (True, (L _ ies))) = text "hiding" <+> ppr_ies ies

        ppr_ies []  = text "()"
        ppr_ies ies = char '(' <+> interpp'SP ies <+> char ')'

pprImpExp :: (HasOccName name, OutputableBndr name) => name -> SDoc
pprImpExp name = type_pref <+> pprPrefixOcc name
    where
    occ = occName name
    type_pref | isTcOcc occ && isSymOcc occ = text "type"
              | otherwise                   = empty

instance (HasOccName name, OutputableBndr name) => Outputable (IE name) where
    ppr (IEVar          var)    = pprPrefixOcc (unLoc var)
    ppr (IEThingAbs     thing)  = pprImpExp (unLoc thing)
    ppr (IEThingAll      thing) = hcat [pprImpExp (unLoc thing), text "(..)"]
    ppr (IEThingWith thing wc withs flds)
        = pprImpExp (unLoc thing) <> parens (fsep (punctuate comma
                                              (ppWiths ++
                                              map (ppr . flLabel . unLoc) flds)))
      where
        ppWiths =
          case wc of
              NoIEWildcard ->
                map (pprImpExp . unLoc) withs
              IEWildcard pos ->
                let (bs, as) = splitAt pos (map (pprImpExp . unLoc) withs)
                in bs ++ [text ".."] ++ as
    ppr (IEModuleContents mod')
        = text "module" <+> ppr mod'
    ppr (IEGroup n _)           = text ("<IEGroup: " ++ show n ++ ">")
    ppr (IEDoc doc)             = ppr doc
    ppr (IEDocNamed string)     = text ("<IEDocNamed: " ++ string ++ ">")

-- HsSyn
-------------------------------------------------------------------------------

instance (OutputableBndr name, HasOccName name)
        => Outputable (HsModule name) where

    ppr (HsModule Nothing _ imports decls _ mbDoc)
      = pp_mb mbDoc $$ pp_nonnull imports
                    $$ pp_nonnull decls

    ppr (HsModule (Just name) exports imports decls deprec mbDoc)
      = vcat [
            pp_mb mbDoc,
            case exports of
              Nothing -> pp_header (text "where")
              Just es -> vcat [
                           pp_header lparen,
                           nest 8 (fsep (punctuate comma (map ppr (unLoc es)))),
                           nest 4 (text ") where")
                          ],
            pp_nonnull imports,
            pp_nonnull decls
          ]
      where
        pp_header rest = case deprec of
           Nothing -> pp_modname <+> rest
           Just d -> vcat [ pp_modname, ppr d, rest ]

        pp_modname = text "module" <+> ppr name

pp_mb :: Outputable t => Maybe t -> SDoc
pp_mb (Just x) = ppr x
pp_mb Nothing  = empty

pp_nonnull :: Outputable t => [t] -> SDoc
pp_nonnull [] = empty
pp_nonnull xs = vcat (map ppr xs)

-- SrcLoc
-------------------------------------------------------------------------------

instance Outputable RealSrcLoc where
    ppr (SrcLoc src_path src_line src_col)
      = hcat [ pprFastFilePath src_path <> colon
             , int src_line <> colon
             , int src_col ]

-- I don't know why there is this style-based difference
--        if userStyle sty || debugStyle sty then
--            hcat [ pprFastFilePath src_path, char ':',
--                   int src_line,
--                   char ':', int src_col
--                 ]
--        else
--            hcat [text "{-# LINE ", int src_line, space,
--                  char '\"', pprFastFilePath src_path, text " #-}"]

instance Outputable SrcLoc where
    ppr (RealSrcLoc l) = ppr l
    ppr (UnhelpfulLoc s)  = ftext s

instance Outputable RealSrcSpan where
    ppr span = pprUserRealSpan True span

-- I don't know why there is this style-based difference
--      = getPprStyle $ \ sty ->
--        if userStyle sty || debugStyle sty then
--           text (showUserRealSpan True span)
--        else
--           hcat [text "{-# LINE ", int (srcSpanStartLine span), space,
--                 char '\"', pprFastFilePath $ srcSpanFile span, text " #-}"]

instance Outputable SrcSpan where
    ppr span = pprUserSpan True span

-- I don't know why there is this style-based difference
--      = getPprStyle $ \ sty ->
--        if userStyle sty || debugStyle sty then
--           pprUserSpan True span
--        else
--           case span of
--           UnhelpfulSpan _ -> panic "Outputable UnhelpfulSpan"
--           RealSrcSpan s -> ppr s

pprUserSpan :: Bool -> SrcSpan -> SDoc
pprUserSpan _         (UnhelpfulSpan s) = ftext s
pprUserSpan show_path (RealSrcSpan s)   = pprUserRealSpan show_path s

pprUserRealSpan :: Bool -> RealSrcSpan -> SDoc
pprUserRealSpan show_path span@(RealSrcSpan' src_path line col _ _)
  | isPointRealSpan span
  = hcat [ ppWhen show_path (pprFastFilePath src_path <> colon)
         , int line <> colon
         , int col ]

pprUserRealSpan show_path span@(RealSrcSpan' src_path line scol _ ecol)
  | isOneLineRealSpan span
  = hcat [ ppWhen show_path (pprFastFilePath src_path <> colon)
         , int line <> colon
         , int scol
         , ppUnless (ecol - scol <= 1) (char '-' <> int (ecol - 1)) ]
            -- For single-character or point spans, we just
            -- output the starting column number

pprUserRealSpan show_path (RealSrcSpan' src_path sline scol eline ecol)
  = hcat [ ppWhen show_path (pprFastFilePath src_path <> colon)
         , parens (int sline <> comma <> int scol)
         , char '-'
         , parens (int eline <> comma <> int ecol') ]
 where
   ecol' = if ecol == 0 then ecol else ecol - 1

instance (Outputable l, Outputable e) => Outputable (GenLocated l e) where
  ppr (L l e) = ifPprDebug (braces (ppr l))
             $$ ppr e

-- RdrName
--------------------------------------------------------------------------------

instance Outputable RdrName where
--    ppr (Exact name)   = ppr name
    ppr (Unqual occ)   = ppr occ
    ppr (Qual mod occ) = ppr mod <> dot <> ppr occ
    ppr (Orig mod occ) = getPprStyle (\sty -> pprModulePrefix sty mod occ <> ppr occ)
    ppr (BuiltIn n)    = ppr n

-- SHAYAN HACK: the following can be wrong!
instance Outputable BuiltInNames where
 ppr UnicodeStarKindTyCon = text "★"
 ppr StarKindTyCon        = text "*"
 ppr (TupleDataCon b i)   = case b of
                             Boxed   -> text (mkBoxedTupleStr i)
                             Unboxed -> text (mkUnboxedTupleStr i)
 ppr (CTupleTyCon  i)     = text (mkConstraintTupleStr i)
 ppr (TupleTyCon   b i)   = case b of
                             Boxed   -> text (mkBoxedTupleStr i)
                             Unboxed -> text (mkUnboxedTupleStr i)
 ppr NilDataCon           = text "[]"
 ppr ConsDataCon          = text ":"
 ppr ListTyCon            = text "[]"
 ppr ParrTyCon            = text "[::]"
 ppr FunTyCon             = text "->"
 ppr EqTyCon              = text "~"
 ppr EqPrimTyCon          = text "#~"
 ppr Forall_tv            = text "forall"

mkBoxedTupleStr :: Arity -> String
mkBoxedTupleStr 0  = "()"
mkBoxedTupleStr 1  = "Unit"   -- See Note [One-tuples]
mkBoxedTupleStr ar = '(' : commas ar ++ ")"

mkUnboxedTupleStr :: Arity -> String
mkUnboxedTupleStr 0  = "(##)"
mkUnboxedTupleStr 1  = "Unit#"  -- See Note [One-tuples]
mkUnboxedTupleStr ar = "(#" ++ commas ar ++ "#)"

mkConstraintTupleStr :: Arity -> String
mkConstraintTupleStr 0  = "(%%)"
mkConstraintTupleStr 1  = "Unit%"   -- See Note [One-tuples]
mkConstraintTupleStr ar = "(%" ++ commas ar ++ "%)"

commas :: Arity -> String
commas ar = take (ar-1) (repeat ',')


instance OutputableBndr RdrName where
    pprBndr _ n
        | isTvOcc (rdrNameOcc n) = char '@' <+> ppr n
        | otherwise              = ppr n

    pprInfixOcc  rdr = pprInfixVar  (isSymOcc (rdrNameOcc rdr)) (ppr rdr)
    pprPrefixOcc rdr
--      | Just name <- isExact_maybe rdr = pprPrefixName name
             -- pprPrefixName has some special cases, so
             -- we delegate to them rather than reproduce them
      | otherwise = pprPrefixVar (isSymOcc (rdrNameOcc rdr)) (ppr rdr)


-- Name
--------------------------------------------------------------------------------

pprModulePrefix :: PprStyle -> Module -> OccName -> SDoc
-- Print the "M." part of a name, based on whether it's in scope or not
-- See Note [Printing original names] in HscTypes
pprModulePrefix sty mod occ = sdocWithDynFlags $ \dflags ->
  if gopt Opt_SuppressModulePrefixes dflags
  then empty
  else
    case qualName sty (moduleName mod) occ of              -- See Outputable.QualifyName:
      NameQual modname -> ppr modname <> dot       -- Name is in scope
      NameNotInScope1  -> ppr mod <> dot           -- Not in scope
      NameNotInScope2  -> ppr (moduleUnitId mod) <> colon     -- Module not in
                          <> ppr (moduleName mod) <> dot          -- scope either
      NameUnqual       -> empty                   -- In scope unqualified

-- Module
--------------------------------------------------------------------------------

instance Outputable ModuleName where
  ppr = pprModuleName

pprModuleName :: ModuleName -> SDoc
pprModuleName (ModuleName nm) =
    getPprStyle $ \ sty ->
    if codeStyle sty
        then ztext (zEncodeFS nm)
        else ftext nm

instance Outputable Module where
  ppr = pprModule

pprModule :: Module -> SDoc
pprModule mod@(Module p n)  =
  pprPackagePrefix p mod <> pprModuleName n

pprPackagePrefix :: UnitId -> Module -> SDoc
pprPackagePrefix p mod = getPprStyle doc
 where
   doc sty
       | codeStyle sty =
          if p == mainUnitId
                then empty -- never qualify the main package in code
                else ztext (zEncodeFS (unitIdFS p)) <> char '_'
       | qualModule sty (moduleName mod) = ppr (moduleUnitId mod) <> char ':'
                -- the PrintUnqualified tells us which modules have to
                -- be qualified with package names
       | otherwise = empty

mainUnitId :: UnitId
mainUnitId = PId (fsLit "main")

instance Outputable UnitId where
   ppr pk = ftext (unitIdFS pk)


-- Bag
--------------------------------------------------------------------------------

instance (Outputable a) => Outputable (Bag a) where
    ppr bag = braces (pprWithCommas ppr (bagToList bag))


-- Unique
--------------------------------------------------------------------------------
pprUnique :: Unique -> SDoc
pprUnique u = text (show u)

instance Outputable Unique where
    ppr = pprUnique

-- UniquFM
--------------------------------------------------------------------------------
instance Outputable a => Outputable (UniqFM a) where
    ppr ufm = pprUniqFM ppr ufm

pprUniqFM :: (a -> SDoc) -> UniqFM a -> SDoc
pprUniqFM ppr_elt ufm
  = brackets $ fsep $ punctuate comma $
    [ ppr uq <+> text ":->" <+> ppr_elt elt
    | (uq, elt) <- nonDetUFMToList ufm ]
  -- It's OK to use nonDetUFMToList here because we only use it for
  -- pretty-printing.

-- OrdList
--------------------------------------------------------------------------------
instance Outputable a => Outputable (OrdList a) where
  ppr ol = ppr (fromOL ol)  -- Convert to list and print that


-- OccName
--------------------------------------------------------------------------------
instance Outputable OccName where
    ppr = pprOccName

instance OutputableBndr OccName where
    pprBndr _ = ppr
    pprInfixOcc n = pprInfixVar (isSymOcc n) (ppr n)
    pprPrefixOcc n = pprPrefixVar (isSymOcc n) (ppr n)

pprOccName :: OccName -> SDoc
pprOccName (OccName sp occ)
  = getPprStyle $ \ sty ->
    if codeStyle sty
    then ztext (zEncodeFS occ)
    else pp_occ <> pp_debug sty
  where
    pp_debug sty | debugStyle sty = braces (pprNameSpaceBrief sp)
                 | otherwise      = empty

    pp_occ = sdocWithDynFlags $ \dflags -> ftext occ

pprNameSpaceBrief :: NameSpace -> SDoc
pprNameSpaceBrief DataName  = char 'd'
pprNameSpaceBrief VarName   = char 'v'
pprNameSpaceBrief TvName    = text "tv"
pprNameSpaceBrief TcClsName = text "tc"
