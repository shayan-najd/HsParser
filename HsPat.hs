{-# OPTIONS_GHC -fwarn-unused-imports #-}
{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[PatSyntax]{Abstract Haskell syntax---patterns}
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module HsPat (
        Pat(..), InPat, LPat,

        HsConPatDetails,
        HsRecFields(..), HsRecField'(..), LHsRecField',
        HsRecField, LHsRecField,
        HsRecUpdField, LHsRecUpdField,

        hsRecFieldsArgs,


        isUnliftedHsBind, looksLazyPatBind,
        isUnliftedLPat, isBangedLPat, isBangedPatBind,
        hsPatNeedsParens,
        isIrrefutableHsPat,

        pprParendLPat, pprConArgs
    ) where

import {-# SOURCE #-} HsExpr            (LHsExpr, HsSplice, pprLExpr, pprSplice)

-- friends:
import HsBinds
import HsLit
import HsTypes
import SrcLoc

import BasicTypes
import Outputable

-- libraries:
import Data.Data hiding (TyCon,Fixity)


type InPat id  = LPat id        -- No 'Out' constructors
type LPat id = Located (Pat id)

-- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnBang'

-- For details on above see note [Api annotations] in ApiAnnotation
data Pat id
  =     ------------ Simple patterns ---------------
    WildPat

  | VarPat      (Located id) -- Variable
                             -- See Note [Located RdrNames] in HsExpr
  | LazyPat     (LPat id)               -- Lazy pattern
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnTilde'

    -- For details on above see note [Api annotations] in ApiAnnotation

  | AsPat       (Located id) (LPat id)  -- As pattern
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnAt'

    -- For details on above see note [Api annotations] in ApiAnnotation

  | ParPat      (LPat id)               -- Parenthesised pattern
                                        -- See Note [Parens in HsSyn] in HsExpr
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'('@,
    --                                    'ApiAnnotation.AnnClose' @')'@

    -- For details on above see note [Api annotations] in ApiAnnotation
  | BangPat     (LPat id)               -- Bang pattern
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnBang'

    -- For details on above see note [Api annotations] in ApiAnnotation

        ------------ Lists, tuples, arrays ---------------
  | ListPat     [LPat id]                            -- Syntactic list

  | TuplePat    [LPat id]        -- Tuple sub-patterns
                Boxity           -- UnitPat is TuplePat []
    -- For details on above see note [Api annotations] in ApiAnnotation
  | PArrPat     [LPat id]               -- Syntactic parallel array

    -- For details on above see note [Api annotations] in ApiAnnotation
        ------------ Constructor patterns ---------------
  | ConPatIn    (Located id)
                (HsConPatDetails id)
        ------------ View patterns ---------------
  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnRarrow'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | ViewPat       (LHsExpr id)
                  (LPat id)

        ------------ Pattern splices ---------------
  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'$('@
  --        'ApiAnnotation.AnnClose' @')'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | SplicePat       (HsSplice id)   -- Includes quasi-quotes

        ------------ Literal and n+k patterns ---------------
  | LitPat          HsLit               -- Used for *non-overloaded* literal patterns:
                                        -- Int#, Char#, Int, Char, String, etc.
  -- For details on above see note [Api annotations] in ApiAnnotation
  | NPlusKPat       (Located id)        -- n+k pattern
                    (Located (HsOverLit id)) -- It'll always be an HsIntegral

        ------------ Pattern type signatures ---------------
  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnDcolon'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | SigPatIn        (LPat id)                 -- Pattern with a type signature
                    (LHsSigWcType id)         -- Signature can bind both
                                              -- kind and type vars

deriving instance (Data id) => Data (Pat id)

type HsConPatDetails id = HsConDetails (LPat id) (HsRecFields id (LPat id))
{-
hsConPatArgs :: HsConPatDetails id -> [LPat id]
hsConPatArgs (PrefixCon ps)   = ps
hsConPatArgs (RecCon fs)      = map (hsRecFieldArg . unLoc) (rec_flds fs)
hsConPatArgs (InfixCon p1 p2) = [p1,p2]
-}
-- HsRecFields is used only for patterns and expressions (not data type
-- declarations)

data HsRecFields id arg         -- A bunch of record fields
                                --      { x = 3, y = True }
        -- Used for both expressions and patterns
  = HsRecFields { rec_flds   :: [LHsRecField id arg],
                  rec_dotdot :: Maybe Int }  -- Note [DotDot fields]
  deriving (Functor, Foldable, Traversable)
deriving instance (Data id, Data arg) => Data (HsRecFields id arg)


-- Note [DotDot fields]
-- ~~~~~~~~~~~~~~~~~~~~
-- The rec_dotdot field means this:
--   Nothing => the normal case
--   Just n  => the group uses ".." notation,
--
-- In the latter case:
--
--   *before* renamer: rec_flds are exactly the n user-written fields
--
--   *after* renamer:  rec_flds includes *all* fields, with
--                     the first 'n' being the user-written ones
--                     and the remainder being 'filled in' implicitly

type LHsRecField' id arg = Located (HsRecField' id arg)
type LHsRecField  id arg = Located (HsRecField  id arg)
type LHsRecUpdField id   = Located (HsRecUpdField id)

type HsRecField    id arg = HsRecField' (FieldOcc id) arg
type HsRecUpdField id     = HsRecField' (AmbiguousFieldOcc id) (LHsExpr id)

-- |  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnEqual',
--
-- For details on above see note [Api annotations] in ApiAnnotation
data HsRecField' id arg = HsRecField {
        hsRecFieldLbl :: Located id,
        hsRecFieldArg :: Maybe arg           -- ^ Filled in by renamer when punning
  } deriving (Data, Functor, Foldable, Traversable)


-- Note [Punning]
-- ~~~~~~~~~~~~~~
-- If you write T { x, y = v+1 }, the HsRecFields will be
--      HsRecField x x True ...
--      HsRecField y (v+1) False ...
-- That is, for "punned" field x is expanded (in the renamer)
-- to x=x; but with a punning flag so we can detect it later
-- (e.g. when pretty printing)
--
-- If the original field was qualified, we un-qualify it, thus
--    T { A.x } means T { A.x = x }


-- Note [HsRecField and HsRecUpdField]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- A HsRecField (used for record construction and pattern matching)
-- contains an unambiguous occurrence of a field (i.e. a FieldOcc).
-- We can't just store the Name, because thanks to
-- DuplicateRecordFields this may not correspond to the label the user
-- wrote.
--
-- A HsRecUpdField (used for record update) contains a potentially
-- ambiguous occurrence of a field (an AmbiguousFieldOcc).  The
-- renamer will fill in the selector function if it can, but if the
-- selector is ambiguous the renamer will defer to the typechecker.
-- After the typechecker, a unique selector will have been determined.
--
-- The renamer produces an Unambiguous result if it can, rather than
-- just doing the lookup in the typechecker, so that completely
-- unambiguous updates can be represented by 'DsMeta.repUpdFields'.
--
-- For example, suppose we have:
--
--     data S = MkS { x :: Int }
--     data T = MkT { x :: Int }
--
--     f z = (z { x = 3 }) :: S
--
-- The parsed HsRecUpdField corresponding to the record update will have:
--
--     hsRecFieldLbl = Unambiguous "x" PlaceHolder :: AmbiguousFieldOcc RdrName
--
-- After the renamer, this will become:
--
--     hsRecFieldLbl = Ambiguous   "x" PlaceHolder :: AmbiguousFieldOcc Name
--
-- (note that the Unambiguous constructor is not type-correct here).
-- The typechecker will determine the particular selector:
--
--     hsRecFieldLbl = Unambiguous "x" $sel:x:MkS  :: AmbiguousFieldOcc Id
--
-- See also Note [Disambiguating record fields] in TcExpr.

-- Probably won't typecheck at once, things have changed :/
hsRecFieldsArgs :: HsRecFields id arg -> [Maybe arg]
hsRecFieldsArgs rbinds = map (hsRecFieldArg . unLoc) (rec_flds rbinds)


{-
************************************************************************
*                                                                      *
*              Printing patterns
*                                                                      *
************************************************************************
-}

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


{-
************************************************************************
*                                                                      *
*              Building patterns
*                                                                      *
************************************************************************
-}

{-
************************************************************************
*                                                                      *
* Predicates for checking things about pattern-lists in EquationInfo   *
*                                                                      *
************************************************************************

\subsection[Pat-list-predicates]{Look for interesting things in patterns}

Unlike in the Wadler chapter, where patterns are either ``variables''
or ``constructors,'' here we distinguish between:
\begin{description}
\item[unfailable:]
Patterns that cannot fail to match: variables, wildcards, and lazy
patterns.

These are the irrefutable patterns; the two other categories
are refutable patterns.

\item[constructor:]
A non-literal constructor pattern (see next category).

\item[literal patterns:]
At least the numeric ones may be overloaded.
\end{description}

A pattern is in {\em exactly one} of the above three categories; `as'
patterns are treated specially, of course.

The 1.3 report defines what ``irrefutable'' and ``failure-free'' patterns are.
-}

isUnliftedLPat :: LPat id -> Bool
isUnliftedLPat (L _ (ParPat p))             = isUnliftedLPat p
isUnliftedLPat (L _ (TuplePat _ Unboxed)) = True
isUnliftedLPat _                            = False

isUnliftedHsBind :: HsBind id -> Bool
-- A pattern binding with an outermost bang or unboxed tuple must be matched strictly
-- Defined in this module because HsPat is above HsBinds in the import graph
isUnliftedHsBind (PatBind { pat_lhs = p }) = isUnliftedLPat p
isUnliftedHsBind _                         = False

isBangedPatBind :: HsBind id -> Bool
isBangedPatBind (PatBind {pat_lhs = pat}) = isBangedLPat pat
isBangedPatBind _ = False

isBangedLPat :: LPat id -> Bool
isBangedLPat (L _ (ParPat p))   = isBangedLPat p
isBangedLPat (L _ (BangPat {})) = True
isBangedLPat _                  = False

looksLazyPatBind :: HsBind id -> Bool
-- Returns True of anything *except*
--     a StrictHsBind (as above) or
--     a VarPat
-- In particular, returns True of a pattern binding with a compound pattern, like (I# x)
looksLazyPatBind (PatBind { pat_lhs = p }) = looksLazyLPat p
looksLazyPatBind _                         = False

looksLazyLPat :: LPat id -> Bool
looksLazyLPat (L _ (ParPat p))             = looksLazyLPat p
looksLazyLPat (L _ (AsPat _ p))            = looksLazyLPat p
looksLazyLPat (L _ (BangPat {}))           = False
looksLazyLPat (L _ (TuplePat _ Unboxed)) = False
looksLazyLPat (L _ (VarPat {}))            = False
looksLazyLPat (L _ (WildPat {}))           = False
looksLazyLPat _                            = True

isIrrefutableHsPat :: (OutputableBndr id) => LPat id -> Bool
-- (isIrrefutableHsPat p) is true if matching against p cannot fail,
-- in the sense of falling through to the next pattern.
--      (NB: this is not quite the same as the (silly) defn
--      in 3.17.2 of the Haskell 98 report.)
--
-- WARNING: isIrrefutableHsPat returns False if it's in doubt.
-- Specifically on a ConPatIn, which is what it sees for a
-- (LPat Name) in the renamer, it doesn't know the size of the
-- constructor family, so it returns False.  Result: only
-- tuple patterns are considered irrefuable at the renamer stage.
--
-- But if it returns True, the pattern is definitely irrefutable
isIrrefutableHsPat pat
  = go pat
  where
    go (L _ pat) = go1 pat

    go1 (WildPat {})        = True
    go1 (VarPat {})         = True
    go1 (LazyPat {})        = True
    go1 (BangPat pat)       = go pat
    go1 (ParPat pat)        = go pat
    go1 (AsPat _ pat)       = go pat
    go1 (ViewPat _ pat)   = go pat
    go1 (SigPatIn pat _)    = go pat
    go1 (TuplePat pats _) = all go pats
    go1 (ListPat {}) = False
    go1 (PArrPat {})        = False     -- ?

    go1 (ConPatIn {})       = False     -- Conservative
    go1 (LitPat {})    = False
    go1 (NPlusKPat {}) = False

    -- Both should be gotten rid of by renamer before
    -- isIrrefutablePat is called
    go1 (SplicePat {})     = urk pat

    urk pat = pprPanic "isIrrefutableHsPat:" (ppr pat)

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
