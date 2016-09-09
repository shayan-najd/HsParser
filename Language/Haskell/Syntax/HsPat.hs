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

module Language.Haskell.Syntax.HsPat
             ( LPat
             , LHsRecUpdField
             , LHsRecField
             , HsRecFields(..)
             , HsConPatDetails(..)
             , HsRecField'(..)
             , HsRecField(..)
             , HsRecUpdField(..)
             , Pat(..)
             ) where

import {-# SOURCE #-} Language.Haskell.Syntax.HsExpr (LHsExpr, HsSplice)

-- friends:
import Language.Haskell.Syntax.HsLit
import Language.Haskell.Syntax.HsTypes
import Language.Haskell.Syntax.SrcLoc
import Language.Haskell.Syntax.BasicTypes

-- libraries:
import Data.Data hiding (TyCon,Fixity)

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
