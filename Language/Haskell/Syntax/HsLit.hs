{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[HsLit]{Abstract syntax: source-language literals}
-}

{-# LANGUAGE CPP, DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder
{-# LANGUAGE ConstraintKinds #-}

module Language.Haskell.Syntax.HsLit ( HsLit(..)
             , HsOverLit(..)
             , OverLitVal(..)) where

import Language.Haskell.Syntax.BasicTypes ( FractionalLit(..),SourceText )
import Language.Haskell.Utility.FastString

import Data.ByteString (ByteString)
import Data.Data hiding ( Fixity )

{-
************************************************************************
*                                                                      *
\subsection[HsLit]{Literals}
*                                                                      *
************************************************************************
-}

-- Note [Literal source text] in BasicTypes for SourceText fields in
-- the following
data HsLit
  = HsChar          SourceText Char        -- Character
  | HsCharPrim      SourceText Char        -- Unboxed character
  | HsString        SourceText FastString  -- String
  | HsStringPrim    SourceText ByteString  -- Packed bytes
  | HsIntPrim       SourceText Integer     -- literal Int#
  | HsWordPrim      SourceText Integer     -- literal Word#
  | HsInt64Prim     SourceText Integer     -- literal Int64#
  | HsWord64Prim    SourceText Integer     -- literal Word64#
  | HsFloatPrim     FractionalLit      -- Unboxed Float
  | HsDoublePrim    FractionalLit      -- Unboxed Double
  deriving Data

instance Eq HsLit where
  (HsChar _ x1)       == (HsChar _ x2)       = x1==x2
  (HsCharPrim _ x1)   == (HsCharPrim _ x2)   = x1==x2
  (HsString _ x1)     == (HsString _ x2)     = x1==x2
  (HsStringPrim _ x1) == (HsStringPrim _ x2) = x1==x2
  (HsWordPrim _ x1)   == (HsWordPrim _ x2)   = x1==x2
  (HsInt64Prim _ x1)  == (HsInt64Prim _ x2)  = x1==x2
  (HsWord64Prim _ x1) == (HsWord64Prim _ x2) = x1==x2
  _                   == _                   = False

data HsOverLit id       -- An overloaded literal
  = OverLit {
        ol_val :: OverLitVal }
deriving instance (Data id) => Data (HsOverLit id)

-- Note [Literal source text] in BasicTypes for SourceText fields in
-- the following
data OverLitVal
  = HsIntegral   !SourceText !Integer    -- Integer-looking literals;
  | HsFractional !FractionalLit          -- Frac-looking literals
  | HsIsString   !SourceText !FastString -- String-looking literals
  deriving Data


{-
Note [ol_rebindable]
~~~~~~~~~~~~~~~~~~~~
The ol_rebindable field is True if this literal is actually
using rebindable syntax.  Specifically:

  False iff ol_witness is the standard one
  True  iff ol_witness is non-standard

Equivalently it's True if
  a) RebindableSyntax is on
  b) the witness for fromInteger/fromRational/fromString
     that happens to be in scope isn't the standard one

Note [Overloaded literal witnesses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*Before* type checking, the HsExpr in an HsOverLit is the
name of the coercion function, 'fromInteger' or 'fromRational'.
*After* type checking, it is a witness for the literal, such as
        (fromInteger 3) or lit_78
This witness should replace the literal.

This dual role is unusual, because we're replacing 'fromInteger' with
a call to fromInteger.  Reason: it allows commoning up of the fromInteger
calls, which wouldn't be possible if the desguarar made the application.

The PostTcType in each branch records the type the overload literal is
found to have.
-}

-- Comparison operations are needed when grouping literals
-- for compiling pattern-matching (module MatchLit)
instance Eq (HsOverLit id) where
  (OverLit {ol_val = val1}) == (OverLit {ol_val=val2}) = val1 == val2

instance Eq OverLitVal where
  (HsIntegral _ i1)   == (HsIntegral _ i2)   = i1 == i2
  (HsFractional f1)   == (HsFractional f2)   = f1 == f2
  (HsIsString _ s1)   == (HsIsString _ s2)   = s1 == s2
  _                   == _                   = False

instance Ord (HsOverLit id) where
  compare (OverLit {ol_val=val1}) (OverLit {ol_val=val2}) = val1 `compare` val2

instance Ord OverLitVal where
  compare (HsIntegral _ i1)   (HsIntegral _ i2)   = i1 `compare` i2
  compare (HsIntegral _ _)    (HsFractional _)    = LT
  compare (HsIntegral _ _)    (HsIsString _ _)    = LT
  compare (HsFractional f1)   (HsFractional f2)   = f1 `compare` f2
  compare (HsFractional _)    (HsIntegral _ _)    = GT
  compare (HsFractional _)    (HsIsString _ _)    = LT
  compare (HsIsString _ s1)   (HsIsString _ s2)   = s1 `compare` s2
  compare (HsIsString _ _)    (HsIntegral _ _)    = GT
  compare (HsIsString _ _)    (HsFractional _)    = GT
