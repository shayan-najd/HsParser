{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- #name_types#
-- GHC uses several kinds of name internally:
--
-- * 'OccName.OccName': see "OccName#name_types"
--
-- * 'RdrName.RdrName' is the type of names that come directly from the parser. They
--   have not yet had their scoping and binding resolved by the renamer and can be
--   thought of to a first approximation as an 'OccName.OccName' with an optional module
--   qualifier
--
-- * 'Name.Name': see "Name#name_types"
--
-- * 'Id.Id': see "Id#name_types"
--
-- * 'Var.Var': see "Var#name_types"

module RdrName ( RdrName(..)
               , BuiltInNames(..)
               , rdrNameOcc
               ) where

import Language.Haskell.Syntax.Module (ModuleName(..),Module)
import OccName(OccName(..))
import Language.Haskell.Syntax.BasicTypes

import Data.Data

{-
************************************************************************
*                                                                      *
\subsection{The main data type}
*                                                                      *
************************************************************************
-}

-- | Do not use the data constructors of RdrName directly: prefer the family
-- of functions that creates them, such as 'mkRdrUnqual'
--
-- - Note: A Located RdrName will only have API Annotations if it is a
--         compound one,
--   e.g.
--
-- > `bar`
-- > ( ~ )
--
-- - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnType',
--           'ApiAnnotation.AnnOpen'  @'('@ or @'['@ or @'[:'@,
--           'ApiAnnotation.AnnClose' @')'@ or @']'@ or @':]'@,,
--           'ApiAnnotation.AnnBackquote' @'`'@,
--           'ApiAnnotation.AnnVal','ApiAnnotation.AnnTildehsh',
--           'ApiAnnotation.AnnTilde',

-- For details on above see note [Api annotations] in ApiAnnotation
data RdrName
  = Unqual OccName
        -- ^ Used for ordinary, unqualified occurrences, e.g. @x@, @y@ or @Foo@.
        -- Create such a 'RdrName' with 'mkRdrUnqual'

  | Qual ModuleName OccName
        -- ^ A qualified name written by the user in
        -- /source/ code.  The module isn't necessarily
        -- the module where the thing is defined;
        -- just the one from which it is imported.
        -- Examples are @Bar.x@, @Bar.y@ or @Bar.Foo@.
        -- Create such a 'RdrName' with 'mkRdrQual'

  | Orig Module OccName
        -- ^ An original name; the module is the /defining/ module.
        -- This is used when GHC generates code that will be fed
        -- into the renamer (e.g. from deriving clauses), but where
        -- we want to say \"Use Prelude.map dammit\". One of these
        -- can be created with 'mkOrig'

--  | Exact Name
        -- ^ We know exactly the 'Name'. This is used:
        --
        --  (1) When the parser parses built-in syntax like @[]@
        --      and @(,)@, but wants a 'RdrName' from it
        --
        --  (2) By Template Haskell, when TH has generated a unique name
        --
        -- Such a 'RdrName' can be created by using 'getRdrName' on a 'Name'
 | BuiltIn BuiltInNames
  deriving Data

data BuiltInNames
 = UnicodeStarKindTyCon
 | StarKindTyCon
 | CTupleTyCon  Int
 | TupleDataCon Boxity Int
 | TupleTyCon   Boxity Int
 | NilDataCon
 | ConsDataCon
 | ListTyCon
 | ParrTyCon
 | FunTyCon
 | EqTyCon
 | EqPrimTyCon
 | Forall_tv
 deriving (Data,Show)

{-
************************************************************************
*                                                                      *
\subsection{Simple functions}
*                                                                      *
************************************************************************
-}

rdrNameOcc :: RdrName -> OccName
rdrNameOcc (Qual _ occ) = occ
rdrNameOcc (Unqual occ) = occ
rdrNameOcc (Orig _ occ) = occ
--rdrNameOcc (Exact name) = nameOccName name -- SHAYAN HACK! TODO!

{-
************************************************************************
*                                                                      *
\subsection{Instances}
*                                                                      *
************************************************************************
-}

instance Eq RdrName where
--    (Exact n1)    == (Exact n2)    = n1==n2
        -- Convert exact to orig
--    (Exact n1)    == r2@(Orig _ _) = nukeExact n1 == r2
--    r1@(Orig _ _) == (Exact n2)    = r1 == nukeExact n2
--    Hack! SHAYAN TODO!

    (Orig m1 o1)  == (Orig m2 o2)  = m1==m2 && o1==o2
    (Qual m1 o1)  == (Qual m2 o2)  = m1==m2 && o1==o2
    (Unqual o1)   == (Unqual o2)   = o1==o2
    _             == _             = False
