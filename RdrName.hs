{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

{-# LANGUAGE CPP, DeriveDataTypeable #-}

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

module RdrName (
        -- * The main type
        RdrName(..),    -- Constructors exported only to BinIface

        -- ** Construction
        mkRdrUnqual, mkRdrQual,
        mkUnqual, mkVarUnqual, mkQual, mkOrig,
        nameRdrName, getRdrName,

        -- ** Destruction
        rdrNameOcc, rdrNameSpace, demoteRdrName,
        isRdrDataCon, isRdrTyVar, isRdrTc, isQual, isQual_maybe, isUnqual,
        isOrig, isOrig_maybe, isExact, isExact_maybe, isSrcRdrName,
        ) where

#include "HsVersions.h"

import Module (ModuleName,Module,mkModuleNameFS)
import Name
import U.FastString
import U.Outputable
import U.Util
import OccName(HasOccName(..),OccName(..),NameSpace,
               isSymOcc,isTvOcc,demoteOccName,
               mkOccNameFS,mkVarOccFS,isDataOcc,isTcOcc)
import U.Panic


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

  | Exact Name
        -- ^ We know exactly the 'Name'. This is used:
        --
        --  (1) When the parser parses built-in syntax like @[]@
        --      and @(,)@, but wants a 'RdrName' from it
        --
        --  (2) By Template Haskell, when TH has generated a unique name
        --
        -- Such a 'RdrName' can be created by using 'getRdrName' on a 'Name'
  deriving Data

{-
************************************************************************
*                                                                      *
\subsection{Simple functions}
*                                                                      *
************************************************************************
-}

instance HasOccName RdrName where
  occName = rdrNameOcc

rdrNameOcc :: RdrName -> OccName
rdrNameOcc (Qual _ occ) = occ
rdrNameOcc (Unqual occ) = occ
rdrNameOcc (Orig _ occ) = occ
rdrNameOcc (Exact name) = nameOccName name

rdrNameSpace :: RdrName -> NameSpace
rdrNameSpace = occNameSpace . rdrNameOcc

-- demoteRdrName lowers the NameSpace of RdrName.
-- see Note [Demotion] in OccName
demoteRdrName :: RdrName -> Maybe RdrName
demoteRdrName (Unqual occ) = fmap Unqual (demoteOccName occ)
demoteRdrName (Qual m occ) = fmap (Qual m) (demoteOccName occ)
demoteRdrName (Orig _ _) = panic "demoteRdrName"
demoteRdrName (Exact _) = panic "demoteRdrName"

        -- These two are the basic constructors
mkRdrUnqual :: OccName -> RdrName
mkRdrUnqual occ = Unqual occ

mkRdrQual :: ModuleName -> OccName -> RdrName
mkRdrQual mod occ = Qual mod occ

mkOrig :: Module -> OccName -> RdrName
mkOrig mod occ = Orig mod occ

---------------
        -- These two are used when parsing source files
        -- They do encode the module and occurrence names
mkUnqual :: NameSpace -> FastString -> RdrName
mkUnqual sp n = Unqual (mkOccNameFS sp n)

mkVarUnqual :: FastString -> RdrName
mkVarUnqual n = Unqual (mkVarOccFS n)

-- | Make a qualified 'RdrName' in the given namespace and where the 'ModuleName' and
-- the 'OccName' are taken from the first and second elements of the tuple respectively
mkQual :: NameSpace -> (FastString, FastString) -> RdrName
mkQual sp (m, n) = Qual (mkModuleNameFS m) (mkOccNameFS sp n)

getRdrName :: NamedThing thing => thing -> RdrName
getRdrName name = nameRdrName (getName name)

nameRdrName :: Name -> RdrName
nameRdrName name = Exact name
-- Keep the Name even for Internal names, so that the
-- unique is still there for debug printing, particularly
-- of Types (which are converted to IfaceTypes before printing)

nukeExact :: Name -> RdrName
nukeExact n
  | isExternalName n = Orig (nameModule n) (nameOccName n)
  | otherwise        = Unqual (nameOccName n)

isRdrDataCon :: RdrName -> Bool
isRdrTyVar   :: RdrName -> Bool
isRdrTc      :: RdrName -> Bool

isRdrDataCon rn = isDataOcc (rdrNameOcc rn)
isRdrTyVar   rn = isTvOcc   (rdrNameOcc rn)
isRdrTc      rn = isTcOcc   (rdrNameOcc rn)

isSrcRdrName :: RdrName -> Bool
isSrcRdrName (Unqual _) = True
isSrcRdrName (Qual _ _) = True
isSrcRdrName _          = False

isUnqual :: RdrName -> Bool
isUnqual (Unqual _) = True
isUnqual _          = False

isQual :: RdrName -> Bool
isQual (Qual _ _) = True
isQual _          = False

isQual_maybe :: RdrName -> Maybe (ModuleName, OccName)
isQual_maybe (Qual m n) = Just (m,n)
isQual_maybe _          = Nothing

isOrig :: RdrName -> Bool
isOrig (Orig _ _) = True
isOrig _          = False

isOrig_maybe :: RdrName -> Maybe (Module, OccName)
isOrig_maybe (Orig m n) = Just (m,n)
isOrig_maybe _          = Nothing

isExact :: RdrName -> Bool
isExact (Exact _) = True
isExact _         = False

isExact_maybe :: RdrName -> Maybe Name
isExact_maybe (Exact n) = Just n
isExact_maybe _         = Nothing

{-
************************************************************************
*                                                                      *
\subsection{Instances}
*                                                                      *
************************************************************************
-}

instance Outputable RdrName where
    ppr (Exact name)   = ppr name
    ppr (Unqual occ)   = ppr occ
    ppr (Qual mod occ) = ppr mod <> dot <> ppr occ
    ppr (Orig mod occ) = getPprStyle (\sty -> pprModulePrefix sty mod occ <> ppr occ)

instance OutputableBndr RdrName where
    pprBndr _ n
        | isTvOcc (rdrNameOcc n) = char '@' <+> ppr n
        | otherwise              = ppr n

    pprInfixOcc  rdr = pprInfixVar  (isSymOcc (rdrNameOcc rdr)) (ppr rdr)
    pprPrefixOcc rdr
      | Just name <- isExact_maybe rdr = pprPrefixName name
             -- pprPrefixName has some special cases, so
             -- we delegate to them rather than reproduce them
      | otherwise = pprPrefixVar (isSymOcc (rdrNameOcc rdr)) (ppr rdr)

instance Eq RdrName where
    (Exact n1)    == (Exact n2)    = n1==n2
        -- Convert exact to orig
    (Exact n1)    == r2@(Orig _ _) = nukeExact n1 == r2
    r1@(Orig _ _) == (Exact n2)    = r1 == nukeExact n2

    (Orig m1 o1)  == (Orig m2 o2)  = m1==m2 && o1==o2
    (Qual m1 o1)  == (Qual m2 o2)  = m1==m2 && o1==o2
    (Unqual o1)   == (Unqual o2)   = o1==o2
    _             == _             = False

instance Ord RdrName where
    a <= b = case (a `compare` b) of { LT -> True;  EQ -> True;  GT -> False }
    a <  b = case (a `compare` b) of { LT -> True;  EQ -> False; GT -> False }
    a >= b = case (a `compare` b) of { LT -> False; EQ -> True;  GT -> True  }
    a >  b = case (a `compare` b) of { LT -> False; EQ -> False; GT -> True  }

        -- Exact < Unqual < Qual < Orig
        -- [Note: Apr 2004] We used to use nukeExact to convert Exact to Orig
        --      before comparing so that Prelude.map == the exact Prelude.map, but
        --      that meant that we reported duplicates when renaming bindings
        --      generated by Template Haskell; e.g
        --      do { n1 <- newName "foo"; n2 <- newName "foo";
        --           <decl involving n1,n2> }
        --      I think we can do without this conversion
    compare (Exact n1) (Exact n2) = n1 `compare` n2
    compare (Exact _)  _          = LT

    compare (Unqual _)   (Exact _)    = GT
    compare (Unqual o1)  (Unqual  o2) = o1 `compare` o2
    compare (Unqual _)   _            = LT

    compare (Qual _ _)   (Exact _)    = GT
    compare (Qual _ _)   (Unqual _)   = GT
    compare (Qual m1 o1) (Qual m2 o2) = (o1 `compare` o2) `thenCmp` (m1 `compare` m2)
    compare (Qual _ _)   (Orig _ _)   = LT

    compare (Orig m1 o1) (Orig m2 o2) = (o1 `compare` o2) `thenCmp` (m1 `compare` m2)
    compare (Orig _ _)   _            = GT
