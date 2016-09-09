{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

{-# LANGUAGE DeriveDataTypeable, BangPatterns #-}

-- |
-- #name_types#
-- GHC uses several kinds of name internally:
--
-- * 'OccName.OccName' represents names as strings with just a little more information:
--   the \"namespace\" that the name came from, e.g. the namespace of value, type constructors or
--   data constructors
--
-- * 'RdrName.RdrName': see "RdrName#name_types"
--
-- * 'Name.Name': see "Name#name_types"
--
-- * 'Id.Id': see "Id#name_types"
--
-- * 'Var.Var': see "Var#name_types"

module OccName (OccName(..),
                HasOccName(..),
                NameSpace(..),
                isSymOcc,
                mkOccNameFS,
                isDataOcc,
                tvName,
                mkVarOccFS,
                demoteOccName,
                isTvOcc,
                mkRecFldSelOcc,
                occNameString,
                tcClsName,
                varName,
                setOccNameSpace,
                isVarNameSpace,
                srcDataName,
                dataName,
                isTcOcc) where

import Language.Haskell.Utility.Util
import U.Unique
import Language.Haskell.Utility.FastString
import Lexeme
import Data.Data

{-
************************************************************************
*                                                                      *
\subsection{Name space}
*                                                                      *
************************************************************************
-}

data NameSpace = VarName        -- Variables, including "real" data constructors
               | DataName       -- "Source" data constructors
               | TvName         -- Type variables
               | TcClsName      -- Type constructors and classes; Haskell has them
                                -- in the same name space for now.
               deriving( Eq, Ord, Show )
   {-! derive: Binary !-}

-- Note [Data Constructors]
-- see also: Note [Data Constructor Naming] in DataCon.hs
--
-- $real_vs_source_data_constructors
-- There are two forms of data constructor:
--
--      [Source data constructors] The data constructors mentioned in Haskell source code
--
--      [Real data constructors] The data constructors of the representation type, which may not be the same as the source type
--
-- For example:
--
-- > data T = T !(Int, Int)
--
-- The source datacon has type @(Int, Int) -> T@
-- The real   datacon has type @Int -> Int -> T@
--
-- GHC chooses a representation based on the strictness etc.

tcClsName :: NameSpace
dataName, srcDataName      :: NameSpace
tvName, varName            :: NameSpace

-- Though type constructors and classes are in the same name space now,
-- the NameSpace type is abstract, so we can easily separate them later
tcClsName = TcClsName           -- Not sure which!

dataName    = DataName
srcDataName = DataName  -- Haskell-source data constructors should be
                        -- in the Data name space

tvName      = TvName
varName     = VarName

isVarNameSpace :: NameSpace -> Bool     -- Variables or type variables, but not constructors
isVarNameSpace TvName  = True
isVarNameSpace VarName = True
isVarNameSpace _       = False

-- demoteNameSpace lowers the NameSpace if possible.  We can not know
-- in advance, since a TvName can appear in an HsTyVar.
-- See Note [Demotion] in RnEnv
demoteNameSpace :: NameSpace -> Maybe NameSpace
demoteNameSpace VarName = Nothing
demoteNameSpace DataName = Nothing
demoteNameSpace TvName = Nothing
demoteNameSpace TcClsName = Just DataName

{-
************************************************************************
*                                                                      *
\subsection[Name-pieces-datatypes]{The @OccName@ datatypes}
*                                                                      *
************************************************************************
-}

data OccName = OccName
    { occNameSpace  :: !NameSpace
    , occNameFS     :: !FastString
    } deriving Show

instance Eq OccName where
    (OccName sp1 s1) == (OccName sp2 s2) = s1 == s2 && sp1 == sp2

instance Ord OccName where
        -- Compares lexicographically, *not* by Unique of the string
    compare (OccName sp1 s1) (OccName sp2 s2)
        = (s1  `compare` s2) `thenCmp` (sp1 `compare` sp2)

instance Data OccName where
  -- don't traverse?
  toConstr _   = abstractConstr "OccName"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNoRepType "OccName"

instance HasOccName OccName where
  occName = id

{-
Note [Suppressing uniques in OccNames]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This is a hack to de-wobblify the OccNames that contain uniques from
Template Haskell that have been turned into a string in the OccName.
See Note [Unique OccNames from Template Haskell] in Convert.hs

************************************************************************
*                                                                      *
\subsection{Construction}
*                                                                      *
************************************************************************
-}

mkOccName :: NameSpace -> String -> OccName
mkOccName occ_sp str = OccName occ_sp (mkFastString str)

mkOccNameFS :: NameSpace -> FastString -> OccName
mkOccNameFS occ_sp fs = OccName occ_sp fs

mkVarOccFS :: FastString -> OccName
mkVarOccFS fs = mkOccNameFS varName fs

-- demoteOccName lowers the Namespace of OccName.
-- see Note [Demotion]
demoteOccName :: OccName -> Maybe OccName
demoteOccName (OccName space name) = do
  space' <- demoteNameSpace space
  return $ OccName space' name


{- | Other names in the compiler add additional information to an OccName.
This class provides a consistent way to access the underlying OccName. -}
class HasOccName name where
  occName :: name -> OccName

{-
************************************************************************
*                                                                      *
                Environments
*                                                                      *
************************************************************************

OccEnvs are used mainly for the envts in ModIfaces.

Note [The Unique of an OccName]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
They are efficient, because FastStrings have unique Int# keys.  We assume
this key is less than 2^24, and indeed FastStrings are allocated keys
sequentially starting at 0.

So we can make a Unique using
        mkUnique ns key  :: Unique
where 'ns' is a Char representing the name space.  This in turn makes it
easy to build an OccEnv.
-}

instance Uniquable OccName where
      -- See Note [The Unique of an OccName]
  getUnique (OccName VarName   fs) = mkVarOccUnique  fs
  getUnique (OccName DataName  fs) = mkDataOccUnique fs
  getUnique (OccName TvName    fs) = mkTvOccUnique   fs
  getUnique (OccName TcClsName fs) = mkTcOccUnique   fs

{-
************************************************************************
*                                                                      *
\subsection{Predicates and taking them apart}
*                                                                      *
************************************************************************
-}

occNameString :: OccName -> String
occNameString (OccName _ s) = unpackFS s

setOccNameSpace :: NameSpace -> OccName -> OccName
setOccNameSpace sp (OccName _ occ) = OccName sp occ

isTvOcc, isTcOcc, isDataOcc :: OccName -> Bool

isTvOcc (OccName TvName _) = True
isTvOcc _                  = False

isTcOcc (OccName TcClsName _) = True
isTcOcc _                     = False

isDataOcc (OccName DataName _) = True
isDataOcc _                    = False

-- | Test if the 'OccName' is that for any operator (whether
-- it is a data constructor or variable or whatever)
isSymOcc :: OccName -> Bool
isSymOcc (OccName DataName s)  = isLexConSym s
isSymOcc (OccName TcClsName s) = isLexSym s
isSymOcc (OccName VarName s)   = isLexSym s
isSymOcc (OccName TvName s)    = isLexSym s


{-
************************************************************************
*                                                                      *
\subsection{Making system names}
*                                                                      *
************************************************************************

Here's our convention for splitting up the interface file name space:

   d...         dictionary identifiers
                (local variables, so no name-clash worries)

All of these other OccNames contain a mixture of alphabetic
and symbolic characters, and hence cannot possibly clash with
a user-written type or function name

   $f...        Dict-fun identifiers (from inst decls)
   $dmop        Default method for 'op'
   $pnC         n'th superclass selector for class C
   $wf          Worker for function 'f'
   $sf..        Specialised version of f
   D:C          Data constructor for dictionary for class C
   NTCo:T       Coercion connecting newtype T with its representation type
   TFCo:R       Coercion connecting a data family to its representation type R

In encoded form these appear as Zdfxxx etc

        :...            keywords (export:, letrec: etc.)
--- I THINK THIS IS WRONG!

This knowledge is encoded in the following functions.

@mk_deriv@ generates an @OccName@ from the prefix and a string.
NB: The string must already be encoded!
-}

mk_deriv :: NameSpace
         -> String              -- Distinguishes one sort of derived name from another
         -> String
         -> OccName

mk_deriv occ_sp sys_prefix str = mkOccName occ_sp (sys_prefix ++ str)

--- Overloaded record field selectors
mkRecFldSelOcc :: String -> OccName
mkRecFldSelOcc   = mk_deriv varName "$sel"
