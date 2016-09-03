{-
(c) The University of Glasgow, 1992-2006


Here we collect a variety of helper functions that construct or
analyse HsSyn.  All these functions deal with generic HsSyn; functions
which deal with the instantiated versions are located elsewhere:

   Parameterised by     Module
   ----------------     -------------
   RdrName              parser/RdrHsSyn
   Name                 rename/RnHsSyn
   Id                   typecheck/TcHsSyn
-}

{-# OPTIONS_GHC -fwarn-unused-imports #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module HsUtils(mkChunkified,chunkify,mkMatchGroup,mkLHsSigWcType,mkNPlusKPat,
               mkUntypedSplice,mkClassOpSigs,mkHsOpApp,mkHsIntegral,
               mkHsIsString,mkHsDo,mkLHsSigType,mkHsFractional,
               mkBindStmt, mkBodyStmt,mkPatSynBind,unguardedRHS,
               mkRecStmt,mkHsComp,mkGroupByUsingStmt,
               mkGroupUsingStmt,mkTransformByStmt,missingTupArg,
               mkTransformStmt,mkHsSpliceNE,mkHsSpliceTE,
               mkHsSpliceE,mkHsSpliceTy,mkHsQuasiQuote,
               unguardedGRHSs,mkHsIf) where {-
  -- Terms
  mkHsPar, mkHsApp, mkHsAppType, -- mkHsConApp,
  mkHsCaseAlt,
  mkSimpleMatch, unguardedGRHSs, unguardedRHS,
  mkMatchGroup, mkMatchGroupName, mkMatch, mkHsLam, mkHsIf,
  -- mkHsWrap, mkLHsWrap, mkHsWrapCo, mkHsWrapCoR, mkLHsWrapCo,
  --  mkHsDictLet, mkHsLams,
  mkHsOpApp, mkHsDo, mkHsComp,
  mkLHsPar, -- mkHsCmdWrap, mkLHsCmdWrap,

  -- nlHsTyApp,
  -- nlHsTyApps,
  nlHsVar, nlHsLit, nlHsApp, nlHsApps, -- nlHsSyntaxApps,
  nlHsIntLit, nlHsVarApps,
  nlHsDo, nlHsOpApp, nlHsLam, nlHsPar, nlHsIf, nlHsCase, nlList,
  mkLHsTupleExpr, mkLHsVarTuple, missingTupArg,
  toLHsSigWcType,

  -- * Constructing general big tuples
  -- $big_tuples
  mkChunkified, chunkify,

  -- Bindings
  mkFunBind, mkVarBind, mkHsVarBind, mk_easy_FunBind, mkTopFunBind,
  mkPatSynBind,
  isInfixFunBind,

  -- Literals
  mkHsIntegral, mkHsFractional, mkHsIsString, mkHsString, mkHsStringPrimLit,

  -- Patterns
  mkNPat, mkNPlusKPat, nlVarPat, nlLitPat, nlConVarPat, nlConVarPatName, nlConPat,
  nlConPatName, nlInfixConPat, nlNullaryConPat, nlWildConPat, nlWildPat,
  nlWildPatName, nlWildPatId, nlTuplePat, mkParPat,
  mkBigLHsVarTup, mkBigLHsTup, mkBigLHsVarPatTup, mkBigLHsPatTup,

  -- Types
  mkHsAppTy, mkHsAppTys, userHsTyVarBndrs, userHsLTyVarBndrs,
  mkLHsSigType, mkLHsSigWcType, mkClassOpSigs,
  nlHsAppTy, nlHsTyVar, nlHsFunTy, nlHsTyConApp,

  -- Stmts
  mkTransformStmt, mkTransformByStmt, mkBodyStmt, mkBindStmt, mkTcBindStmt,
  mkLastStmt,
  emptyTransStmt, mkGroupUsingStmt, mkGroupByUsingStmt,
  emptyRecStmt, emptyRecStmtName, emptyRecStmtId, mkRecStmt,

  -- Template Haskell
  mkHsSpliceTy, mkHsSpliceE, mkHsSpliceTE, mkUntypedSplice, mkHsSpliceNE,
  mkHsQuasiQuote, unqualQuasiQuote,

  -- Flags
  noRebindableInfo,

  -- Collecting binders
  collectLocalBinders, collectHsValBinders, collectHsBindListBinders,
  collectHsIdBinders,
  collectHsBindsBinders, collectHsBindBinders, collectMethodBinders,
  collectPatBinders, collectPatsBinders,
  collectLStmtsBinders, collectStmtsBinders,
  collectLStmtBinders, collectStmtBinders,

  hsLTyClDeclBinders, hsTyClForeignBinders, hsPatSynSelectors,
  hsForeignDeclsBinders, hsGroupBinders, hsDataFamInstBinders,
  hsDataDefnBinders,

  -- Collecting implicit binders
  lStmtsImplicits, hsValBindsImplicits, lPatImplicits
  ) where
-}
#include "HsVersions.h"

import HsBinds
import HsExpr
import HsPat
import HsTypes
import HsLit

import RdrName
import Name
import BasicTypes
import SrcLoc
import FastString
import Outputable

mAX_TUPLE_SIZE = 62

{-
************************************************************************
*                                                                      *
        Some useful helpers for constructing syntax
*                                                                      *
************************************************************************

These functions attempt to construct a not-completely-useless SrcSpan
from their components, compared with the nl* functions below which
just attach noSrcSpan to everything.
-}


unguardedGRHSs :: Located (body id) -> GRHSs id (Located (body id))
unguardedGRHSs rhs@(L loc _)
  = GRHSs (unguardedRHS loc rhs) (noLoc emptyLocalBinds)

unguardedRHS :: SrcSpan -> Located (body id) -> [LGRHS id (Located (body id))]
unguardedRHS loc rhs = [L loc (GRHS [] rhs)]

mkMatchGroup :: Origin -> [LMatch RdrName (Located (body RdrName))]
             -> MatchGroup RdrName (Located (body RdrName))
mkMatchGroup origin matches = MG { mg_alts = mkLocatedList matches
                                 , mg_origin = origin }

mkLocatedList ::  [Located a] -> Located [Located a]
mkLocatedList [] = noLoc []
mkLocatedList ms = L (combineLocs (head ms) (last ms)) ms


-------------------------------
-- These are the bits of syntax that contain rebindable names
-- See RnEnv.lookupSyntaxName

mkHsIntegral   :: String -> Integer -> HsOverLit RdrName
mkHsFractional :: FractionalLit -> HsOverLit RdrName
mkHsIsString   :: String -> FastString -> HsOverLit RdrName
mkHsDo         :: HsStmtContext RdrName -> [ExprLStmt RdrName] -> HsExpr RdrName
mkHsComp       :: HsStmtContext RdrName -> [ExprLStmt RdrName] -> LHsExpr RdrName
               -> HsExpr RdrName

mkNPlusKPat :: Located RdrName -> Located (HsOverLit RdrName) -> Pat RdrName

mkLastStmt :: Located (bodyR idR) -> StmtLR idL idR (Located (bodyR idR))
mkBodyStmt :: Located (bodyR RdrName)
           -> StmtLR idL RdrName (Located (bodyR RdrName))
mkBindStmt :: LPat idL -> Located (bodyR idR)
           -> StmtLR idL idR (Located (bodyR idR))

emptyRecStmt     :: StmtLR idL  RdrName bodyR
mkRecStmt    :: [LStmtLR idL RdrName bodyR] -> StmtLR idL RdrName bodyR


mkHsIntegral src i  = OverLit (HsIntegral   src i) noExpr
mkHsFractional   f  = OverLit (HsFractional     f) noExpr
mkHsIsString src s  = OverLit (HsIsString   src s) noExpr

mkHsDo ctxt stmts = HsDo ctxt (mkLocatedList stmts)
mkHsComp ctxt stmts expr = mkHsDo ctxt (stmts ++ [last_stmt])
  where
    last_stmt = L (getLoc expr) $ mkLastStmt expr

mkHsIf :: LHsExpr id -> LHsExpr id -> LHsExpr id -> HsExpr id
mkHsIf c a b = HsIf c a b

mkNPlusKPat id lit = NPlusKPat id lit

mkTransformStmt    ::
                    [ExprLStmt idL] -> LHsExpr idR
                   -> StmtLR idL idR (LHsExpr idL)
mkTransformByStmt  ::
                    [ExprLStmt idL] -> LHsExpr idR -> LHsExpr idR
                   -> StmtLR idL idR (LHsExpr idL)
mkGroupUsingStmt   ::
                    [ExprLStmt idL]                -> LHsExpr idR
                   -> StmtLR idL idR (LHsExpr idL)
mkGroupByUsingStmt ::
                    [ExprLStmt idL] -> LHsExpr idR -> LHsExpr idR
                   -> StmtLR idL idR (LHsExpr idL)

emptyTransStmt :: StmtLR idL idR (LHsExpr idR)
emptyTransStmt = TransStmt { trS_form = panic "emptyTransStmt: form"
                           , trS_stmts = [], trS_bndrs = []
                           , trS_by = Nothing, trS_using = noLoc noExpr
                           }
mkTransformStmt    ss u   = emptyTransStmt { trS_form = ThenForm,  trS_stmts = ss, trS_using = u }
mkTransformByStmt  ss u b = emptyTransStmt { trS_form = ThenForm,  trS_stmts = ss, trS_using = u, trS_by = Just b }
mkGroupUsingStmt   ss u   = emptyTransStmt { trS_form = GroupForm, trS_stmts = ss, trS_using = u }
mkGroupByUsingStmt ss b u = emptyTransStmt { trS_form = GroupForm, trS_stmts = ss, trS_using = u, trS_by = Just b }

mkLastStmt body     = LastStmt body False
mkBodyStmt body     = BodyStmt body
mkBindStmt pat body = BindStmt pat body

emptyRecStmt' :: forall idL idR body. StmtLR idL idR body
emptyRecStmt' =
   RecStmt
     { recS_stmts = [], recS_later_ids = []
     , recS_rec_ids = [] }

emptyRecStmt     = emptyRecStmt'
mkRecStmt stmts  = emptyRecStmt { recS_stmts = stmts }

-------------------------------
--- A useful function for building @OpApps@.  The operator is always a
-- variable, and we don't know the fixity yet.
mkHsOpApp :: LHsExpr id -> id -> LHsExpr id -> HsExpr id
mkHsOpApp e1 op e2 = OpApp e1 (noLoc (HsVar (noLoc op))) e2

unqualSplice :: RdrName
unqualSplice = mkRdrUnqual (mkVarOccFS (fsLit "splice"))

mkUntypedSplice :: LHsExpr RdrName -> HsSplice RdrName
mkUntypedSplice e = HsUntypedSplice unqualSplice e

mkHsSpliceE :: LHsExpr RdrName -> HsExpr RdrName
mkHsSpliceE e = HsSpliceE (mkUntypedSplice e)

mkHsSpliceTE :: LHsExpr RdrName -> HsExpr RdrName
mkHsSpliceTE e = HsSpliceE (HsTypedSplice unqualSplice e)

mkHsSpliceNE :: LHsExpr RdrName -> HsExpr RdrName
mkHsSpliceNE e = HsSpliceE (HsNativSplice unqualSplice e)

mkHsSpliceTy :: LHsExpr RdrName -> HsType RdrName
mkHsSpliceTy e = HsSpliceTy (HsUntypedSplice unqualSplice e)

mkHsQuasiQuote :: RdrName -> SrcSpan -> FastString -> HsSplice RdrName
mkHsQuasiQuote quoter span quote = HsQuasiQuote unqualSplice quoter span quote




{-
Tuples.  All these functions are *pre-typechecker* because they lack
types on the tuple.
-}

missingTupArg :: HsTupArg RdrName
missingTupArg = Missing


-- $big_tuples
-- #big_tuples#
--
-- GHCs built in tuples can only go up to 'mAX_TUPLE_SIZE' in arity, but
-- we might concievably want to build such a massive tuple as part of the
-- output of a desugaring stage (notably that for list comprehensions).
--
-- We call tuples above this size \"big tuples\", and emulate them by
-- creating and pattern matching on >nested< tuples that are expressible
-- by GHC.
--
-- Nesting policy: it's better to have a 2-tuple of 10-tuples (3 objects)
-- than a 10-tuple of 2-tuples (11 objects), so we want the leaves of any
-- construction to be big.
--
-- If you just use the 'mkBigCoreTup', 'mkBigCoreVarTupTy', 'mkTupleSelector'
-- and 'mkTupleCase' functions to do all your work with tuples you should be
-- fine, and not have to worry about the arity limitation at all.

-- | Lifts a \"small\" constructor into a \"big\" constructor by recursive decompositon
mkChunkified :: ([a] -> a)      -- ^ \"Small\" constructor function, of maximum input arity 'mAX_TUPLE_SIZE'
             -> [a]             -- ^ Possible \"big\" list of things to construct from
             -> a               -- ^ Constructed thing made possible by recursive decomposition
mkChunkified small_tuple as = mk_big_tuple (chunkify as)
  where
        -- Each sub-list is short enough to fit in a tuple
    mk_big_tuple [as] = small_tuple as
    mk_big_tuple as_s = mk_big_tuple (chunkify (map small_tuple as_s))

chunkify :: [a] -> [[a]]
-- ^ Split a list into lists that are small enough to have a corresponding
-- tuple arity. The sub-lists of the result all have length <= 'mAX_TUPLE_SIZE'
-- But there may be more than 'mAX_TUPLE_SIZE' sub-lists
chunkify xs
  | n_xs <= mAX_TUPLE_SIZE = [xs]
  | otherwise              = split xs
  where
    n_xs     = length xs
    split [] = []
    split xs = take mAX_TUPLE_SIZE xs : split (drop mAX_TUPLE_SIZE xs)

{-
************************************************************************
*                                                                      *
        LHsSigType and LHsSigWcType
*                                                                      *
********************************************************************* -}

mkLHsSigType :: LHsType RdrName -> LHsSigType RdrName
mkLHsSigType ty = mkHsImplicitBndrs ty

mkLHsSigWcType :: LHsType RdrName -> LHsSigWcType RdrName
mkLHsSigWcType ty = mkHsImplicitBndrs (mkHsWildCardBndrs ty)

mkClassOpSigs :: [LSig RdrName] -> [LSig RdrName]
-- Convert TypeSig to ClassOpSig
-- The former is what is parsed, but the latter is
-- what we need in class/instance declarations
mkClassOpSigs sigs
  = map fiddle sigs
  where
    fiddle (L loc (TypeSig nms ty)) = L loc (ClassOpSig False nms (dropWildCards ty))
    fiddle sig                      = sig

mkPatSynBind :: Located RdrName -> HsPatSynDetails (Located RdrName)
             -> LPat RdrName -> HsPatSynDir RdrName -> HsBind RdrName
mkPatSynBind name details lpat dir = PatSynBind psb
  where
    psb = PSB{ psb_id = name
             , psb_args = details
             , psb_def = lpat
             , psb_dir = dir
             }
