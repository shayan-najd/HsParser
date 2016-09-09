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
module Language.Haskell.Syntax.HsUtils ( mkChunkified
                , mkAnonWildCardTy
                , mkHsWildCardBndrs
                , chunkify
                , mkMatchGroup
                , mkLHsSigWcType
                , mkNPlusKPat
                , mkClassOpSigs
                , mkHsOpApp
                , mkHsIntegral
                , mkHsIsString
                , mkHsDo
                , mkLHsSigType
                , mkHsFractional
                , mkBindStmt
                , mkBodyStmt
                , mkPatSynBind
                , unguardedRHS
                , mkRecStmt
                , mkHsComp
                , mkGroupByUsingStmt
                , mkGroupUsingStmt
                , mkTransformByStmt
                , missingTupArg
                , mkTransformStmt
                , unguardedGRHSs
                , mkHsIf
                , mkFalse
                , mkTrue) where


import Language.Haskell.Syntax.HsBinds
import Language.Haskell.Syntax.HsExpr
import Language.Haskell.Syntax.HsPat
import Language.Haskell.Syntax.HsTypes
import Language.Haskell.Syntax.HsLit
import Language.Haskell.Syntax.BooleanFormula

import Language.Haskell.Syntax.BasicTypes
import Language.Haskell.Syntax.SrcLoc
import Language.Haskell.Utility.FastString
-- import U.Panic (panic)

mAX_TUPLE_SIZE = 62

panic = error -- SHAYAN HACK!
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
  = GRHSs (unguardedRHS loc rhs) (noLoc EmptyLocalBinds)

unguardedRHS :: SrcSpan -> Located (body id) -> [LGRHS id (Located (body id))]
unguardedRHS loc rhs = [L loc (GRHS [] rhs)]

mkMatchGroup :: Origin -> [LMatch id (Located (body id))]
             -> MatchGroup id (Located (body id))
mkMatchGroup origin matches = MG { mg_alts = mkLocatedList matches
                                 , mg_origin = origin }

mkLocatedList ::  [Located a] -> Located [Located a]
mkLocatedList [] = noLoc []
mkLocatedList ms = L (combineLocs (head ms) (last ms)) ms


-------------------------------
-- These are the bits of syntax that contain rebindable names
-- See RnEnv.lookupSyntaxName

mkHsIntegral   :: String -> Integer -> HsOverLit id
mkHsFractional :: FractionalLit -> HsOverLit id
mkHsIsString   :: String -> FastString -> HsOverLit id
mkHsDo         :: HsStmtContext id -> [ExprLStmt id] -> HsExpr id
mkHsComp       :: HsStmtContext id -> [ExprLStmt id] -> LHsExpr id
               -> HsExpr id

mkNPlusKPat :: Located id -> Located (HsOverLit id) -> Pat id

mkLastStmt :: Located (bodyR idR) -> StmtLR idL idR (Located (bodyR idR))
mkBodyStmt :: Located (bodyR id)
           -> StmtLR idL id (Located (bodyR id))
mkBindStmt :: LPat idL -> Located (bodyR idR)
           -> StmtLR idL idR (Located (bodyR idR))

emptyRecStmt     :: StmtLR idL  id bodyR
mkRecStmt    :: [LStmtLR idL id bodyR] -> StmtLR idL id bodyR


mkHsIntegral src i  = OverLit (HsIntegral   src i)
mkHsFractional   f  = OverLit (HsFractional     f)
mkHsIsString src s  = OverLit (HsIsString   src s)

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
                           , trS_stmts = []
                           , trS_bndrs = []
                           , trS_by = Nothing
                           , trS_using = panic "no Using"
                           }
mkTransformStmt    ss u   = emptyTransStmt { trS_form = ThenForm,  trS_stmts = ss, trS_using = u }
mkTransformByStmt  ss u b = emptyTransStmt { trS_form = ThenForm,  trS_stmts = ss, trS_using = u, trS_by = Just b }
mkGroupUsingStmt   ss u   = emptyTransStmt { trS_form = GroupForm, trS_stmts = ss, trS_using = u }
mkGroupByUsingStmt ss b u = emptyTransStmt { trS_form = GroupForm, trS_stmts = ss, trS_using = u, trS_by = Just b }

mkLastStmt body     = LastStmt body False
mkBodyStmt body     = BodyStmt body
mkBindStmt pat body = BindStmt pat body

emptyRecStmt' :: StmtLR idL idR body
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

{-
Tuples.  All these functions are *pre-typechecker* because they lack
types on the tuple.
-}

missingTupArg :: HsTupArg id
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

mkLHsSigType :: LHsType id -> LHsSigType id
mkLHsSigType ty = HsIB ty

mkLHsSigWcType :: LHsType id -> LHsSigWcType id
mkLHsSigWcType ty = HsIB (mkHsWildCardBndrs ty)

mkClassOpSigs :: [LSig id] -> [LSig id]
-- Convert TypeSig to ClassOpSig
-- The former is what is parsed, but the latter is
-- what we need in class/instance declarations
mkClassOpSigs sigs
  = map fiddle sigs
  where
    fiddle (L loc (TypeSig nms ty)) = L loc (ClassOpSig False nms (dropWildCards ty))
    fiddle sig                      = sig

mkPatSynBind :: Located id -> HsPatSynDetails (Located id)
             -> LPat id -> HsPatSynDir id -> HsBind id
mkPatSynBind name details lpat dir = PatSynBind psb
  where
    psb = PSB{ psb_id = name
             , psb_args = details
             , psb_def = lpat
             , psb_dir = dir
             }

mkAnonWildCardTy :: HsType id
mkAnonWildCardTy = HsWildCardTy AnonWildCard

mkHsWildCardBndrs :: thing -> HsWildCardBndrs id thing
mkHsWildCardBndrs x = HsWC { hswc_body = x
                           , hswc_ctx  = Nothing }

dropWildCards :: LHsSigWcType name -> LHsSigType name
-- Drop the wildcard part of a LHsSogWcType
dropWildCards sig_ty = sig_ty { hsib_body = hsSogWcType sig_ty }

hsSogWcType :: LHsSigWcType name -> LHsType name
hsSogWcType = hswc_body . hsib_body

mkFalse, mkTrue :: BooleanFormula a
mkFalse = Or []
mkTrue = And []
