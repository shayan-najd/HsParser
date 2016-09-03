{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1998


This module contains definitions for the IdInfo for things that
have a standard form, namely:

- data constructors
- record selectors
- method and superclass selectors
- primitive operations
-}

{-# LANGUAGE CPP #-}

module MkId (DataConBoxer,mkDataConWorkId,mkDictSelId,magicDictId) where {-
        mkDictFunId, mkDictFunTy, mkDictSelId, mkDictSelRhs,
        mkFCallId,

        wrapNewTypeBody, unwrapNewTypeBody,
        wrapFamInstBody, unwrapFamInstScrut,
        wrapTypeUnbranchedFamInstBody, unwrapTypeUnbranchedFamInstScrut,

        DataConBoxer(..), mkDataConRep, mkDataConWorkId,

        -- And some particular Ids; see below for why they are wired in
        wiredInIds, ghcPrimIds,
        unsafeCoerceName, unsafeCoerceId, realWorldPrimId,
        voidPrimId, voidArgId,
        nullAddrId, seqId, lazyId, lazyIdKey, runRWId,
        coercionTokenId, magicDictId,
        proxyHashId,

        -- Re-export error Ids
    ) where -}

#include "HsVersions.h"

import Rules
import TysPrim
import TysWiredIn
-- import PrelRules
import Type
import Coercion
-- import MkCore
import CoreUtils        (mkCast )
import CoreUnfold
import TyCon
import Class
import Name
import DataCon
import Id
import IdInfo
import Demand
import CoreSyn
import PrelNames
import U.BasicTypes       hiding ( SuccessFlag(..) )
import U.Util
import U.Outputable
import U.FastString
import ListSetOps

mkDictSelId :: Name          -- Name of one of the *value* selectors
                             -- (dictionary superclass or method)
            -> Class -> Id
mkDictSelId name clas
  = mkGlobalId (ClassOpId clas) name sel_ty info
  where
    tycon          = classTyCon clas
    sel_names      = map idName (classAllSelIds clas)
    new_tycon      = isNewTyCon tycon
    [data_con]     = tyConDataCons tycon
    binders        = dataConUnivTyBinders data_con
    tyvars         = dataConUnivTyVars data_con
    arg_tys        = dataConRepArgTys data_con  -- Includes the dictionary superclasses
    val_index      = assoc "MkId.mkDictSelId" (sel_names `zip` [0..]) name

    sel_ty = mkForAllTys binders $
             mkFunTy (mkClassPred clas (mkTyVarTys tyvars)) $
             getNth arg_tys val_index

    base_info = noCafIdInfo
                `setArityInfo`         1
                `setStrictnessInfo`    strict_sig

    info | new_tycon
         = base_info `setInlinePragInfo` alwaysInlinePragma
                     `setUnfoldingInfo`  mkInlineUnfolding (Just 1) (mkDictSelRhs clas val_index)
                   -- See Note [Single-method classes] in TcInstDcls
                   -- for why alwaysInlinePragma

         | otherwise
         = base_info `setRuleInfo` mkRuleInfo [rule]
                   -- Add a magic BuiltinRule, but no unfolding
                   -- so that the rule is always available to fire.
                   -- See Note [ClassOp/DFun selection] in TcInstDcls

    n_ty_args = length tyvars

    -- This is the built-in rule that goes
    --      op (dfT d1 d2) --->  opT d1 d2
    rule = BuiltinRule { ru_name = fsLit "Class op " `appendFS`
                                     occNameFS (getOccName name)
                       , ru_fn    = name
                       , ru_nargs = n_ty_args + 1
                       , ru_try   = dictSelRule val_index n_ty_args }

        -- The strictness signature is of the form U(AAAVAAAA) -> T
        -- where the V depends on which item we are selecting
        -- It's worth giving one, so that absence info etc is generated
        -- even if the selector isn't inlined

    strict_sig = mkClosedStrictSig [arg_dmd] topRes
    arg_dmd | new_tycon = evalDmd
            | otherwise = mkManyUsedDmd $
                          mkProdDmd [ if name == sel_name then evalDmd else absDmd
                                    | sel_name <- sel_names ]

mkDictSelRhs :: Class
             -> Int         -- 0-indexed selector among (superclasses ++ methods)
             -> CoreExpr
mkDictSelRhs clas val_index
  = mkLams tyvars (Lam dict_id rhs_body)
  where
    tycon          = classTyCon clas
    new_tycon      = isNewTyCon tycon
    [data_con]     = tyConDataCons tycon
    tyvars         = dataConUnivTyVars data_con
    arg_tys        = dataConRepArgTys data_con  -- Includes the dictionary superclasses

    the_arg_id     = getNth arg_ids val_index
    pred           = mkClassPred clas (mkTyVarTys tyvars)
    dict_id        = mkTemplateLocal 1 pred
    arg_ids        = mkTemplateLocalsNum 2 arg_tys

    rhs_body | new_tycon = unwrapNewTypeBody tycon (mkTyVarTys tyvars) (Var dict_id)
             | otherwise = Case (Var dict_id) dict_id (idType the_arg_id)
                                [(DataAlt data_con, arg_ids, varToCoreExpr the_arg_id)]
                                -- varToCoreExpr needed for equality superclass selectors
                                --   sel a b d = case x of { MkC _ (g:a~b) _ -> CO g }

dictSelRule :: Int -> Arity -> RuleFun
-- Tries to persuade the argument to look like a constructor
-- application, using exprIsConApp_maybe, and then selects
-- from it
--       sel_i t1..tk (D t1..tk op1 ... opm) = opi
--
dictSelRule val_index n_ty_args _ id_unf _ args
  | (dict_arg : _) <- drop n_ty_args args
  , Just (_, _, con_args) <- exprIsConApp_maybe id_unf dict_arg
  = Just (getNth con_args val_index)
  | otherwise
  = Nothing


mkDataConWorkId :: Name -> DataCon -> Id
mkDataConWorkId wkr_name data_con
  | isNewTyCon tycon
  = mkGlobalId (DataConWrapId data_con) wkr_name nt_wrap_ty nt_work_info
  | otherwise
  = mkGlobalId (DataConWorkId data_con) wkr_name alg_wkr_ty wkr_info

  where
    tycon = dataConTyCon data_con

        ----------- Workers for data types --------------
    alg_wkr_ty = dataConRepType data_con
    wkr_arity = dataConRepArity data_con
    wkr_info  = noCafIdInfo
                `setArityInfo`       wkr_arity
                `setStrictnessInfo`  wkr_sig
                `setUnfoldingInfo`   evaldUnfolding  -- Record that it's evaluated,
                                                     -- even if arity = 0

    wkr_sig = mkClosedStrictSig (replicate wkr_arity topDmd) (dataConCPR data_con)
        --      Note [Data-con worker strictness]
        -- Notice that we do *not* say the worker is strict
        -- even if the data constructor is declared strict
        --      e.g.    data T = MkT !(Int,Int)
        -- Why?  Because the *wrapper* is strict (and its unfolding has case
        -- expressions that do the evals) but the *worker* itself is not.
        -- If we pretend it is strict then when we see
        --      case x of y -> $wMkT y
        -- the simplifier thinks that y is "sure to be evaluated" (because
        --  $wMkT is strict) and drops the case.  No, $wMkT is not strict.
        --
        -- When the simplifer sees a pattern
        --      case e of MkT x -> ...
        -- it uses the dataConRepStrictness of MkT to mark x as evaluated;
        -- but that's fine... dataConRepStrictness comes from the data con
        -- not from the worker Id.

        ----------- Workers for newtypes --------------
    (nt_tvs, _, nt_arg_tys, _) = dataConSig data_con
    res_ty_args  = mkTyVarTys nt_tvs
    nt_wrap_ty   = dataConUserType data_con
    nt_work_info = noCafIdInfo          -- The NoCaf-ness is set by noCafIdInfo
                  `setArityInfo` 1      -- Arity 1
                  `setInlinePragInfo`    alwaysInlinePragma
                  `setUnfoldingInfo`     newtype_unf
    id_arg1      = mkTemplateLocal 1 (head nt_arg_tys)
    newtype_unf  = ASSERT2( isVanillaDataCon data_con &&
                            isSingleton nt_arg_tys, ppr data_con  )
                              -- Note [Newtype datacons]
                   mkCompulsoryUnfolding $
                   mkLams nt_tvs $ Lam id_arg1 $
                   wrapNewTypeBody tycon res_ty_args (Var id_arg1)

dataConCPR :: DataCon -> DmdResult
dataConCPR con
  | isDataTyCon tycon     -- Real data types only; that is,
                          -- not unboxed tuples or newtypes
  , null (dataConExTyVars con)  -- No existentials
  , wkr_arity > 0
  , wkr_arity <= mAX_CPR_SIZE
  = if is_prod then vanillaCprProdRes (dataConRepArity con)
               else cprSumRes (dataConTag con)
  | otherwise
  = topRes
  where
    is_prod   = isProductTyCon tycon
    tycon     = dataConTyCon con
    wkr_arity = dataConRepArity con

    mAX_CPR_SIZE :: Arity
    mAX_CPR_SIZE = 10
    -- We do not treat very big tuples as CPR-ish:
    --      a) for a start we get into trouble because there aren't
    --         "enough" unboxed tuple types (a tiresome restriction,
    --         but hard to fix),
    --      b) more importantly, big unboxed tuples get returned mainly
    --         on the stack, and are often then allocated in the heap
    --         by the caller.  So doing CPR for them may in fact make
    --         things worse.


data DataConBoxer  -- SHAYAN TODO

wrapNewTypeBody :: TyCon -> [Type] -> CoreExpr -> CoreExpr
-- The wrapper for the data constructor for a newtype looks like this:
--      newtype T a = MkT (a,Int)
--      MkT :: forall a. (a,Int) -> T a
--      MkT = /\a. \(x:(a,Int)). x `cast` sym (CoT a)
-- where CoT is the coercion TyCon associated with the newtype
--
-- The call (wrapNewTypeBody T [a] e) returns the
-- body of the wrapper, namely
--      e `cast` (CoT [a])
--
-- If a coercion constructor is provided in the newtype, then we use
-- it, otherwise the wrap/unwrap are both no-ops
--
-- If the we are dealing with a newtype *instance*, we have a second coercion
-- identifying the family instance with the constructor of the newtype
-- instance.  This coercion is applied in any case (ie, composed with the
-- coercion constructor of the newtype or applied by itself).

wrapNewTypeBody tycon args result_expr
  = ASSERT( isNewTyCon tycon )
    wrapFamInstBody tycon args $
    mkCast result_expr (mkSymCo co)
  where
    co = mkUnbranchedAxInstCo Representational (newTyConCo tycon) args []

-- When unwrapping, we do *not* apply any family coercion, because this will
-- be done via a CoPat by the type checker.  We have to do it this way as
-- computing the right type arguments for the coercion requires more than just
-- a spliting operation (cf, TcPat.tcConPat).

unwrapNewTypeBody :: TyCon -> [Type] -> CoreExpr -> CoreExpr
unwrapNewTypeBody tycon args result_expr
  = ASSERT( isNewTyCon tycon )
    mkCast result_expr (mkUnbranchedAxInstCo Representational (newTyConCo tycon) args [])

-- If the type constructor is a representation type of a data instance, wrap
-- the expression into a cast adjusting the expression type, which is an
-- instance of the representation type, to the corresponding instance of the
-- family instance type.
-- See Note [Wrappers for data instance tycons]
wrapFamInstBody :: TyCon -> [Type] -> CoreExpr -> CoreExpr
wrapFamInstBody tycon args body
  | Just co_con <- tyConFamilyCoercion_maybe tycon
  = mkCast body (mkSymCo (mkUnbranchedAxInstCo Representational co_con args []))
  | otherwise
  = body


magicDictName :: Name
magicDictName     = mkWiredInIdName gHC_PRIM  (fsLit "magicDict")      magicDictKey       magicDictId

magicDictId :: Id  -- See Note [magicDictId magic]
magicDictId = pcMiscPrelId magicDictName ty info
  where
  info = noCafIdInfo `setInlinePragInfo` neverInlinePragma
  ty   = mkSpecForAllTys [alphaTyVar] alphaTy

pcMiscPrelId :: Name -> Type -> IdInfo -> Id
pcMiscPrelId name ty info
  = mkVanillaGlobalWithInfo name ty info
    -- We lie and say the thing is imported; otherwise, we get into
    -- a mess with dependency analysis; e.g., core2stg may heave in
    -- random calls to GHCbase.unpackPS__.  If GHCbase is the module
    -- being compiled, then it's just a matter of luck if the definition
    -- will be in "the right place" to be in scope.
