{-# LANGUAGE CPP, KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RoleAnnotations #-}

module HsExpr where

import SrcLoc     ( Located )
import U.Outputable ( SDoc, Outputable, OutputableBndr )
import {-# SOURCE #-} HsPat  ( LPat )
import Data.Data hiding ( Fixity )

type role HsExpr nominal
type role HsCmd nominal
type role MatchGroup nominal representational
type role GRHSs nominal representational
type role HsSplice nominal
data HsExpr (i :: *)
data HsCmd  (i :: *)
data HsSplice (i :: *)
data MatchGroup (a :: *) (body :: *)
data GRHSs (a :: *) (body :: *)

instance (Data id) => Data (HsSplice id)
instance (Data id) => Data (HsExpr id)
instance (Data id) => Data (HsCmd id)
instance (Data body,Data id) => Data (MatchGroup id body)
instance (Data body,Data id) => Data (GRHSs id body)

instance (OutputableBndr id) => Outputable (HsExpr id)
instance (OutputableBndr id) => Outputable (HsCmd id)

type LHsExpr a = Located (HsExpr a)

pprLExpr :: (OutputableBndr id) => LHsExpr id -> SDoc

pprExpr :: (OutputableBndr id) => HsExpr id -> SDoc

pprSplice :: (OutputableBndr id) => HsSplice id -> SDoc

pprPatBind :: (OutputableBndr bndr,
               OutputableBndr id, Outputable body)
           => LPat bndr -> GRHSs id body -> SDoc

pprFunBind :: (OutputableBndr idR, Outputable body)
           => MatchGroup idR body -> SDoc
