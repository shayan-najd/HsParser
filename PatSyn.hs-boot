module PatSyn where

import U.BasicTypes (Arity)
import {-# SOURCE #-} TyCoRep (Type)
import Var (TyVar)
import Name (Name)

data PatSyn

patSynArity :: PatSyn -> Arity
patSynInstArgTys :: PatSyn -> [Type] -> [Type]
patSynExTyVars :: PatSyn -> [TyVar]
patSynName :: PatSyn -> Name
