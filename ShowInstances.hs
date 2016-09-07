{-# LANGUAGE StandaloneDeriving,
             FlexibleInstances #-}

module ShowInstances where

import HsLit
import HsExpr
import HsPat
import HsTypes
import HsDecls
import SrcLoc
import HsBinds
import BasicTypes
import BooleanFormula
import ForeignCall
import RdrName

deriving instance Show RdrName
deriving instance Show Origin
deriving instance Show TransForm
deriving instance Show HsIPName
deriving instance Show Fixity
deriving instance Show InlinePragma
deriving instance Show FunctionFixity
deriving instance Show HsTyLit
deriving instance Show HsTupleSort
deriving instance Show HsSrcBang
deriving instance Show HsLit
deriving instance Show Boxity
deriving instance Show StringLiteral
deriving instance Show HsArrAppType
deriving instance Show OverLitVal
deriving instance Show FixityDirection
deriving instance Show Activation
deriving instance Show SrcUnpackedness
deriving instance Show SrcStrictness
deriving instance Show WarningTxt
deriving instance Show DocDecl
deriving instance Show SpliceExplicitFlag
deriving instance Show OverlapMode
deriving instance Show ForeignImport
deriving instance Show ForeignExport
deriving instance Show Header
deriving instance Show CType
deriving instance Show CCallConv
deriving instance Show CExportSpec
deriving instance Show NewOrData
deriving instance Show CImportSpec
deriving instance Show CCallTarget


deriving instance Show a => Show (FamilyInfo a)
deriving instance Show a => Show (HsDecl a)
deriving instance Show a => Show (AnnProvenance a)
deriving instance Show a => Show (RuleBndr a)
deriving instance Show a => Show (FamilyResultSig a)
deriving instance Show a => Show (InjectivityAnn a)

deriving instance Show a => Show (ConDecl a)
deriving instance Show a => Show (ClsInstDecl a)
deriving instance Show a => Show (DataFamInstDecl a)
deriving instance Show a => Show (TyFamInstDecl a)
deriving instance Show a => Show (FamilyDecl a)
deriving instance Show a => Show (HsDataDefn a)
deriving instance Show a => Show (InstDecl a)

deriving instance Show a => Show (LHsQTyVars a)
deriving instance Show a => Show (RuleDecl a)
deriving instance Show a => Show (RoleAnnotDecl a)
deriving instance Show a => Show (TyClDecl a)
deriving instance Show a => Show (AnnDecl a)
deriving instance Show a => Show (DefaultDecl a)
deriving instance Show a => Show (DerivDecl a)
deriving instance Show a => Show (ForeignDecl a)
deriving instance Show a => Show (RuleDecls a)
deriving instance Show a => Show (SpliceDecl a)
deriving instance Show a => Show (TyClGroup a)
deriving instance Show a => Show (ArithSeqInfo a)
deriving instance Show a => Show (HsBracket a)
deriving instance Show a => Show (HsGroup a)
deriving instance Show a => Show (HsCmd a)
deriving instance Show a => Show (VectDecl a)
deriving instance Show a => Show (WarnDecl a)
deriving instance Show a => Show (WarnDecls a)
deriving instance Show a => Show (HsCmdTop a)
deriving instance Show a => Show (HsTupArg a)
deriving instance Show a => Show (AmbiguousFieldOcc a)
deriving instance Show a => Show (HsOverLit a)
deriving instance Show a => Show (HsExpr a)
deriving instance Show a => Show (Pat a)
deriving instance Show a => Show (HsType a)
deriving instance Show a => Show (Located a)
deriving instance Show a => Show (HsMatchContext a)
deriving instance Show a => Show (HsPatSynDir a)
deriving instance Show a => Show (HsPatSynDetails a)
deriving instance Show a => Show (Sig a)
deriving instance Show a => Show (HsIPBinds a)
deriving instance Show a => Show (IPBind a)
deriving instance Show a => Show (BooleanFormula a)
deriving instance Show a => Show (FixitySig a)
deriving instance Show a => Show (RecordPatSynField a)
deriving instance Show a => Show (HsStmtContext a)
deriving instance Show a => Show (HsSplice a)
deriving instance Show a => Show (HsWildCardInfo a)
deriving instance Show a => Show (HsTyVarBndr a)
deriving instance Show a => Show (HsAppType a)
deriving instance Show a => Show (ConDeclField a)
deriving instance Show a => Show (FieldOcc a)

deriving instance (Show a, Show b) => Show (TyFamEqn a b)
deriving instance (Show a, Show b) => Show (HsRecField' a b)
deriving instance (Show a, Show b) => Show (HsRecFields a b)
deriving instance (Show a, Show b) => Show (HsConDetails a b)
deriving instance (Show a, Show b) => Show (HsImplicitBndrs a b)
deriving instance (Show a, Show b) => Show (HsWildCardBndrs a b)
deriving instance (Show a, Show b, Show c) => Show (StmtLR a b c)
deriving instance (Show a, Show b) => Show (HsLocalBindsLR a b)
deriving instance (Show a, Show b) => Show (HsValBindsLR a b)
deriving instance (Show a, Show b) => Show (ParStmtBlock a b)
deriving instance (Show a, Show b) => Show (HsBindLR a b)
deriving instance (Show a, Show b) => Show (MatchGroup a b)
deriving instance (Show a, Show b) => Show (Match a b)
deriving instance (Show a, Show b) => Show (GRHSs a b)
deriving instance (Show a, Show b) => Show (GRHS a b)
deriving instance (Show a, Show b) => Show (PatSynBind a b)



-- HsMatchContext
