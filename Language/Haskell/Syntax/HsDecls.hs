{-# OPTIONS_GHC -fwarn-unused-imports #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, DeriveFoldable,
             DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Abstract syntax of global declarations.
--
-- Definitions for: @SynDecl@ and @ConDecl@, @ClassDecl@,
-- @InstDecl@, @DefaultDecl@ and @ForeignDecl@.
module Language.Haskell.Syntax.HsDecls ( LHsDecl
               , LConDecl
               , LTyFamInstEqn
               , LTyClDecl
               , LInstDecl
               , LTyFamInstDecl
               , LRoleAnnotDecl
               , LFamilyDecl
               , LDataFamInstDecl
               , LDocDecl
               , LInjectivityAnn
               , LWarnDecl
               , LRuleBndr
               , LRuleDecl
               , LDerivDecl
               , LFamilyResultSig
               , Role(..)
               , FunDep(..)
               , SpliceDecl(..)
               , FamilyResultSig(..)
               , LTyFamDefltEqn(..)
               , HsTyPats(..)
               , TyClDecl(..)
               , TyClGroup(..)
               , FamilyDecl(..)
               , FamilyInfo(..)
               , HsDataDefn(..)
               , NewOrData(..)
               , DataFamInstDecl(..)
               , TyFamInstEqn(..)
               , TyFamInstDecl(..)
               , RoleAnnotDecl(..)
               , RuleDecl(..)
               , ForeignDecl(..)
               , DefaultDecl(..)
               , DerivDecl(..)
               , ClsInstDecl(..)
               , InstDecl(..)
               , AnnProvenance(..)
               , ForeignExport(..)
               , ForeignImport(..)
               , RuleDecls(..)
               , RuleBndr(..)
               , WarnDecls(..)
               , WarnDecl(..)
               , HsDecl(..)
               , AnnDecl(..)
               , ConDecl(..)
               , VectDecl(..)
               , DocDecl(..)
               , HsGroup(..)
               , InjectivityAnn(..)
               , TyFamEqn(..)
               , CImportSpec(..)
               , HsDeriving(..)
               , HsConDeclDetails(..)
               , SpliceExplicitFlag(..)
               )where

-- friends:
import {-# SOURCE #-} Language.Haskell.Syntax.HsExpr( LHsExpr, HsExpr, HsSplice)
        -- Because Expr imports Decls via HsBracket

import Language.Haskell.Syntax.HsBinds
import Language.Haskell.Syntax.HsTypes
import Language.Haskell.Syntax.HsDoc
import Language.Haskell.Syntax.BasicTypes
import Language.Haskell.Syntax.ForeignCall

-- others:
import Language.Haskell.Syntax.SrcLoc

import Data.Data        hiding (TyCon,Fixity)

data Role = Nominal | Representational | Phantom
          deriving (Data,Show)

type FunDep a = ([a],[a])

{-
************************************************************************
*                                                                      *
\subsection[HsDecl]{Declarations}
*                                                                      *
************************************************************************
-}

type LHsDecl id = Located (HsDecl id)
        -- ^ When in a list this may have
        --
        --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnSemi'
        --

-- For details on above see note [Api annotations] in ApiAnnotation

-- | A Haskell Declaration
data HsDecl id
  = TyClD       (TyClDecl id)     -- ^ A type or class declaration.
  | InstD       (InstDecl  id)    -- ^ An instance declaration.
  | DerivD      (DerivDecl id)
  | ValD        (HsBind id)
  | SigD        (Sig id)
  | DefD        (DefaultDecl id)
  | ForD        (ForeignDecl id)
  | WarningD    (WarnDecls id)
  | AnnD        (AnnDecl id)
  | RuleD       (RuleDecls id)
  | VectD       (VectDecl id)
  | SpliceD     (SpliceDecl id)   -- Includes quasi-quotes
  | DocD        (DocDecl)
  | RoleAnnotD  (RoleAnnotDecl id)
deriving instance (Data id) => Data (HsDecl id)


-- NB: all top-level fixity decls are contained EITHER
-- EITHER SigDs
-- OR     in the ClassDecls in TyClDs
--
-- The former covers
--      a) data constructors
--      b) class methods (but they can be also done in the
--              signatures of class decls)
--      c) imported functions (that have an IfacSig)
--      d) top level decls
--
-- The latter is for class methods only

-- | A 'HsDecl' is categorised into a 'HsGroup' before being
-- fed to the renamer.
data HsGroup id
  = HsGroup {
        hs_valds  :: HsValBinds id,
        hs_splcds :: [LSpliceDecl id],

        hs_tyclds :: [TyClGroup id],
                -- A list of mutually-recursive groups;
                -- This includes `InstDecl`s as well;
                -- Parser generates a singleton list;
                -- renamer does dependency analysis

        hs_derivds :: [LDerivDecl id],

        hs_fixds  :: [LFixitySig id],
                -- Snaffled out of both top-level fixity signatures,
                -- and those in class declarations

        hs_defds  :: [LDefaultDecl id],
        hs_fords  :: [LForeignDecl id],
        hs_warnds :: [LWarnDecls id],
        hs_annds  :: [LAnnDecl id],
        hs_ruleds :: [LRuleDecls id],
        hs_vects  :: [LVectDecl id],

        hs_docs   :: [LDocDecl]
  }
deriving instance (Data id) => Data (HsGroup id)


data SpliceExplicitFlag = ExplicitSplice | -- <=> $(f x y)
                          ImplicitSplice   -- <=> f x y,  i.e. a naked top level expression
    deriving Data

type LSpliceDecl name = Located (SpliceDecl name)
data SpliceDecl id
  = SpliceDecl                  -- Top level splice
        (Located (HsSplice id))
        SpliceExplicitFlag
deriving instance (Data id) => Data (SpliceDecl id)

{-
************************************************************************
*                                                                      *
            Type and class declarations
*                                                                      *
************************************************************************

Note [The Naming story]
~~~~~~~~~~~~~~~~~~~~~~~
Here is the story about the implicit names that go with type, class,
and instance decls.  It's a bit tricky, so pay attention!

"Implicit" (or "system") binders
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Each data type decl defines
        a worker name for each constructor
        to-T and from-T convertors
  Each class decl defines
        a tycon for the class
        a data constructor for that tycon
        the worker for that constructor
        a selector for each superclass

All have occurrence names that are derived uniquely from their parent
declaration.

None of these get separate definitions in an interface file; they are
fully defined by the data or class decl.  But they may *occur* in
interface files, of course.  Any such occurrence must haul in the
relevant type or class decl.

Plan of attack:
 - Ensure they "point to" the parent data/class decl
   when loading that decl from an interface file
   (See RnHiFiles.getSysBinders)

 - When typechecking the decl, we build the implicit TyCons and Ids.
   When doing so we look them up in the name cache (RnEnv.lookupSysName),
   to ensure correct module and provenance is set

These are the two places that we have to conjure up the magic derived
names.  (The actual magic is in OccName.mkWorkerOcc, etc.)

Default methods
~~~~~~~~~~~~~~~
 - Occurrence name is derived uniquely from the method name
   E.g. $dmmax

 - If there is a default method name at all, it's recorded in
   the ClassOpSig (in HsBinds), in the DefMethInfo field.
   (DefMethInfo is defined in Class.hs)

Source-code class decls and interface-code class decls are treated subtly
differently, which has given me a great deal of confusion over the years.
Here's the deal.  (We distinguish the two cases because source-code decls
have (Just binds) in the tcdMeths field, whereas interface decls have Nothing.

In *source-code* class declarations:

 - When parsing, every ClassOpSig gets a DefMeth with a suitable RdrName
   This is done by RdrHsSyn.mkClassOpSigDM

 - The renamer renames it to a Name

 - During typechecking, we generate a binding for each $dm for
   which there's a programmer-supplied default method:
        class Foo a where
          op1 :: <type>
          op2 :: <type>
          op1 = ...
   We generate a binding for $dmop1 but not for $dmop2.
   The Class for Foo has a Nothing for op2 and
                         a Just ($dm_op1, VanillaDM) for op1.
   The Name for $dmop2 is simply discarded.

In *interface-file* class declarations:
  - When parsing, we see if there's an explicit programmer-supplied default method
    because there's an '=' sign to indicate it:
        class Foo a where
          op1 = :: <type>       -- NB the '='
          op2   :: <type>
    We use this info to generate a DefMeth with a suitable RdrName for op1,
    and a NoDefMeth for op2
  - The interface file has a separate definition for $dmop1, with unfolding etc.
  - The renamer renames it to a Name.
  - The renamer treats $dmop1 as a free variable of the declaration, so that
    the binding for $dmop1 will be sucked in.  (See RnHsSyn.tyClDeclFVs)
    This doesn't happen for source code class decls, because they *bind* the default method.

Dictionary functions
~~~~~~~~~~~~~~~~~~~~
Each instance declaration gives rise to one dictionary function binding.

The type checker makes up new source-code instance declarations
(e.g. from 'deriving' or generic default methods --- see
TcInstDcls.tcInstDecls1).  So we can't generate the names for
dictionary functions in advance (we don't know how many we need).

On the other hand for interface-file instance declarations, the decl
specifies the name of the dictionary function, and it has a binding elsewhere
in the interface file:
        instance {Eq Int} = dEqInt
        dEqInt :: {Eq Int} <pragma info>

So again we treat source code and interface file code slightly differently.

Source code:
  - Source code instance decls have a Nothing in the (Maybe name) field
    (see data InstDecl below)

  - The typechecker makes up a Local name for the dict fun for any source-code
    instance decl, whether it comes from a source-code instance decl, or whether
    the instance decl is derived from some other construct (e.g. 'deriving').

  - The occurrence name it chooses is derived from the instance decl (just for
    documentation really) --- e.g. dNumInt.  Two dict funs may share a common
    occurrence name, but will have different uniques.  E.g.
        instance Foo [Int]  where ...
        instance Foo [Bool] where ...
    These might both be dFooList

  - The CoreTidy phase externalises the name, and ensures the occurrence name is
    unique (this isn't special to dict funs).  So we'd get dFooList and dFooList1.

  - We can take this relaxed approach (changing the occurrence name later)
    because dict fun Ids are not captured in a TyCon or Class (unlike default
    methods, say).  Instead, they are kept separately in the InstEnv.  This
    makes it easy to adjust them after compiling a module.  (Once we've finished
    compiling that module, they don't change any more.)


Interface file code:
  - The instance decl gives the dict fun name, so the InstDecl has a (Just name)
    in the (Maybe name) field.

  - RnHsSyn.instDeclFVs treats the dict fun name as free in the decl, so that we
    suck in the dfun binding
-}

type LTyClDecl name = Located (TyClDecl name)

-- | A type or class declaration.
data TyClDecl name
  = -- | @type/data family T :: *->*@
    --
    --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnType',
    --             'ApiAnnotation.AnnData',
    --             'ApiAnnotation.AnnFamily','ApiAnnotation.AnnDcolon',
    --             'ApiAnnotation.AnnWhere','ApiAnnotation.AnnOpenP',
    --             'ApiAnnotation.AnnDcolon','ApiAnnotation.AnnCloseP',
    --             'ApiAnnotation.AnnEqual','ApiAnnotation.AnnRarrow',
    --             'ApiAnnotation.AnnVbar'

    -- For details on above see note [Api annotations] in ApiAnnotation
    FamDecl { tcdFam :: FamilyDecl name }

  | -- | @type@ declaration
    --
    --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnType',
    --             'ApiAnnotation.AnnEqual',

    -- For details on above see note [Api annotations] in ApiAnnotation
    SynDecl { tcdLName  :: Located name           -- ^ Type constructor
            , tcdTyVars :: LHsQTyVars name        -- ^ Type variables; for an associated type
                                                  --   these include outer binders
            , tcdRhs    :: LHsType name   }

  | -- | @data@ declaration
    --
    --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnData',
    --              'ApiAnnotation.AnnFamily',
    --              'ApiAnnotation.AnnNewType',
    --              'ApiAnnotation.AnnNewType','ApiAnnotation.AnnDcolon'
    --              'ApiAnnotation.AnnWhere',

    -- For details on above see note [Api annotations] in ApiAnnotation
    DataDecl { tcdLName    :: Located name        -- ^ Type constructor
             , tcdTyVars   :: LHsQTyVars name  -- ^ Type variables; for an associated type
                                                  --   these include outer binders
                                                  -- Eg  class T a where
                                                  --       type F a :: *
                                                  --       type F a = a -> a
                                                  -- Here the type decl for 'f' includes 'a'
                                                  -- in its tcdTyVars
             , tcdDataDefn :: HsDataDefn name
             }

  | ClassDecl { tcdCtxt    :: LHsContext name,          -- ^ Context...
                tcdLName   :: Located name,             -- ^ Name of the class
                tcdTyVars  :: LHsQTyVars name,          -- ^ Class type variables
                tcdFDs     :: [Located (FunDep (Located name))],
                                                        -- ^ Functional deps
                tcdSigs    :: [LSig name],              -- ^ Methods' signatures
                tcdMeths   :: LHsBinds name,            -- ^ Default methods
                tcdATs     :: [LFamilyDecl name],       -- ^ Associated types;
                tcdATDefs  :: [LTyFamDefltEqn name],    -- ^ Associated type defaults
                tcdDocs    :: [LDocDecl]               -- ^ Haddock docs

    }
        -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnClass',
        --           'ApiAnnotation.AnnWhere','ApiAnnotation.AnnOpen',
        --           'ApiAnnotation.AnnClose'
        --   - The tcdFDs will have 'ApiAnnotation.AnnVbar',
        --                          'ApiAnnotation.AnnComma'
        --                          'ApiAnnotation.AnnRarrow'

        -- For details on above see note [Api annotations] in ApiAnnotation

deriving instance (Data id) => Data (TyClDecl id)

{- Note [Complete user-supplied kind signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We kind-check declarations differently if they have a complete, user-supplied
kind signature (CUSK). This is because we can safely generalise a CUSKed
declaration before checking all of the others, supporting polymorphic recursion.
See ghc.haskell.org/trac/ghc/wiki/GhcKinds/KindInference#Proposednewstrategy
and #9200 for lots of discussion of how we got here.

A declaration has a CUSK if we can know its complete kind without doing any
inference, at all. Here are the rules:

 - A class or datatype is said to have a CUSK if and only if all of its type
variables are annotated. Its result kind is, by construction, Constraint or *
respectively.

 - A type synonym has a CUSK if and only if all of its type variables and its
RHS are annotated with kinds.

 - A closed type family is said to have a CUSK if and only if all of its type
variables and its return type are annotated.

 - An open type family always has a CUSK -- unannotated type variables (and
return type) default to *.

 - Additionally, if -XTypeInType is on, then a data definition with a top-level
   :: must explicitly bind all kind variables to the right of the ::.
   See test dependent/should_compile/KindLevels, which requires this case.
   (Naturally, any kind variable mentioned before the :: should not be bound
   after it.)
-}


{- *********************************************************************
*                                                                      *
                         TyClGroup
        Strongly connected components of
      type, class, instance, and role declarations
*                                                                      *
********************************************************************* -}

{- Note [TyClGroups and dependency analysis]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A TyClGroup represents a strongly connected components of type/class/instance
decls, together with the role annotations for the type/class declarations.

The hs_tyclds :: [TyClGroup] field of a HsGroup is a dependency-order
sequence of strongly-connected components.

Invariants
 * The type and class declarations, group_tyclds, may depend on each
   other, or earlier TyClGroups, but not on later ones

 * The role annotations, group_roles, are role-annotations for some or
   all of the types and classes in group_tyclds (only).

 * The instance declarations, group_instds, may (and usually will)
   depend on group_tyclds, or on earlier TyClGroups, but not on later
   ones.

See Note [Dependency analsis of type, class, and instance decls]
in RnSource for more info.
-}

data TyClGroup name  -- See Note [TyClGroups and dependency analysis]
  = TyClGroup { group_tyclds :: [LTyClDecl name]
              , group_roles  :: [LRoleAnnotDecl name]
              , group_instds :: [LInstDecl name] }
deriving instance (Data id) => Data (TyClGroup id)


{- *********************************************************************
*                                                                      *
               Data and type family declarations
*                                                                      *
********************************************************************* -}

{- Note [FamilyResultSig]
~~~~~~~~~~~~~~~~~~~~~~~~~

This data type represents the return signature of a type family.  Possible
values are:

 * NoSig - the user supplied no return signature:
      type family Id a where ...

 * KindSig - the user supplied the return kind:
      type family Id a :: * where ...

 * TyVarSig - user named the result with a type variable and possibly
   provided a kind signature for that variable:
      type family Id a = r where ...
      type family Id a = (r :: *) where ...

   Naming result of a type family is required if we want to provide
   injectivity annotation for a type family:
      type family Id a = r | r -> a where ...

See also: Note [Injectivity annotation]

Note [Injectivity annotation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A user can declare a type family to be injective:

   type family Id a = r | r -> a where ...

 * The part after the "|" is called "injectivity annotation".
 * "r -> a" part is called "injectivity condition"; at the moment terms
   "injectivity annotation" and "injectivity condition" are synonymous
   because we only allow a single injectivity condition.
 * "r" is the "LHS of injectivity condition". LHS can only contain the
   variable naming the result of a type family.

 * "a" is the "RHS of injectivity condition". RHS contains space-separated
   type and kind variables representing the arguments of a type
   family. Variables can be omitted if a type family is not injective in
   these arguments. Example:
         type family Foo a b c = d | d -> a c where ...

Note that:
 (a) naming of type family result is required to provide injectivity
     annotation
 (b) for associated types if the result was named then injectivity annotation
     is mandatory. Otherwise result type variable is indistinguishable from
     associated type default.

It is possible that in the future this syntax will be extended to support
more complicated injectivity annotations. For example we could declare that
if we know the result of Plus and one of its arguments we can determine the
other argument:

   type family Plus a b = (r :: Nat) | r a -> b, r b -> a where ...

Here injectivity annotation would consist of two comma-separated injectivity
conditions.

See also Note [Injective type families] in TyCon
-}

type LFamilyResultSig name = Located (FamilyResultSig name)
data FamilyResultSig name = -- see Note [FamilyResultSig]
    NoSig
  -- ^ - 'ApiAnnotation.AnnKeywordId' :

  -- For details on above see note [Api annotations] in ApiAnnotation

  | KindSig  (LHsKind name)
  -- ^ - 'ApiAnnotation.AnnKeywordId' :
  --             'ApiAnnotation.AnnOpenP','ApiAnnotation.AnnDcolon',
  --             'ApiAnnotation.AnnCloseP'

  -- For details on above see note [Api annotations] in ApiAnnotation

  | TyVarSig (LHsTyVarBndr name)
  -- ^ - 'ApiAnnotation.AnnKeywordId' :
  --             'ApiAnnotation.AnnOpenP','ApiAnnotation.AnnDcolon',
  --             'ApiAnnotation.AnnCloseP', 'ApiAnnotation.AnnEqual'

  -- For details on above see note [Api annotations] in ApiAnnotation

deriving instance (Data name) => Data (FamilyResultSig name)

type LFamilyDecl name = Located (FamilyDecl name)
data FamilyDecl name = FamilyDecl
  { fdInfo           :: FamilyInfo name              -- type/data, closed/open
  , fdLName          :: Located name                 -- type constructor
  , fdTyVars         :: LHsQTyVars name              -- type variables
  , fdResultSig      :: LFamilyResultSig name        -- result signature
  , fdInjectivityAnn :: Maybe (LInjectivityAnn name) -- optional injectivity ann
  }
  -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnType',
  --             'ApiAnnotation.AnnData', 'ApiAnnotation.AnnFamily',
  --             'ApiAnnotation.AnnWhere', 'ApiAnnotation.AnnOpenP',
  --             'ApiAnnotation.AnnDcolon', 'ApiAnnotation.AnnCloseP',
  --             'ApiAnnotation.AnnEqual', 'ApiAnnotation.AnnRarrow',
  --             'ApiAnnotation.AnnVbar'

  -- For details on above see note [Api annotations] in ApiAnnotation

deriving instance (Data id) => Data (FamilyDecl id)

type LInjectivityAnn name = Located (InjectivityAnn name)

-- | If the user supplied an injectivity annotation it is represented using
-- InjectivityAnn. At the moment this is a single injectivity condition - see
-- Note [Injectivity annotation]. `Located name` stores the LHS of injectivity
-- condition. `[Located name]` stores the RHS of injectivity condition. Example:
--
--   type family Foo a b c = r | r -> a c where ...
--
-- This will be represented as "InjectivityAnn `r` [`a`, `c`]"
data InjectivityAnn name
  = InjectivityAnn (Located name) [Located name]
  -- ^ - 'ApiAnnotation.AnnKeywordId' :
  --             'ApiAnnotation.AnnRarrow', 'ApiAnnotation.AnnVbar'

  -- For details on above see note [Api annotations] in ApiAnnotation
  deriving Data

data FamilyInfo name
  = DataFamily
  | OpenTypeFamily
     -- | 'Nothing' if we're in an hs-boot file and the user
     -- said "type family Foo x where .."
  | ClosedTypeFamily (Maybe [LTyFamInstEqn name])
deriving instance (Data name) => Data (FamilyInfo name)


{- *********************************************************************
*                                                                      *
               Data types and data constructors
*                                                                      *
********************************************************************* -}

data HsDataDefn name   -- The payload of a data type defn
                       -- Used *both* for vanilla data declarations,
                       --       *and* for data family instances
  = -- | Declares a data type or newtype, giving its constructors
    -- @
    --  data/newtype T a = <constrs>
    --  data/newtype instance T [a] = <constrs>
    -- @
    HsDataDefn { dd_ND     :: NewOrData,
                 dd_ctxt   :: LHsContext name,           -- ^ Context
                 dd_cType  :: Maybe (Located CType),
                 dd_kindSig:: Maybe (LHsKind name),
                     -- ^ Optional kind signature.
                     --
                     -- @(Just k)@ for a GADT-style @data@,
                     -- or @data instance@ decl, with explicit kind sig
                     --
                     -- Always @Nothing@ for H98-syntax decls

                 dd_cons   :: [LConDecl name],
                     -- ^ Data constructors
                     --
                     -- For @data T a = T1 | T2 a@
                     --   the 'LConDecl's all have 'ConDeclH98'.
                     -- For @data T a where { T1 :: T a }@
                     --   the 'LConDecls' all have 'ConDeclGADT'.

                 dd_derivs :: HsDeriving name  -- ^ Optional 'deriving' claues

             -- For details on above see note [Api annotations] in ApiAnnotation
   }
deriving instance (Data id) => Data (HsDataDefn id)

type HsDeriving name = Maybe (Located [LHsSigType name])
  -- ^ The optional 'deriving' clause of a data declaration
  --
  --   @Nothing@ => not specified,
  --   @Just []@ => derive exactly what is asked
  --
  -- It's a 'LHsSigType' because, with Generalised Newtype
  -- Deriving, we can mention type variables that aren't
  -- bound by the date type.   e.g.
  --     data T b = ... deriving( C [a] )
  -- should producd a derived instance for (C [a] (T b))
  --
  -- The payload of the Maybe is Located so that we have a
  -- place to hang the API annotations:
  --  - 'ApiAnnotation.AnnKeywordId' :
  --       'ApiAnnotation.AnnDeriving',
  --       'ApiAnnotation.AnnOpen','ApiAnnotation.AnnClose'

data NewOrData
  = NewType                     -- ^ @newtype Blah ...@
  | DataType                    -- ^ @data Blah ...@
  deriving( Eq, Data )                -- Needed because Demand derives Eq

type LConDecl name = Located (ConDecl name)
      -- ^ May have 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnSemi' when
      --   in a GADT constructor list

  -- For details on above see note [Api annotations] in ApiAnnotation

-- |
--
-- @
-- data T b = forall a. Eq a => MkT a b
--   MkT :: forall b a. Eq a => MkT a b
--
-- data T b where
--      MkT1 :: Int -> T Int
--
-- data T = Int `MkT` Int
--        | MkT2
--
-- data T a where
--      Int `MkT` Int :: T Int
-- @
--
-- - 'ApiAnnotation.AnnKeywordId's : 'ApiAnnotation.AnnOpen',
--            'ApiAnnotation.AnnDotdot','ApiAnnotation.AnnCLose',
--            'ApiAnnotation.AnnEqual','ApiAnnotation.AnnVbar',
--            'ApiAnnotation.AnnDarrow','ApiAnnotation.AnnDarrow',
--            'ApiAnnotation.AnnForall','ApiAnnotation.AnnDot'

-- For details on above see note [Api annotations] in ApiAnnotation
data ConDecl name
  = ConDeclGADT
      { con_names   :: [Located name]
      , con_type    :: LHsSigType name
        -- ^ The type after the ‘::’
      , con_doc     :: Maybe LHsDocString
          -- ^ A possible Haddock comment.
      }

  | ConDeclH98
      { con_name    :: Located name

      , con_qvars     :: Maybe (LHsQTyVars name)
        -- User-written forall (if any), and its implicit
        -- kind variables
        -- Non-Nothing needs -XExistentialQuantification
        --               e.g. data T a = forall b. MkT b (b->a)
        --               con_qvars = {b}

      , con_cxt       :: Maybe (LHsContext name)
        -- ^ User-written context (if any)

      , con_details   :: HsConDeclDetails name
          -- ^ Arguments

      , con_doc       :: Maybe LHsDocString
          -- ^ A possible Haddock comment.
      }
deriving instance (Data name) => Data (ConDecl name)

type HsConDeclDetails name
   = HsConDetails (LBangType name) (Located [LConDeclField name])


{-
************************************************************************
*                                                                      *
                Instance declarations
*                                                                      *
************************************************************************

Note [Type family instance declarations in HsSyn]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The data type TyFamEqn represents one equation of a type family instance.
It is parameterised over its tfe_pats field:

 * An ordinary type family instance declaration looks like this in source Haskell
      type instance T [a] Int = a -> a
   (or something similar for a closed family)
   It is represented by a TyFamInstEqn, with *type* in the tfe_pats field.

 * On the other hand, the *default instance* of an associated type looks like
   this in source Haskell
      class C a where
        type T a b
        type T a b = a -> b   -- The default instance
   It is represented by a TyFamDefltEqn, with *type variables* in the tfe_pats
   field.
-}

----------------- Type synonym family instances -------------
type LTyFamInstEqn  name = Located (TyFamInstEqn  name)
  -- ^ May have 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnSemi'
  --   when in a list

-- For details on above see note [Api annotations] in ApiAnnotation

type LTyFamDefltEqn name = Located (TyFamDefltEqn name)

type HsTyPats name = HsImplicitBndrs name [LHsType name]
            -- ^ Type patterns (with kind and type bndrs)
            -- See Note [Family instance declaration binders]

{- Note [Family instance declaration binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The HsTyPats field is LHS patterns or a type/data family instance.

The hsib_vars of the HsImplicitBndrs are the template variables of the
type patterns, i.e. fv(pat_tys).  Note in particular

* The hsib_vars *includes* any anonymous wildcards.  For example
     type instance F a _ = a
  The hsib_vars will be {a, _}.  Remember that each separate wildcard
  '_' gets its own unique.  In this context wildcards behave just like
  an ordinary type variable, only anonymous.

* The hsib_vars *including* type variables that are already in scope

   Eg   class C s t where
          type F t p :: *
        instance C w (a,b) where
          type F (a,b) x = x->a
   The hsib_vars of the F decl are {a,b,x}, even though the F decl
   is nested inside the 'instance' decl.

   However after the renamer, the uniques will match up:
        instance C w7 (a8,b9) where
          type F (a8,b9) x10 = x10->a8
   so that we can compare the type pattern in the 'instance' decl and
   in the associated 'type' decl
-}

type TyFamInstEqn  name = TyFamEqn name (HsTyPats name)
type TyFamDefltEqn name = TyFamEqn name (LHsQTyVars name)
  -- See Note [Type family instance declarations in HsSyn]

-- | One equation in a type family instance declaration
-- See Note [Type family instance declarations in HsSyn]
data TyFamEqn name pats
  = TyFamEqn
       { tfe_tycon :: Located name
       , tfe_pats  :: pats
       , tfe_rhs   :: LHsType name }
    -- ^
    --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnEqual'

    -- For details on above see note [Api annotations] in ApiAnnotation
deriving instance (Data name, Data pats) => Data (TyFamEqn name pats)

type LTyFamInstDecl name = Located (TyFamInstDecl name)
data TyFamInstDecl name
  = TyFamInstDecl
       { tfid_eqn  :: LTyFamInstEqn name
       }
    -- ^
    --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnType',
    --           'ApiAnnotation.AnnInstance',

    -- For details on above see note [Api annotations] in ApiAnnotation
deriving instance (Data name) => Data (TyFamInstDecl name)

----------------- Data family instances -------------

type LDataFamInstDecl name = Located (DataFamInstDecl name)
data DataFamInstDecl name
  = DataFamInstDecl
       { dfid_tycon     :: Located name
       , dfid_pats      :: HsTyPats   name       -- LHS
       , dfid_defn      :: HsDataDefn name       -- RHS
       } -- Free vars for dependency analysis
    -- ^
    --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnData',
    --           'ApiAnnotation.AnnNewType','ApiAnnotation.AnnInstance',
    --           'ApiAnnotation.AnnDcolon'
    --           'ApiAnnotation.AnnWhere','ApiAnnotation.AnnOpen',
    --           'ApiAnnotation.AnnClose'

    -- For details on above see note [Api annotations] in ApiAnnotation
deriving instance (Data name) => Data (DataFamInstDecl name)


----------------- Class instances -------------

data ClsInstDecl name
  = ClsInstDecl
      { cid_poly_ty :: LHsSigType name    -- Context => Class Instance-type
                                          -- Using a polytype means that the renamer conveniently
                                          -- figures out the quantified type variables for us.
      , cid_binds         :: LHsBinds name           -- Class methods
      , cid_sigs          :: [LSig name]             -- User-supplied pragmatic info
      , cid_tyfam_insts   :: [LTyFamInstDecl name]   -- Type family instances
      , cid_datafam_insts :: [LDataFamInstDecl name] -- Data family instances
      , cid_overlap_mode  :: Maybe (Located OverlapMode)
         -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
         --                                    'ApiAnnotation.AnnClose',

        -- For details on above see note [Api annotations] in ApiAnnotation
      }
    -- ^
    --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnInstance',
    --           'ApiAnnotation.AnnWhere',
    --           'ApiAnnotation.AnnOpen','ApiAnnotation.AnnClose',

    -- For details on above see note [Api annotations] in ApiAnnotation
deriving instance (Data id) => Data (ClsInstDecl id)


----------------- Instances of all kinds -------------

type LInstDecl name = Located (InstDecl name)
data InstDecl name  -- Both class and family instances
  = ClsInstD
      { cid_inst  :: ClsInstDecl name }
  | DataFamInstD              -- data family instance
      { dfid_inst :: DataFamInstDecl name }
  | TyFamInstD              -- type family instance
      { tfid_inst :: TyFamInstDecl name }
deriving instance (Data id) => Data (InstDecl id)

-- Extract the declarations of associated data types from an instance

{-
************************************************************************
*                                                                      *
\subsection[DerivDecl]{A stand-alone instance deriving declaration}
*                                                                      *
************************************************************************
-}

type LDerivDecl name = Located (DerivDecl name)

data DerivDecl name = DerivDecl
        { deriv_type         :: LHsSigType name
        , deriv_overlap_mode :: Maybe (Located OverlapMode)
         -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
         --                                    'ApiAnnotation.AnnClose',
         --                                    'ApiAnnotation.AnnDeriving',
         --                                    'ApiAnnotation.AnnInstance'

  -- For details on above see note [Api annotations] in ApiAnnotation
        }

deriving instance (Data name) => Data (DerivDecl name)

{-
************************************************************************
*                                                                      *
\subsection[DefaultDecl]{A @default@ declaration}
*                                                                      *
************************************************************************

There can only be one default declaration per module, but it is hard
for the parser to check that; we pass them all through in the abstract
syntax, and that restriction must be checked in the front end.
-}

type LDefaultDecl name = Located (DefaultDecl name)

data DefaultDecl name
  = DefaultDecl [LHsType name]
        -- ^ - 'ApiAnnotation.AnnKeywordId's : 'ApiAnnotation.AnnDefault',
        --          'ApiAnnotation.AnnOpen','ApiAnnotation.AnnClose'

        -- For details on above see note [Api annotations] in ApiAnnotation
deriving instance (Data name) => Data (DefaultDecl name)

{-
************************************************************************
*                                                                      *
\subsection{Foreign function interface declaration}
*                                                                      *
************************************************************************
-}

-- foreign declarations are distinguished as to whether they define or use a
-- Haskell name
--
--  * the Boolean value indicates whether the pre-standard deprecated syntax
--   has been used
--
type LForeignDecl name = Located (ForeignDecl name)

data ForeignDecl name
  = ForeignImport
      { fd_name   :: Located name          -- defines this name
      , fd_sig_ty :: LHsSigType name       -- sig_ty
      , fd_fi     :: ForeignImport }

  | ForeignExport
      { fd_name   :: Located name          -- uses this name
      , fd_sig_ty :: LHsSigType name       -- sig_ty
      , fd_fe     :: ForeignExport }
        -- ^
        --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnForeign',
        --           'ApiAnnotation.AnnImport','ApiAnnotation.AnnExport',
        --           'ApiAnnotation.AnnDcolon'

        -- For details on above see note [Api annotations] in ApiAnnotation

deriving instance (Data name) => Data (ForeignDecl name)
{-
    In both ForeignImport and ForeignExport:
        sig_ty is the type given in the Haskell code
        rep_ty is the representation for this type, i.e. with newtypes
               coerced away and type functions evaluated.
    Thus if the declaration is valid, then rep_ty will only use types
    such as Int and IO that we know how to make foreign calls with.
-}

-- Specification Of an imported external entity in dependence on the calling
-- convention
--
data ForeignImport = -- import of a C entity
                     --
                     --  * the two strings specifying a header file or library
                     --   may be empty, which indicates the absence of a
                     --   header or object specification (both are not used
                     --   in the case of `CWrapper' and when `CFunction'
                     --   has a dynamic target)
                     --
                     --  * the calling convention is irrelevant for code
                     --   generation in the case of `CLabel', but is needed
                     --   for pretty printing
                     --
                     --  * `Safety' is irrelevant for `CLabel' and `CWrapper'
                     --
                     CImport  (Located CCallConv) -- ccall or stdcall
                              (Located Safety)  -- interruptible, safe or unsafe
                              (Maybe Header)       -- name of C header
                              CImportSpec          -- details of the C entity
                              (Located SourceText) -- original source text for
                                                   -- the C entity
  deriving Data

-- details of an external C entity
--
data CImportSpec = CLabel    CLabelString     -- import address of a C label
                 | CFunction CCallTarget      -- static or dynamic function
                 | CWrapper                   -- wrapper to expose closures
                                              -- (former f.e.d.)
  deriving Data

-- specification of an externally exported entity in dependence on the calling
-- convention
--
data ForeignExport = CExport  (Located CExportSpec) -- contains the calling
                                                    -- convention
                              (Located SourceText)  -- original source text for
                                                    -- the C entity
  deriving Data

-- pretty printing of foreign declarations
--

{-
************************************************************************
*                                                                      *
\subsection{Transformation rules}
*                                                                      *
************************************************************************
-}

type LRuleDecls name = Located (RuleDecls name)

  -- Note [Pragma source text] in BasicTypes
data RuleDecls name = HsRules { rds_src   :: SourceText
                              , rds_rules :: [LRuleDecl name] }
deriving instance (Data name) => Data (RuleDecls name)

type LRuleDecl name = Located (RuleDecl name)

data RuleDecl name
  = HsRule                             -- Source rule
        (Located (SourceText,RuleName)) -- Rule name
               -- Note [Pragma source text] in BasicTypes
        Activation
        [LRuleBndr name]        -- Forall'd vars; after typechecking this
                                --   includes tyvars
        (Located (HsExpr name)) -- LHS
        (Located (HsExpr name)) -- RHS
        -- ^
        --  - 'ApiAnnotation.AnnKeywordId' :
        --           'ApiAnnotation.AnnOpen','ApiAnnotation.AnnTilde',
        --           'ApiAnnotation.AnnVal',
        --           'ApiAnnotation.AnnClose',
        --           'ApiAnnotation.AnnForall','ApiAnnotation.AnnDot',
        --           'ApiAnnotation.AnnEqual',

        -- For details on above see note [Api annotations] in ApiAnnotation
deriving instance (Data name) => Data (RuleDecl name)

type LRuleBndr name = Located (RuleBndr name)
data RuleBndr name
  = RuleBndr (Located name)
  | RuleBndrSig (Located name) (LHsSigWcType name)
        -- ^
        --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
        --     'ApiAnnotation.AnnDcolon','ApiAnnotation.AnnClose'

        -- For details on above see note [Api annotations] in ApiAnnotation
deriving instance (Data name) => Data (RuleBndr name)
{-
************************************************************************
*                                                                      *
\subsection{Vectorisation declarations}
*                                                                      *
************************************************************************

A vectorisation pragma, one of

  {-# VECTORISE f = closure1 g (scalar_map g) #-}
  {-# VECTORISE SCALAR f #-}
  {-# NOVECTORISE f #-}

  {-# VECTORISE type T = ty #-}
  {-# VECTORISE SCALAR type T #-}
-}

type LVectDecl name = Located (VectDecl name)

data VectDecl name
  = HsVect
      SourceText   -- Note [Pragma source text] in BasicTypes
      (Located name)
      (LHsExpr name)
        -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
        --           'ApiAnnotation.AnnEqual','ApiAnnotation.AnnClose'

        -- For details on above see note [Api annotations] in ApiAnnotation
  | HsNoVect
      SourceText   -- Note [Pragma source text] in BasicTypes
      (Located name)
        -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
        --                                    'ApiAnnotation.AnnClose'

        -- For details on above see note [Api annotations] in ApiAnnotation
  | HsVectTypeIn                -- pre type-checking
      SourceText                -- Note [Pragma source text] in BasicTypes
      Bool                      -- 'TRUE' => SCALAR declaration
      (Located name)
      (Maybe (Located name))    -- 'Nothing' => no right-hand side
        -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
        --           'ApiAnnotation.AnnType','ApiAnnotation.AnnClose',
        --           'ApiAnnotation.AnnEqual'

        -- For details on above see note [Api annotations] in ApiAnnotation
  | HsVectClassIn               -- pre type-checking
      SourceText                -- Note [Pragma source text] in BasicTypes
      (Located name)
        -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
        --           'ApiAnnotation.AnnClass','ApiAnnotation.AnnClose',

       -- For details on above see note [Api annotations] in ApiAnnotation
  | HsVectInstIn                -- pre type-checking (always SCALAR)  !!!FIXME: should be superfluous now
      (LHsSigType name)
deriving instance (Data name) => Data (VectDecl name)

{-
************************************************************************
*                                                                      *
\subsection[DocDecl]{Document comments}
*                                                                      *
************************************************************************
-}

type LDocDecl = Located (DocDecl)

data DocDecl
  = DocCommentNext HsDocString
  | DocCommentPrev HsDocString
  | DocCommentNamed String HsDocString
  | DocGroup Int HsDocString
  deriving Data

{-
************************************************************************
*                                                                      *
\subsection[DeprecDecl]{Deprecations}
*                                                                      *
************************************************************************

We use exported entities for things to deprecate.
-}


type LWarnDecls name = Located (WarnDecls name)

 -- Note [Pragma source text] in BasicTypes
data WarnDecls name = Warnings { wd_src :: SourceText
                               , wd_warnings :: [LWarnDecl name]
                               }
  deriving Data


type LWarnDecl name = Located (WarnDecl name)

data WarnDecl name = Warning [Located name] WarningTxt
  deriving Data

{-
************************************************************************
*                                                                      *
\subsection[AnnDecl]{Annotations}
*                                                                      *
************************************************************************
-}

type LAnnDecl name = Located (AnnDecl name)

data AnnDecl name = HsAnnotation
                      SourceText -- Note [Pragma source text] in BasicTypes
                      (AnnProvenance name) (Located (HsExpr name))
      -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
      --           'ApiAnnotation.AnnType'
      --           'ApiAnnotation.AnnModule'
      --           'ApiAnnotation.AnnClose'

      -- For details on above see note [Api annotations] in ApiAnnotation
deriving instance (Data name) => Data (AnnDecl name)

data AnnProvenance name = ValueAnnProvenance (Located name)
                        | TypeAnnProvenance (Located name)
                        | ModuleAnnProvenance
  deriving (Data, Functor)
deriving instance Foldable    AnnProvenance
deriving instance Traversable AnnProvenance

{-
************************************************************************
*                                                                      *
\subsection[RoleAnnot]{Role annotations}
*                                                                      *
************************************************************************
-}

type LRoleAnnotDecl name = Located (RoleAnnotDecl name)

-- See #8185 for more info about why role annotations are
-- top-level declarations
data RoleAnnotDecl name
  = RoleAnnotDecl (Located name)         -- type constructor
                  [Located (Maybe Role)] -- optional annotations
      -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnType',
      --           'ApiAnnotation.AnnRole'

      -- For details on above see note [Api annotations] in ApiAnnotation
  deriving Data
