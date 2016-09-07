{-# OPTIONS_GHC -fwarn-unused-imports #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

{-# LANGUAGE CPP, DeriveDataTypeable, ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}

-- | Abstract Haskell syntax for expressions.
module HsExpr {- (SrcLoc,
              LHsExpr,
              LGRHS,
              LMatch,
              HsExpr(..),
              GRHS(..),
              GRHSs(..),
              noExpr,
              HsTupArg(..),
              HsSplice(..),
              MatchGroup(..),
              HsCmd(..),
              SyntaxExpr(..),
              HsStmtContext (..),
              ExprLStmt,
              StmtLR(..),
              LStmtLR(..),
              TransForm(..),
              ArithSeqInfo(..),
              PendingRnSplice (..),

              tupArgPresent,
              noSyntaxExpr,
              pprLExpr,
              pprExpr,
              pprSplice,
              pprPatBind,
              pprFunBind) -} where

#include "HsVersions.h"

-- friends:
import HsDecls
import HsPat
import HsLit
import HsTypes
import HsBinds

import BasicTypes
import U.FastString
import SrcLoc
import U.Panic (panic)

-- libraries:
import Data.Data hiding (Fixity(..))

{-
************************************************************************
*                                                                      *
\subsection{Expressions proper}
*                                                                      *
************************************************************************
-}

-- * Expressions proper

type LHsExpr id = Located (HsExpr id)
  -- ^ May have 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnComma' when
  --   in a list

  -- For details on above see note [Api annotations] in ApiAnnotation

-- | This is used for rebindable-syntax pieces that are too polymorphic
-- for tcSyntaxOp (trS_fmap and the mzip in ParStmt)
noExpr :: HsExpr id
noExpr = HsLit (HsString "" (fsLit "noExpr"))

{-
Note [CmdSyntaxtable]
~~~~~~~~~~~~~~~~~~~~~
Used only for arrow-syntax stuff (HsCmdTop), the CmdSyntaxTable keeps
track of the methods needed for a Cmd.

* Before the renamer, this list is an empty list

* After the renamer, it takes the form @[(std_name, HsVar actual_name)]@
  For example, for the 'arr' method
   * normal case:            (GHC.Control.Arrow.arr, HsVar GHC.Control.Arrow.arr)
   * with rebindable syntax: (GHC.Control.Arrow.arr, arr_22)
             where @arr_22@ is whatever 'arr' is in scope

* After the type checker, it takes the form [(std_name, <expression>)]
  where <expression> is the evidence for the method.  This evidence is
  instantiated with the class, but is still polymorphic in everything
  else.  For example, in the case of 'arr', the evidence has type
         forall b c. (b->c) -> a b c
  where 'a' is the ambient type of the arrow.  This polymorphism is
  important because the desugarer uses the same evidence at multiple
  different types.

This is Less Cool than what we normally do for rebindable syntax, which is to
make fully-instantiated piece of evidence at every use site.  The Cmd way
is Less Cool because
  * The renamer has to predict which methods are needed.
    See the tedious RnExpr.methodNamesCmd.

  * The desugarer has to know the polymorphic type of the instantiated
    method. This is checked by Inst.tcSyntaxName, but is less flexible
    than the rest of rebindable syntax, where the type is less
    pre-ordained.  (And this flexibility is useful; for example we can
    typecheck do-notation with (>>=) :: m1 a -> (a -> m2 b) -> m2 b.)
-}

{-
Note [OutOfScope and GlobalRdrEnv]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To understand why we bundle a GlobalRdrEnv with an out-of-scope variable,
consider the following module:

    module A where

    foo :: ()
    foo = bar

    bat :: [Double]
    bat = [1.2, 3.4]

    $(return [])

    bar = ()
    bad = False

When A is compiled, the renamer determines that `bar` is not in scope in the
declaration of `foo` (since `bar` is declared in the following inter-splice
group).  Once it has finished typechecking the entire module, the typechecker
then generates the associated error message, which specifies both the type of
`bar` and a list of possible in-scope alternatives:

    A.hs:6:7: error:
        • Variable not in scope: bar :: ()
        • ‘bar’ (line 13) is not in scope before the splice on line 11
          Perhaps you meant ‘bat’ (line 9)

When it calls RnEnv.unknownNameSuggestions to identify these alternatives, the
typechecker must provide a GlobalRdrEnv.  If it provided the current one, which
contains top-level declarations for the entire module, the error message would
incorrectly suggest the out-of-scope `bar` and `bad` as possible alternatives
for `bar` (see Trac #11680).  Instead, the typechecker must use the same
GlobalRdrEnv the renamer used when it determined that `bar` is out-of-scope.

To obtain this GlobalRdrEnv, can the typechecker simply use the out-of-scope
`bar`'s location to either reconstruct it (from the current GlobalRdrEnv) or to
look it up in some global store?  Unfortunately, no.  The problem is that
location information is not always sufficient for this task.  This is most
apparent when dealing with the TH function addTopDecls, which adds its
declarations to the FOLLOWING inter-splice group.  Consider these declarations:

    ex9 = cat               -- cat is NOT in scope here

    $(do -------------------------------------------------------------
        ds <- [d| f = cab   -- cat and cap are both in scope here
                  cat = ()
                |]
        addTopDecls ds
        [d| g = cab         -- only cap is in scope here
            cap = True
          |])

    ex10 = cat              -- cat is NOT in scope here

    $(return []) -----------------------------------------------------

    ex11 = cat              -- cat is in scope

Here, both occurrences of `cab` are out-of-scope, and so the typechecker needs
the GlobalRdrEnvs which were used when they were renamed.  These GlobalRdrEnvs
are different (`cat` is present only in the GlobalRdrEnv for f's `cab'), but the
locations of the two `cab`s are the same (they are both created in the same
splice).  Thus, we must include some additional information with each `cab` to
allow the typechecker to obtain the correct GlobalRdrEnv.  Clearly, the simplest
information to use is the GlobalRdrEnv itself.
-}

-- | A Haskell expression.
data HsExpr id
  = HsVar     (Located id)   -- ^ Variable

                             -- See Note [Located RdrNames]

  | HsRecFld (AmbiguousFieldOcc id) -- ^ Variable pointing to record selector

  | HsOverLabel FastString   -- ^ Overloaded label (See Note [Overloaded labels]
                             --   in GHC.OverloadedLabels)
  | HsIPVar   HsIPName       -- ^ Implicit parameter
  | HsOverLit (HsOverLit id) -- ^ Overloaded literals

  | HsLit     HsLit          -- ^ Simple (non-overloaded) literals

  | HsLam     (MatchGroup id (LHsExpr id)) -- ^ Lambda abstraction. Currently always a single match
       --
       -- - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnLam',
       --       'ApiAnnotation.AnnRarrow',

       -- For details on above see note [Api annotations] in ApiAnnotation

  | HsLamCase (MatchGroup id (LHsExpr id)) -- ^ Lambda-case
       --
       -- - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnLam',
       --           'ApiAnnotation.AnnCase','ApiAnnotation.AnnOpen',
       --           'ApiAnnotation.AnnClose'

       -- For details on above see note [Api annotations] in ApiAnnotation

  | HsApp     (LHsExpr id) (LHsExpr id) -- ^ Application

  | HsAppType (LHsExpr id) (LHsWcType id) -- ^ Visible type application
       --
       -- Explicit type argument; e.g  f @Int x y
       -- NB: Has wildcards, but no implicit quantification
       --
       -- - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnAt',



  -- | Operator applications:
  -- NB Bracketed ops such as (+) come out as Vars.

  -- NB We need an expr for the operator in an OpApp/Section since
  -- the typechecker may need to apply the operator to a few types.

  | OpApp       (LHsExpr id)    -- left operand
                (LHsExpr id)    -- operator
                (LHsExpr id)    -- right operand

  -- | Negation operator. Contains the negated expression and the name
  -- of 'negate'
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnMinus'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | NegApp      (LHsExpr id)

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'('@,
  --             'ApiAnnotation.AnnClose' @')'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsPar       (LHsExpr id)    -- ^ Parenthesised expr; see Note [Parens in HsSyn]

  | SectionL    (LHsExpr id)    -- operand; see Note [Sections in HsSyn]
                (LHsExpr id)    -- operator
  | SectionR    (LHsExpr id)    -- operator; see Note [Sections in HsSyn]
                (LHsExpr id)    -- operand

  -- | Used for explicit tuples and sections thereof
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
  --         'ApiAnnotation.AnnClose'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | ExplicitTuple
        [LHsTupArg id]
        Boxity

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnCase',
  --       'ApiAnnotation.AnnOf','ApiAnnotation.AnnOpen' @'{'@,
  --       'ApiAnnotation.AnnClose' @'}'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsCase      (LHsExpr id)
                (MatchGroup id (LHsExpr id))

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnIf',
  --       'ApiAnnotation.AnnSemi',
  --       'ApiAnnotation.AnnThen','ApiAnnotation.AnnSemi',
  --       'ApiAnnotation.AnnElse',

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsIf        (LHsExpr id)    --  predicate
                (LHsExpr id)    --  then part
                (LHsExpr id)    --  else part

  -- | Multi-way if
  --
  -- - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnIf'
  --       'ApiAnnotation.AnnOpen','ApiAnnotation.AnnClose',

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsMultiIf [LGRHS id (LHsExpr id)]

  -- | let(rec)
  --
  -- - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnLet',
  --       'ApiAnnotation.AnnOpen' @'{'@,
  --       'ApiAnnotation.AnnClose' @'}'@,'ApiAnnotation.AnnIn'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsLet       (Located (HsLocalBinds id))
                (LHsExpr  id)

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnDo',
  --             'ApiAnnotation.AnnOpen', 'ApiAnnotation.AnnSemi',
  --             'ApiAnnotation.AnnVbar',
  --             'ApiAnnotation.AnnClose'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsDo        (HsStmtContext id)     -- The parameterisation is unimportant
                                         -- because in this context we never use
                                         -- the PatGuard or ParStmt variant
                (Located [ExprLStmt id]) -- "do":one or more stmts

  -- | Syntactic list: [a,b,c,...]
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'['@,
  --              'ApiAnnotation.AnnClose' @']'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | ExplicitList [LHsExpr id]

  -- | Syntactic parallel array: [:e1, ..., en:]
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'[:'@,
  --              'ApiAnnotation.AnnDotdot','ApiAnnotation.AnnComma',
  --              'ApiAnnotation.AnnVbar'
  --              'ApiAnnotation.AnnClose' @':]'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | ExplicitPArr [LHsExpr id]

  -- | Record construction
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'{'@,
  --         'ApiAnnotation.AnnDotdot','ApiAnnotation.AnnClose' @'}'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | RecordCon
      { rcon_con_name :: Located id         -- The constructor name;
                                            --  not used after type checking
      , rcon_flds     :: HsRecordBinds id } -- The fields

  -- | Record update
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'{'@,
  --         'ApiAnnotation.AnnDotdot','ApiAnnotation.AnnClose' @'}'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | RecordUpd
      { rupd_expr :: LHsExpr id
      , rupd_flds :: [LHsRecUpdField id]
      }
  -- For a type family, the arg types are of the *instance* tycon,
  -- not the family tycon

  -- | Expression with an explicit type signature. @e :: type@
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnDcolon'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | ExprWithTySig
                (LHsExpr id)
                (LHsSigWcType id)


  -- | Arithmetic sequence
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'['@,
  --              'ApiAnnotation.AnnComma','ApiAnnotation.AnnDotdot',
  --              'ApiAnnotation.AnnClose' @']'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | ArithSeq (ArithSeqInfo id)

  -- | Arithmetic sequence for parallel array
  --
  -- > [:e1..e2:] or [:e1, e2..e3:]
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'[:'@,
  --              'ApiAnnotation.AnnComma','ApiAnnotation.AnnDotdot',
  --              'ApiAnnotation.AnnVbar',
  --              'ApiAnnotation.AnnClose' @':]'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | PArrSeq (ArithSeqInfo id)

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'{-\# SCC'@,
  --             'ApiAnnotation.AnnVal' or 'ApiAnnotation.AnnValStr',
  --              'ApiAnnotation.AnnClose' @'\#-}'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsSCC       SourceText            -- Note [Pragma source text] in BasicTypes
                StringLiteral         -- "set cost centre" SCC pragma
                (LHsExpr id)          -- expr whose cost is to be measured

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'{-\# CORE'@,
  --             'ApiAnnotation.AnnVal', 'ApiAnnotation.AnnClose' @'\#-}'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsCoreAnn   SourceText            -- Note [Pragma source text] in BasicTypes
                StringLiteral         -- hdaume: core annotation
                (LHsExpr id)

  -----------------------------------------------------------
  -- MetaHaskell Extensions

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
  --         'ApiAnnotation.AnnOpen','ApiAnnotation.AnnClose',
  --         'ApiAnnotation.AnnClose'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsBracket    (HsBracket id)

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
  --         'ApiAnnotation.AnnClose'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsSpliceE  (HsSplice id)

  -----------------------------------------------------------
  -- Arrow notation extension

  -- | @proc@ notation for Arrows
  --
  --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnProc',
  --          'ApiAnnotation.AnnRarrow'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsProc      (LPat id)               -- arrow abstraction, proc
                (LHsCmdTop id)          -- body of the abstraction
                                        -- always has an empty stack

  ---------------------------------------
  -- static pointers extension
  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnStatic',

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsStatic (LHsExpr id)        -- Body

  ---------------------------------------
  -- The following are commands, not expressions proper
  -- They are only used in the parsing stage and are removed
  --    immediately in parser.RdrHsSyn.checkCommand

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.Annlarrowtail',
  --          'ApiAnnotation.Annrarrowtail','ApiAnnotation.AnnLarrowtail',
  --          'ApiAnnotation.AnnRarrowtail'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsArrApp             -- Arrow tail, or arrow application (f -< arg)
        (LHsExpr id)     -- arrow expression, f
        (LHsExpr id)     -- input expression, arg
        HsArrAppType     -- higher-order (-<<) or first-order (-<)
        Bool             -- True => right-to-left (f -< arg)
                         -- False => left-to-right (arg >- f)

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'(|'@,
  --         'ApiAnnotation.AnnClose' @'|)'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsArrForm            -- Command formation,  (| e cmd1 .. cmdn |)
        (LHsExpr id)     -- the operator
                         -- after type-checking, a type abstraction to be
                         -- applied to the type of the local environment tuple
        (Maybe Fixity)   -- fixity (filled in by the renamer), for forms that
                         -- were converted from OpApp's by the renamer
        [LHsCmdTop id]   -- argument commands

  ---------------------------------------
  -- Haskell program coverage (Hpc) Support

  | HsBinTick
     Int                                -- module-local tick number for True
     Int                                -- module-local tick number for False
     (LHsExpr id)                       -- sub-expression

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
  --       'ApiAnnotation.AnnOpen' @'{-\# GENERATED'@,
  --       'ApiAnnotation.AnnVal','ApiAnnotation.AnnVal',
  --       'ApiAnnotation.AnnColon','ApiAnnotation.AnnVal',
  --       'ApiAnnotation.AnnMinus',
  --       'ApiAnnotation.AnnVal','ApiAnnotation.AnnColon',
  --       'ApiAnnotation.AnnVal',
  --       'ApiAnnotation.AnnClose' @'\#-}'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsTickPragma                      -- A pragma introduced tick
     SourceText                       -- Note [Pragma source text] in BasicTypes
     (StringLiteral,(Int,Int),(Int,Int))
                                      -- external span for this tick
     ((SourceText,SourceText),(SourceText,SourceText))
        -- Source text for the four integers used in the span.
        -- See note [Pragma source text] in BasicTypes
     (LHsExpr id)

  ---------------------------------------
  -- These constructors only appear temporarily in the parser.
  -- The renamer translates them into the Right Thing.

  | EWildPat                 -- wildcard

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnAt'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | EAsPat      (Located id) -- as pattern
                (LHsExpr id)

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnRarrow'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | EViewPat    (LHsExpr id) -- view pattern
                (LHsExpr id)

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnTilde'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | ELazyPat    (LHsExpr id) -- ~ pattern

deriving instance (Data id) => Data (HsExpr id)

-- | HsTupArg is used for tuple sections
--  (,a,) is represented by  ExplicitTuple [Missing ty1, Present a, Missing ty3]
--  Which in turn stands for (\x:ty1 \y:ty2. (x,a,y))
type LHsTupArg id = Located (HsTupArg id)
-- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnComma'

-- For details on above see note [Api annotations] in ApiAnnotation
data HsTupArg id
  = Present (LHsExpr id)     -- ^ The argument
  | Missing
deriving instance (Data id) => Data (HsTupArg id)

tupArgPresent :: LHsTupArg id -> Bool
tupArgPresent (L _ (Present {})) = True
tupArgPresent (L _ (Missing {})) = False

{-
Note [Parens in HsSyn]
~~~~~~~~~~~~~~~~~~~~~~
HsPar (and ParPat in patterns, HsParTy in types) is used as follows

  * Generally HsPar is optional; the pretty printer adds parens where
    necessary.  Eg (HsApp f (HsApp g x)) is fine, and prints 'f (g x)'

  * HsPars are pretty printed as '( .. )' regardless of whether
    or not they are strictly necssary

  * HsPars are respected when rearranging operator fixities.
    So   a * (b + c)  means what it says (where the parens are an HsPar)

Note [Sections in HsSyn]
~~~~~~~~~~~~~~~~~~~~~~~~
Sections should always appear wrapped in an HsPar, thus
         HsPar (SectionR ...)
The parser parses sections in a wider variety of situations
(See Note [Parsing sections]), but the renamer checks for those
parens.  This invariant makes pretty-printing easier; we don't need
a special case for adding the parens round sections.

Note [Rebindable if]
~~~~~~~~~~~~~~~~~~~~
The rebindable syntax for 'if' is a bit special, because when
rebindable syntax is *off* we do not want to treat
   (if c then t else e)
as if it was an application (ifThenElse c t e).  Why not?
Because we allow an 'if' to return *unboxed* results, thus
  if blah then 3# else 4#
whereas that would not be possible using a all to a polymorphic function
(because you can't call a polymorphic function at an unboxed type).

So we use Nothing to mean "use the old built-in typing rule".

Note [Record Update HsWrapper]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There is a wrapper in RecordUpd which is used for the *required*
constraints for pattern synonyms. This wrapper is created in the
typechecking and is then directly used in the desugaring without
modification.

For example, if we have the record pattern synonym P,
  pattern P :: (Show a) => a -> Maybe a
  pattern P{x} = Just x

  foo = (Just True) { x = False }
then `foo` desugars to something like
  foo = case Just True of
          P x -> P False
hence we need to provide the correct dictionaries to P's matcher on
the RHS so that we can build the expression.

Note [Located RdrNames]
~~~~~~~~~~~~~~~~~~~~~~~
A number of syntax elements have seemingly redundant locations attached to them.
This is deliberate, to allow transformations making use of the API Annotations
to easily correlate a Located Name in the RenamedSource with a Located RdrName
in the ParsedSource.

There are unfortunately enough differences between the ParsedSource and the
RenamedSource that the API Annotations cannot be used directly with
RenamedSource, so this allows a simple mapping to be used based on the location.
-}

isQuietHsExpr :: HsExpr id -> Bool
-- Parentheses do display something, but it gives little info and
-- if we go deeper when we go inside them then we get ugly things
-- like (...)
isQuietHsExpr (HsPar _)          = True
-- applications don't display anything themselves
isQuietHsExpr (HsApp _ _)        = True
isQuietHsExpr (HsAppType _ _)    = True
isQuietHsExpr (OpApp _ _ _)      = True
isQuietHsExpr _ = False

-- We must tiresomely make the "id" parameter to the LHsWcType existential
-- because it's different in the HsAppType case and the HsAppTypeOut case
-- data LHsWcTypeX = forall id. (OutputableBndr id) => LHsWcTypeX (LHsWcType id)

hsExprNeedsParens :: HsExpr id -> Bool
-- True of expressions for which '(e)' and 'e'
-- mean the same thing
hsExprNeedsParens (ArithSeq {})       = False
hsExprNeedsParens (PArrSeq {})        = False
hsExprNeedsParens (HsLit {})          = False
hsExprNeedsParens (HsOverLit {})      = False
hsExprNeedsParens (HsVar {})          = False
hsExprNeedsParens (HsIPVar {})        = False
hsExprNeedsParens (HsOverLabel {})    = False
hsExprNeedsParens (ExplicitTuple {})  = False
hsExprNeedsParens (ExplicitList {})   = False
hsExprNeedsParens (ExplicitPArr {})   = False
hsExprNeedsParens (HsPar {})          = False
hsExprNeedsParens (HsBracket {})      = False
hsExprNeedsParens (HsDo sc _)
       | isListCompExpr sc            = False
hsExprNeedsParens (HsRecFld{})        = False
hsExprNeedsParens _ = True


isAtomicHsExpr :: HsExpr id -> Bool
-- True of a single token
isAtomicHsExpr (HsVar {})        = True
isAtomicHsExpr (HsLit {})        = True
isAtomicHsExpr (HsOverLit {})    = True
isAtomicHsExpr (HsIPVar {})      = True
isAtomicHsExpr (HsOverLabel {})  = True
isAtomicHsExpr (HsPar e)         = isAtomicHsExpr (unLoc e)
isAtomicHsExpr (HsRecFld{})      = True
isAtomicHsExpr _                 = False

{-
************************************************************************
*                                                                      *
\subsection{Commands (in arrow abstractions)}
*                                                                      *
************************************************************************

We re-use HsExpr to represent these.
-}

type LHsCmd id = Located (HsCmd id)

data HsCmd id
  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.Annlarrowtail',
  --          'ApiAnnotation.Annrarrowtail','ApiAnnotation.AnnLarrowtail',
  --          'ApiAnnotation.AnnRarrowtail'

  -- For details on above see note [Api annotations] in ApiAnnotation
  = HsCmdArrApp          -- Arrow tail, or arrow application (f -< arg)
        (LHsExpr id)     -- arrow expression, f
        (LHsExpr id)     -- input expression, arg
        HsArrAppType     -- higher-order (-<<) or first-order (-<)
        Bool             -- True => right-to-left (f -< arg)
                         -- False => left-to-right (arg >- f)

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'(|'@,
  --         'ApiAnnotation.AnnClose' @'|)'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | HsCmdArrForm         -- Command formation,  (| e cmd1 .. cmdn |)
        (LHsExpr id)     -- the operator
                         -- after type-checking, a type abstraction to be
                         -- applied to the type of the local environment tuple
        (Maybe Fixity)   -- fixity (filled in by the renamer), for forms that
                         -- were converted from OpApp's by the renamer
        [LHsCmdTop id]   -- argument commands

  | HsCmdApp    (LHsCmd id)
                (LHsExpr id)

  | HsCmdLam    (MatchGroup id (LHsCmd id))     -- kappa
       -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnLam',
       --       'ApiAnnotation.AnnRarrow',

       -- For details on above see note [Api annotations] in ApiAnnotation

  | HsCmdPar    (LHsCmd id)                     -- parenthesised command
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'('@,
    --             'ApiAnnotation.AnnClose' @')'@

    -- For details on above see note [Api annotations] in ApiAnnotation

  | HsCmdCase   (LHsExpr id)
                (MatchGroup id (LHsCmd id))     -- bodies are HsCmd's
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnCase',
    --       'ApiAnnotation.AnnOf','ApiAnnotation.AnnOpen' @'{'@,
    --       'ApiAnnotation.AnnClose' @'}'@

    -- For details on above see note [Api annotations] in ApiAnnotation

  | HsCmdIf     (LHsExpr id)                    -- predicate
                (LHsCmd id)                     -- then part
                (LHsCmd id)                     -- else part
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnIf',
    --       'ApiAnnotation.AnnSemi',
    --       'ApiAnnotation.AnnThen','ApiAnnotation.AnnSemi',
    --       'ApiAnnotation.AnnElse',

    -- For details on above see note [Api annotations] in ApiAnnotation

  | HsCmdLet    (Located (HsLocalBinds id))      -- let(rec)
                (LHsCmd  id)
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnLet',
    --       'ApiAnnotation.AnnOpen' @'{'@,
    --       'ApiAnnotation.AnnClose' @'}'@,'ApiAnnotation.AnnIn'

    -- For details on above see note [Api annotations] in ApiAnnotation

  | HsCmdDo     (Located [CmdLStmt id])

    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnDo',
    --             'ApiAnnotation.AnnOpen', 'ApiAnnotation.AnnSemi',
    --             'ApiAnnotation.AnnVbar',
    --             'ApiAnnotation.AnnClose'

    -- For details on above see note [Api annotations] in ApiAnnotation

deriving instance (Data id) => Data (HsCmd id)

data HsArrAppType = HsHigherOrderApp | HsFirstOrderApp
  deriving Data


{- | Top-level command, introducing a new arrow.
This may occur inside a proc (where the stack is empty) or as an
argument of a command-forming operator.
-}

type LHsCmdTop id = Located (HsCmdTop id)

data HsCmdTop id
  = HsCmdTop (LHsCmd id)
deriving instance (Data id) => Data (HsCmdTop id)

isQuietHsCmd :: HsCmd id -> Bool
-- Parentheses do display something, but it gives little info and
-- if we go deeper when we go inside them then we get ugly things
-- like (...)
isQuietHsCmd (HsCmdPar _) = True
-- applications don't display anything themselves
isQuietHsCmd (HsCmdApp _ _) = True
isQuietHsCmd _ = False

{-
************************************************************************
*                                                                      *
\subsection{Record binds}
*                                                                      *
************************************************************************
-}

type HsRecordBinds id = HsRecFields id (LHsExpr id)

{-
************************************************************************
*                                                                      *
\subsection{@Match@, @GRHSs@, and @GRHS@ datatypes}
*                                                                      *
************************************************************************

@Match@es are sets of pattern bindings and right hand sides for
functions, patterns or case branches. For example, if a function @g@
is defined as:
\begin{verbatim}
g (x,y) = y
g ((x:ys),y) = y+1,
\end{verbatim}
then \tr{g} has two @Match@es: @(x,y) = y@ and @((x:ys),y) = y+1@.

It is always the case that each element of an @[Match]@ list has the
same number of @pats@s inside it.  This corresponds to saying that
a function defined by pattern matching must have the same number of
patterns in each equation.
-}

data MatchGroup id body
  = MG { mg_alts    :: Located [LMatch id body]  -- The alternatives
       , mg_origin  :: Origin }
     -- The type is the type of the entire group
     --      t1 -> ... -> tn -> tr
     -- where there are n patterns
deriving instance (Data body,Data id) => Data (MatchGroup id body)

type LMatch id body = Located (Match id body)
-- ^ May have 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnSemi' when in a
--   list

-- For details on above see note [Api annotations] in ApiAnnotation
data Match id body
  = Match {
        m_ctxt :: HsMatchContext ( id),
          -- See note [m_ctxt in Match]
        m_pats :: [LPat id], -- The patterns
        m_type :: (Maybe (LHsType id)),
                                 -- A type signature for the result of the match
                                 -- Nothing after typechecking
                                 -- NB: No longer supported
        m_grhss :: (GRHSs id body)
  }
deriving instance (Data body,Data id) => Data (Match id body)

{-
Note [m_ctxt in Match]
~~~~~~~~~~~~~~~~~~~~~~

A Match can occur in a number of contexts, such as a FunBind, HsCase, HsLam and
so on.

In order to simplify tooling processing and pretty print output, the provenance
is captured in an HsMatchContext.

This is particularly important for the API Annotations for a multi-equation
FunBind.

The parser initially creates a FunBind with a single Match in it for
every function definition it sees.

These are then grouped together by getMonoBind into a single FunBind,
where all the Matches are combined.

In the process, all the original FunBind fun_id's bar one are
discarded, including the locations.

This causes a problem for source to source conversions via API
Annotations, so the original fun_ids and infix flags are preserved in
the Match, when it originates from a FunBind.

Example infix function definition requiring individual API Annotations

    (&&&  ) [] [] =  []
    xs    &&&   [] =  xs
    (  &&&  ) [] ys =  ys



-}

isInfixMatch :: Match id body -> Bool
isInfixMatch match = case m_ctxt match of
  FunRhs _ Infix -> True
  _              -> False

isEmptyMatchGroup :: MatchGroup id body -> Bool
isEmptyMatchGroup (MG { mg_alts = ms }) = null $ unLoc ms

-- | Is there only one RHS in this list of matches?
isSingletonMatchGroup :: [LMatch id body] -> Bool
isSingletonMatchGroup matches
  | [L _ match] <- matches
  , Match { m_grhss = GRHSs { grhssGRHSs = [_] } } <- match
  = True
  | otherwise
  = False

matchGroupArity :: MatchGroup id body -> Arity
-- Precondition: MatchGroup is non-empty
-- This is called before type checking, when mg_arg_tys is not set
matchGroupArity (MG { mg_alts = alts })
  | L _ (alt1:_) <- alts = length (hsLMatchPats alt1)
  | otherwise        = panic "matchGroupArity"

hsLMatchPats :: LMatch id body -> [LPat id]
hsLMatchPats (L _ (Match _ pats _ _)) = pats

-- | GRHSs are used both for pattern bindings and for Matches
--
--  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnVbar',
--        'ApiAnnotation.AnnEqual','ApiAnnotation.AnnWhere',
--        'ApiAnnotation.AnnOpen','ApiAnnotation.AnnClose'
--        'ApiAnnotation.AnnRarrow','ApiAnnotation.AnnSemi'

-- For details on above see note [Api annotations] in ApiAnnotation
data GRHSs id body
  = GRHSs {
      grhssGRHSs :: [LGRHS id body],       -- ^ Guarded RHSs
      grhssLocalBinds :: Located (HsLocalBinds id) -- ^ The where clause
    }
deriving instance (Data body,Data id) => Data (GRHSs id body)

type LGRHS id body = Located (GRHS id body)

-- | Guarded Right Hand Side.
data GRHS id body = GRHS [GuardLStmt id] -- Guards
                         body            -- Right hand side
deriving instance (Data body,Data id) => Data (GRHS id body)

{-
************************************************************************
*                                                                      *
\subsection{Do stmts and list comprehensions}
*                                                                      *
************************************************************************
-}

type LStmt id body = Located (StmtLR id id body)
type LStmtLR idL idR body = Located (StmtLR idL idR body)

type Stmt id body = StmtLR id id body

type CmdLStmt   id = LStmt id (LHsCmd  id)
type CmdStmt    id = Stmt  id (LHsCmd  id)
type ExprLStmt  id = LStmt id (LHsExpr id)
type ExprStmt   id = Stmt  id (LHsExpr id)

type GuardLStmt id = LStmt id (LHsExpr id)
type GuardStmt  id = Stmt  id (LHsExpr id)
type GhciLStmt  id = LStmt id (LHsExpr id)
type GhciStmt   id = Stmt  id (LHsExpr id)

-- The SyntaxExprs in here are used *only* for do-notation and monad
-- comprehensions, which have rebindable syntax. Otherwise they are unused.
-- | API Annotations when in qualifier lists or guards
--  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnVbar',
--         'ApiAnnotation.AnnComma','ApiAnnotation.AnnThen',
--         'ApiAnnotation.AnnBy','ApiAnnotation.AnnBy',
--         'ApiAnnotation.AnnGroup','ApiAnnotation.AnnUsing'

-- For details on above see note [Api annotations] in ApiAnnotation
data StmtLR idL idR body -- body should always be (LHs**** idR)
  = LastStmt  -- Always the last Stmt in ListComp, MonadComp, PArrComp,
              -- and (after the renamer) DoExpr, MDoExpr
              -- Not used for GhciStmtCtxt, PatGuard, which scope over other stuff
          body
          Bool               -- True <=> return was stripped by ApplicativeDo

  -- For details on above see note [Api annotations] in ApiAnnotation
  | BindStmt (LPat idL)
             body

  | BodyStmt body              -- See Note [BodyStmt]

  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnLet'
  --          'ApiAnnotation.AnnOpen' @'{'@,'ApiAnnotation.AnnClose' @'}'@,

  -- For details on above see note [Api annotations] in ApiAnnotation
  | LetStmt  (Located (HsLocalBindsLR idL idR))

  -- ParStmts only occur in a list/monad comprehension
  | ParStmt  [ParStmtBlock idL idR]

  | TransStmt {
      trS_form  :: TransForm,
      trS_stmts :: [ExprLStmt idL],   -- Stmts to the *left* of the 'group'
                                      -- which generates the tuples to be grouped

      trS_bndrs :: [(idR, idR)],      -- See Note [TransStmt binder map]

      trS_using :: LHsExpr idR,
      trS_by :: Maybe (LHsExpr idR)  -- "by e" (optional)
        -- Invariant: if trS_form = GroupBy, then grp_by = Just e

    }                                 -- See Note [Monad Comprehensions]

  -- Recursive statement (see Note [How RecStmt works] below)
  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnRec'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | RecStmt
     { recS_stmts :: [LStmtLR idL idR body]

        -- The next two fields are only valid after renaming
     , recS_later_ids :: [idR] -- The ids are a subset of the variables bound by the
                               -- stmts that are used in stmts that follow the RecStmt

     , recS_rec_ids :: [idR]   -- Ditto, but these variables are the "recursive" ones,
                               -- that are used before they are bound in the stmts of
                               -- the RecStmt.
        -- An Id can be in both groups
        -- Both sets of Ids are (now) treated monomorphically
        -- See Note [How RecStmt works] for why they are separate
      }
deriving instance (Data body, Data idL, Data idR)
  => Data (StmtLR idL idR body)

data TransForm   -- The 'f' below is the 'using' function, 'e' is the by function
  = ThenForm     -- then f               or    then f by e             (depending on trS_by)
  | GroupForm    -- then group using f   or    then group by e using f (depending on trS_by)
  deriving Data

data ParStmtBlock idL idR
  = ParStmtBlock
        [ExprLStmt idL]
        [idR]              -- The variables to be returned
deriving instance (Data idL, Data idR) => Data (ParStmtBlock idL idR)

data ApplicativeArg idL idR
  = ApplicativeArgOne            -- pat <- expr (pat must be irrefutable)
      (LPat idL)
      (LHsExpr idL)
  | ApplicativeArgMany           -- do { stmts; return vars }
      [ExprLStmt idL]            -- stmts
      (HsExpr idL)               -- return (v1,..,vn), or just (v1,..,vn)
      (LPat idL)                 -- (v1,...,vn)
deriving instance (Data idL, Data idR) => Data (ApplicativeArg idL idR)

{-
Note [The type of bind in Stmts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Some Stmts, notably BindStmt, keep the (>>=) bind operator.
We do NOT assume that it has type
    (>>=) :: m a -> (a -> m b) -> m b
In some cases (see Trac #303, #1537) it might have a more
exotic type, such as
    (>>=) :: m i j a -> (a -> m j k b) -> m i k b
So we must be careful not to make assumptions about the type.
In particular, the monad may not be uniform throughout.

Note [TransStmt binder map]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
The [(idR,idR)] in a TransStmt behaves as follows:

  * Before renaming: []

  * After renaming:
          [ (x27,x27), ..., (z35,z35) ]
    These are the variables
       bound by the stmts to the left of the 'group'
       and used either in the 'by' clause,
                or     in the stmts following the 'group'
    Each item is a pair of identical variables.

  * After typechecking:
          [ (x27:Int, x27:[Int]), ..., (z35:Bool, z35:[Bool]) ]
    Each pair has the same unique, but different *types*.

Note [BodyStmt]
~~~~~~~~~~~~~~~
BodyStmts are a bit tricky, because what they mean
depends on the context.  Consider the following contexts:

        A do expression of type (m res_ty)
        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        * BodyStmt E any_ty:   do { ....; E; ... }
                E :: m any_ty
          Translation: E >> ...

        A list comprehensions of type [elt_ty]
        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        * BodyStmt E Bool:   [ .. | .... E ]
                        [ .. | ..., E, ... ]
                        [ .. | .... | ..., E | ... ]
                E :: Bool
          Translation: if E then fail else ...

        A guard list, guarding a RHS of type rhs_ty
        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        * BodyStmt E BooParStmtBlockl:   f x | ..., E, ... = ...rhs...
                E :: Bool
          Translation: if E then fail else ...

        A monad comprehension of type (m res_ty)
        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        * BodyStmt E Bool:   [ .. | .... E ]
                E :: Bool
          Translation: guard E >> ...

Array comprehensions are handled like list comprehensions.

Note [How RecStmt works]
~~~~~~~~~~~~~~~~~~~~~~~~
Example:
   HsDo [ BindStmt x ex

        , RecStmt { recS_rec_ids   = [a, c]
                  , recS_stmts     = [ BindStmt b (return (a,c))
                                     , LetStmt a = ...b...
                                     , BindStmt c ec ]
                  , recS_later_ids = [a, b]

        , return (a b) ]

Here, the RecStmt binds a,b,c; but
  - Only a,b are used in the stmts *following* the RecStmt,
  - Only a,c are used in the stmts *inside* the RecStmt
        *before* their bindings

Why do we need *both* rec_ids and later_ids?  For monads they could be
combined into a single set of variables, but not for arrows.  That
follows from the types of the respective feedback operators:

        mfix :: MonadFix m => (a -> m a) -> m a
        loop :: ArrowLoop a => a (b,d) (c,d) -> a b c

* For mfix, the 'a' covers the union of the later_ids and the rec_ids
* For 'loop', 'c' is the later_ids and 'd' is the rec_ids

Note [Typing a RecStmt]
~~~~~~~~~~~~~~~~~~~~~~~
A (RecStmt stmts) types as if you had written

  (v1,..,vn, _, ..., _) <- mfix (\~(_, ..., _, r1, ..., rm) ->
                                 do { stmts
                                    ; return (v1,..vn, r1, ..., rm) })

where v1..vn are the later_ids
      r1..rm are the rec_ids

Note [Monad Comprehensions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Monad comprehensions require separate functions like 'return' and
'>>=' for desugaring. These functions are stored in the statements
used in monad comprehensions. For example, the 'return' of the 'LastStmt'
expression is used to lift the body of the monad comprehension:

  [ body | stmts ]
   =>
  stmts >>= \bndrs -> return body

In transform and grouping statements ('then ..' and 'then group ..') the
'return' function is required for nested monad comprehensions, for example:

  [ body | stmts, then f, rest ]
   =>
  f [ env | stmts ] >>= \bndrs -> [ body | rest ]

BodyStmts require the 'Control.Monad.guard' function for boolean
expressions:

  [ body | exp, stmts ]
   =>
  guard exp >> [ body | stmts ]

Parallel statements require the 'Control.Monad.Zip.mzip' function:

  [ body | stmts1 | stmts2 | .. ]
   =>
  mzip stmts1 (mzip stmts2 (..)) >>= \(bndrs1, (bndrs2, ..)) -> return body

In any other context than 'MonadComp', the fields for most of these
'SyntaxExpr's stay bottom.
-}

{-
************************************************************************
*                                                                      *
                Template Haskell quotation brackets
*                                                                      *
************************************************************************
-}

data HsSplice id
   = HsNativSplice       --  $n(f 4)
        id               -- A unique name to identify this splice point
        (LHsExpr id)     -- See Note [Pending Splices]

   | HsTypedSplice       --  $$z  or $$(f 4)
        id               -- A unique name to identify this splice point
        (LHsExpr id)     -- See Note [Pending Splices]

   | HsUntypedSplice     --  $z  or $(f 4)
        id               -- A unique name to identify this splice point
        (LHsExpr id)     -- See Note [Pending Splices]

   | HsQuasiQuote        -- See Note [Quasi-quote overview] in TcSplice
        id               -- Splice point
        id               -- Quoter
        SrcSpan          -- The span of the enclosed string
        FastString       -- The enclosed string

deriving instance (Data id) => Data (HsSplice id)

isTypedSplice :: HsSplice id -> Bool
isTypedSplice (HsTypedSplice {}) = True
isTypedSplice _                  = False   -- Quasi-quotes are untyped splices

data UntypedSpliceFlavour
  = UntypedExpSplice
  | UntypedPatSplice
  | UntypedTypeSplice
  | UntypedDeclSplice
  deriving Data

{-
Note [Pending Splices]
~~~~~~~~~~~~~~~~~~~~~~
When we rename an untyped bracket, we name and lift out all the nested
splices, so that when the typechecker hits the bracket, it can
typecheck those nested splices without having to walk over the untyped
bracket code.  So for example
    [| f $(g x) |]
looks like

    HsBracket (HsApp (HsVar "f") (HsSpliceE _ (g x)))

which the renamer rewrites to

    HsRnBracketOut (HsApp (HsVar f) (HsSpliceE sn (g x)))
                   [PendingRnSplice UntypedExpSplice sn (g x)]

* The 'sn' is the Name of the splice point, the SplicePointName

* The PendingRnExpSplice gives the splice that splice-point name maps to;
  and the typechecker can now conveniently find these sub-expressions

* The other copy of the splice, in the second argument of HsSpliceE
                                in the renamed first arg of HsRnBracketOut
  is used only for pretty printing

There are four varieties of pending splices generated by the renamer,
distinguished by their UntypedSpliceFlavour

 * Pending expression splices (UntypedExpSplice), e.g.,
       [|$(f x) + 2|]

   UntypedExpSplice is also used for
     * quasi-quotes, where the pending expression expands to
          $(quoter "...blah...")
       (see RnSplice.makePending, HsQuasiQuote case)

     * cross-stage lifting, where the pending expression expands to
          $(lift x)
       (see RnSplice.checkCrossStageLifting)

 * Pending pattern splices (UntypedPatSplice), e.g.,
       [| \$(f x) -> x |]

 * Pending type splices (UntypedTypeSplice), e.g.,
       [| f :: $(g x) |]

 * Pending declaration (UntypedDeclSplice), e.g.,
       [| let $(f x) in ... |]

There is a fifth variety of pending splice, which is generated by the type
checker:

  * Pending *typed* expression splices, (PendingTcSplice), e.g.,
        [||1 + $$(f 2)||]

It would be possible to eliminate HsRnBracketOut and use HsBracketOut for the
output of the renamer. However, when pretty printing the output of the renamer,
e.g., in a type error message, we *do not* want to print out the pending
splices. In contrast, when pretty printing the output of the type checker, we
*do* want to print the pending splices. So splitting them up seems to make
sense, although I hate to add another constructor to HsExpr.
-}


data HsBracket id = ExpBr (LHsExpr id)   -- [|  expr  |]
                  | PatBr (LPat id)      -- [p| pat   |]
                  | DecBrL [LHsDecl id]  -- [d| decls |]; result of parser
                  | DecBrG (HsGroup id)  -- [d| decls |]; result of renamer
                  | TypBr (LHsType id)   -- [t| type  |]
                  | VarBr Bool id        -- True: 'x, False: ''T
                                         -- (The Bool flag is used only in pprHsBracket)
                  | TExpBr (LHsExpr id)  -- [||  expr  ||]
                  | NativBr (LHsExpr id)  -- [n|  expr  |]
deriving instance (Data id) => Data (HsBracket id)

isTypedBracket :: HsBracket id -> Bool
isTypedBracket (TExpBr {}) = True
isTypedBracket _           = False

{-
************************************************************************
*                                                                      *
\subsection{Enumerations and list comprehensions}
*                                                                      *
************************************************************************
-}

data ArithSeqInfo id
  = From            (LHsExpr id)
  | FromThen        (LHsExpr id)
                    (LHsExpr id)
  | FromTo          (LHsExpr id)
                    (LHsExpr id)
  | FromThenTo      (LHsExpr id)
                    (LHsExpr id)
                    (LHsExpr id)
deriving instance (Data id) => Data (ArithSeqInfo id)

{-
************************************************************************
*                                                                      *
\subsection{HsMatchCtxt}
*                                                                      *
************************************************************************
-}

data FunctionFixity = Prefix | Infix deriving (Typeable,Data,Eq)

-- | Context of a Match
data HsMatchContext id
  = FunRhs (Located id) FunctionFixity -- ^Function binding for f, fixity
  | LambdaExpr                  -- ^Patterns of a lambda
  | CaseAlt                     -- ^Patterns and guards on a case alternative
  | IfAlt                       -- ^Guards of a multi-way if alternative
  | ProcExpr                    -- ^Patterns of a proc
  | PatBindRhs                  -- ^A pattern binding  eg [y] <- e = e

  | RecUpd                      -- ^Record update [used only in DsExpr to
                                --    tell matchWrapper what sort of
                                --    runtime error message to generate]

  | StmtCtxt (HsStmtContext id) -- ^Pattern of a do-stmt, list comprehension,
                                -- pattern guard, etc

  | ThPatSplice            -- ^A Template Haskell pattern splice
  | ThPatQuote             -- ^A Template Haskell pattern quotation [p| (a,b) |]
  | PatSyn                 -- ^A pattern synonym declaration
  deriving Functor
deriving instance (Data id) => Data (HsMatchContext id)

data HsStmtContext id
  = ListComp
  | MonadComp
  | PArrComp                         -- ^Parallel array comprehension

  | DoExpr                           -- ^do { ... }
  | MDoExpr                          -- ^mdo { ... }  ie recursive do-expression
  | ArrowExpr                        -- ^do-notation in an arrow-command context

  | GhciStmtCtxt                     -- ^A command-line Stmt in GHCi pat <- rhs
  | PatGuard (HsMatchContext id)     -- ^Pattern guard for specified thing
  | ParStmtCtxt (HsStmtContext id)   -- ^A branch of a parallel stmt
  | TransStmtCtxt (HsStmtContext id) -- ^A branch of a transform stmt
  deriving Functor
deriving instance (Data id) => Data (HsStmtContext id)

isListCompExpr :: HsStmtContext id -> Bool
-- Uses syntax [ e | quals ]
isListCompExpr ListComp          = True
isListCompExpr PArrComp          = True
isListCompExpr MonadComp         = True
isListCompExpr (ParStmtCtxt c)   = isListCompExpr c
isListCompExpr (TransStmtCtxt c) = isListCompExpr c
isListCompExpr _                 = False

isMonadCompExpr :: HsStmtContext id -> Bool
isMonadCompExpr MonadComp            = True
isMonadCompExpr (ParStmtCtxt ctxt)   = isMonadCompExpr ctxt
isMonadCompExpr (TransStmtCtxt ctxt) = isMonadCompExpr ctxt
isMonadCompExpr _                    = False
