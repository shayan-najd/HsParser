{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1997-1998

\section[BasicTypes]{Miscellanous types}

This module defines a miscellaneously collection of very simple
types that

\begin{itemize}
\item have no other obvious home
\item don't depend on any other complicated types
\item are used in more than one "part" of the compiler
\end{itemize}
-}

{-# LANGUAGE DeriveDataTypeable #-}

module BasicTypes(
   Boxity(..),
   Arity,
   FunctionOrData(..),
   FractionalLit(..),
   SourceText,
   tupleParens,
   Fixity(..),
   FixityDirection(..),
   TupleSort(..),
   Activation(..),
   InlinePragma(..),
   RuleMatchInfo(..),
   InlineSpec(..),
   RecFlag(..),
   OverlapMode(..),
   OverlapFlag(..),
   WarningTxt(..),
   StringLiteral(..),
   inlinePragmaSpec,
   isDefaultInlinePragma,
   boxityTupleSort,
   TopLevelFlag(..),
   RuleName(..),
   Origin(..),
   maxPrecedence
   ) where

import U.FastString
import U.Outputable
import SrcLoc ( Located,unLoc )
import Data.Data hiding (Fixity)
import Data.Function (on)

{-
************************************************************************
*                                                                      *
\subsection[Arity]{Arity}
*                                                                      *
************************************************************************
-}

-- | The number of value arguments that can be applied to a value before it does
-- "real work". So:
--  fib 100     has arity 0
--  \x -> fib x has arity 1
-- See also Note [Definition of arity] in CoreArity
type Arity = Int

-- | The number of represented arguments that can be applied to a value before it does
-- "real work". So:
--  fib 100                    has representation arity 0
--  \x -> fib x                has representation arity 1
--  \(# x, y #) -> fib (x + y) has representation arity 2

{-
************************************************************************
*                                                                      *
\subsection[FunctionOrData]{FunctionOrData}
*                                                                      *
************************************************************************
-}

data FunctionOrData = IsFunction | IsData
    deriving (Eq, Ord, Data)

instance Outputable FunctionOrData where
    ppr IsFunction = text "(function)"
    ppr IsData     = text "(data)"

data StringLiteral = StringLiteral
                       { sl_st :: SourceText, -- literal raw source.
                                              -- See not [Literal source text]
                         sl_fs :: FastString  -- literal string value
                       } deriving Data

instance Eq StringLiteral where
  (StringLiteral _ a) == (StringLiteral _ b) = a == b

-- reason/explanation from a WARNING or DEPRECATED pragma
data WarningTxt = WarningTxt (Located SourceText)
                             [Located StringLiteral]
                | DeprecatedTxt (Located SourceText)
                                [Located StringLiteral]
    deriving (Eq, Data)

instance Outputable WarningTxt where
    ppr (WarningTxt    _ ws)
                         = doubleQuotes (vcat (map (ftext . sl_fs . unLoc) ws))
    ppr (DeprecatedTxt _ ds)
                         = text "Deprecated:" <+>
                           doubleQuotes (vcat (map (ftext . sl_fs . unLoc) ds))

{-
************************************************************************
*                                                                      *
                Rules
*                                                                      *
************************************************************************
-}

type RuleName = FastString

{-
************************************************************************
*                                                                      *
\subsection[Fixity]{Fixity info}
*                                                                      *
************************************************************************
-}

------------------------
data Fixity = Fixity SourceText Int FixityDirection
  -- Note [Pragma source text]
  deriving Data

instance Outputable Fixity where
    ppr (Fixity _ prec dir) = hcat [ppr dir, space, int prec]

instance Eq Fixity where -- Used to determine if two fixities conflict
  (Fixity _ p1 dir1) == (Fixity _ p2 dir2) = p1==p2 && dir1 == dir2

------------------------
data FixityDirection = InfixL | InfixR | InfixN
                     deriving (Eq, Data)

instance Outputable FixityDirection where
    ppr InfixL = text "infixl"
    ppr InfixR = text "infixr"
    ppr InfixN = text "infix"

------------------------
maxPrecedence :: Int
maxPrecedence = 9

{-
************************************************************************
*                                                                      *
\subsection[Top-level/local]{Top-level/not-top level flag}
*                                                                      *
************************************************************************
-}

data TopLevelFlag
  = TopLevel
  | NotTopLevel

instance Outputable TopLevelFlag where
  ppr TopLevel    = text "<TopLevel>"
  ppr NotTopLevel = text "<NotTopLevel>"

{-
************************************************************************
*                                                                      *
                Boxity flag
*                                                                      *
************************************************************************
-}

data Boxity
  = Boxed
  | Unboxed
  deriving( Eq, Data )

instance Outputable Boxity where
  ppr Boxed   = text "Boxed"
  ppr Unboxed = text "Unboxed"

{-
************************************************************************
*                                                                      *
                Recursive/Non-Recursive flag
*                                                                      *
************************************************************************
-}

data RecFlag = Recursive
             | NonRecursive
             deriving( Eq, Data )

instance Outputable RecFlag where
  ppr Recursive    = text "Recursive"
  ppr NonRecursive = text "NonRecursive"

{-
************************************************************************
*                                                                      *
                Code origin
*                                                                      *
************************************************************************
-}

data Origin = FromSource
            | Generated
            deriving( Eq, Data )

instance Outputable Origin where
  ppr FromSource  = text "FromSource"
  ppr Generated   = text "Generated"

{-
************************************************************************
*                                                                      *
                Instance overlap flag
*                                                                      *
************************************************************************
-}

-- | The semantics allowed for overlapping instances for a particular
-- instance. See Note [Safe Haskell isSafeOverlap] (in `InstEnv.hs`) for a
-- explanation of the `isSafeOverlap` field.
--
-- - 'ApiAnnotation.AnnKeywordId' :
--      'ApiAnnotation.AnnOpen' @'\{-\# OVERLAPPABLE'@ or
--                              @'\{-\# OVERLAPPING'@ or
--                              @'\{-\# OVERLAPS'@ or
--                              @'\{-\# INCOHERENT'@,
--      'ApiAnnotation.AnnClose' @`\#-\}`@,

-- For details on above see note [Api annotations] in ApiAnnotation
data OverlapFlag = OverlapFlag
  { overlapMode   :: OverlapMode
  , isSafeOverlap :: Bool
  } deriving (Eq, Data)


data OverlapMode  -- See Note [Rules for instance lookup] in InstEnv
  = NoOverlap SourceText
                  -- See Note [Pragma source text]
    -- ^ This instance must not overlap another `NoOverlap` instance.
    -- However, it may be overlapped by `Overlapping` instances,
    -- and it may overlap `Overlappable` instances.


  | Overlappable SourceText
                  -- See Note [Pragma source text]
    -- ^ Silently ignore this instance if you find a
    -- more specific one that matches the constraint
    -- you are trying to resolve
    --
    -- Example: constraint (Foo [Int])
    --   instance                      Foo [Int]
    --   instance {-# OVERLAPPABLE #-} Foo [a]
    --
    -- Since the second instance has the Overlappable flag,
    -- the first instance will be chosen (otherwise
    -- its ambiguous which to choose)


  | Overlapping SourceText
                  -- See Note [Pragma source text]
    -- ^ Silently ignore any more general instances that may be
    --   used to solve the constraint.
    --
    -- Example: constraint (Foo [Int])
    --   instance {-# OVERLAPPING #-} Foo [Int]
    --   instance                     Foo [a]
    --
    -- Since the first instance has the Overlapping flag,
    -- the second---more general---instance will be ignored (otherwise
    -- it is ambiguous which to choose)


  | Overlaps SourceText
                  -- See Note [Pragma source text]
    -- ^ Equivalent to having both `Overlapping` and `Overlappable` flags.

  | Incoherent SourceText
                  -- See Note [Pragma source text]
    -- ^ Behave like Overlappable and Overlapping, and in addition pick
    -- an an arbitrary one if there are multiple matching candidates, and
    -- don't worry about later instantiation
    --
    -- Example: constraint (Foo [b])
    -- instance {-# INCOHERENT -} Foo [Int]
    -- instance                   Foo [a]
    -- Without the Incoherent flag, we'd complain that
    -- instantiating 'b' would change which instance
    -- was chosen. See also note [Incoherent instances] in InstEnv

  deriving (Eq, Data)

instance Outputable OverlapFlag where
   ppr flag = ppr (overlapMode flag) <+> pprSafeOverlap (isSafeOverlap flag)

instance Outputable OverlapMode where
   ppr (NoOverlap    _) = empty
   ppr (Overlappable _) = text "[overlappable]"
   ppr (Overlapping  _) = text "[overlapping]"
   ppr (Overlaps     _) = text "[overlap ok]"
   ppr (Incoherent   _) = text "[incoherent]"

pprSafeOverlap :: Bool -> SDoc
pprSafeOverlap True  = text "[safe]"
pprSafeOverlap False = empty

{-
************************************************************************
*                                                                      *
                Tuples
*                                                                      *
************************************************************************
-}

data TupleSort
  = BoxedTuple
  | UnboxedTuple
  | ConstraintTuple
  deriving( Eq, Data )

boxityTupleSort :: Boxity -> TupleSort
boxityTupleSort Boxed   = BoxedTuple
boxityTupleSort Unboxed = UnboxedTuple

tupleParens :: TupleSort -> SDoc -> SDoc
tupleParens BoxedTuple      p = parens p
tupleParens UnboxedTuple    p = text "(#" <+> p <+> ptext (sLit "#)")
tupleParens ConstraintTuple p = parens p

{-
************************************************************************
*                                                                      *
\subsection[Generic]{Generic flag}
*                                                                      *
************************************************************************

This is the "Embedding-Projection pair" datatype, it contains
two pieces of code (normally either RenamedExpr's or Id's)
If we have a such a pair (EP from to), the idea is that 'from' and 'to'
represents functions of type

        from :: T -> Tring
        to   :: Tring -> T

And we should have

        to (from x) = x

T and Tring are arbitrary, but typically T is the 'main' type while
Tring is the 'representation' type.  (This just helps us remember
whether to use 'from' or 'to'.
-}

{-
Embedding-projection pairs are used in several places:

First of all, each type constructor has an EP associated with it, the
code in EP converts (datatype T) from T to Tring and back again.

Secondly, when we are filling in Generic methods (in the typechecker,
tcMethodBinds), we are constructing bimaps by induction on the structure
of the type of the method signature.


************************************************************************
*                                                                      *
\subsection{Occurrence information}
*                                                                      *
************************************************************************

This data type is used exclusively by the simplifier, but it appears in a
SubstResult, which is currently defined in VarEnv, which is pretty near
the base of the module hierarchy.  So it seemed simpler to put the
defn of OccInfo here, safely at the bottom
-}

-- | Identifier occurrence information
data OccInfo
  = NoOccInfo           -- ^ There are many occurrences, or unknown occurrences

  | IAmDead             -- ^ Marks unused variables.  Sometimes useful for
                        -- lambda and case-bound variables.

  | OneOcc
        !InsideLam
        !OneBranch
        !InterestingCxt -- ^ Occurs exactly once, not inside a rule

  -- | This identifier breaks a loop of mutually recursive functions. The field
  -- marks whether it is only a loop breaker due to a reference in a rule
  | IAmALoopBreaker     -- Note [LoopBreaker OccInfo]
        !RulesOnly

  deriving (Eq)

type RulesOnly = Bool

{-
Note [LoopBreaker OccInfo]
~~~~~~~~~~~~~~~~~~~~~~~~~~
   IAmALoopBreaker True  <=> A "weak" or rules-only loop breaker
                             Do not preInlineUnconditionally

   IAmALoopBreaker False <=> A "strong" loop breaker
                             Do not inline at all

See OccurAnal Note [Weak loop breakers]
-}

-----------------
type InterestingCxt = Bool      -- True <=> Function: is applied
                                --          Data value: scrutinised by a case with
                                --                      at least one non-DEFAULT branch

-----------------
type InsideLam = Bool   -- True <=> Occurs inside a non-linear lambda
                        -- Substituting a redex for this occurrence is
                        -- dangerous because it might duplicate work.


-----------------
type OneBranch = Bool   -- True <=> Occurs in only one case branch

instance Outputable OccInfo where
  -- only used for debugging; never parsed.  KSW 1999-07
  ppr NoOccInfo            = empty
  ppr (IAmALoopBreaker ro) = text "LoopBreaker" <> if ro then char '!' else empty
  ppr IAmDead              = text "Dead"
  ppr (OneOcc inside_lam one_branch int_cxt)
        = text "Once" <> pp_lam <> pp_br <> pp_args
        where
          pp_lam | inside_lam = char 'L'
                 | otherwise  = empty
          pp_br  | one_branch = empty
                 | otherwise  = char '*'
          pp_args | int_cxt   = char '!'
                  | otherwise = empty


{-
************************************************************************
*                                                                      *
\subsection{Source Text}
*                                                                      *
************************************************************************
Keeping Source Text for source to source conversions

Note [Pragma source text]
~~~~~~~~~~~~~~~~~~~~~~~~~
The lexer does a case-insensitive match for pragmas, as well as
accepting both UK and US spelling variants.

So

  {-# SPECIALISE #-}
  {-# SPECIALIZE #-}
  {-# Specialize #-}

will all generate ITspec_prag token for the start of the pragma.

In order to be able to do source to source conversions, the original
source text for the token needs to be preserved, hence the
`SourceText` field.

So the lexer will then generate

  ITspec_prag "{ -# SPECIALISE"
  ITspec_prag "{ -# SPECIALIZE"
  ITspec_prag "{ -# Specialize"

for the cases above.
 [without the space between '{' and '-', otherwise this comment won't parse]


Note [Literal source text]
~~~~~~~~~~~~~~~~~~~~~~~~~~
The lexer/parser converts literals from their original source text
versions to an appropriate internal representation. This is a problem
for tools doing source to source conversions, so the original source
text is stored in literals where this can occur.

Motivating examples for HsLit

  HsChar          '\n'       == '\x20`
  HsCharPrim      '\x41`#    == `A`
  HsString        "\x20\x41" == " A"
  HsStringPrim    "\x20"#    == " "#
  HsInt           001        == 1
  HsIntPrim       002#       == 2#
  HsWordPrim      003##      == 3##
  HsInt64Prim     004##      == 4##
  HsWord64Prim    005##      == 5##
  HsInteger       006        == 6

For OverLitVal

  HsIntegral      003      == 0x003
  HsIsString      "\x41nd" == "And"
-}

type SourceText = String -- Note [Literal source text],[Pragma source text]


{-
************************************************************************
*                                                                      *
\subsection{Activation}
*                                                                      *
************************************************************************

When a rule or inlining is active
-}

type PhaseNum = Int  -- Compilation phase
                     -- Phases decrease towards zero
                     -- Zero is the last phase

data CompilerPhase
  = Phase PhaseNum
  | InitialPhase    -- The first phase -- number = infinity!

instance Outputable CompilerPhase where
   ppr (Phase n)    = int n
   ppr InitialPhase = text "InitialPhase"

-- See note [Pragma source text]
data Activation = NeverActive
                | AlwaysActive
                | ActiveBefore SourceText PhaseNum
                  -- Active only *strictly before* this phase
                | ActiveAfter SourceText PhaseNum
                  -- Active in this phase and later
                deriving( Eq, Data )
                  -- Eq used in comparing rules in HsDecls

data RuleMatchInfo = ConLike                    -- See Note [CONLIKE pragma]
                   | FunLike
                   deriving( Eq, Data, Show )
        -- Show needed for Lexer.x

data InlinePragma            -- Note [InlinePragma]
  = InlinePragma
      { inl_src    :: SourceText -- Note [Pragma source text]
      , inl_inline :: InlineSpec

      , inl_sat    :: Maybe Arity    -- Just n <=> Inline only when applied to n
                                     --            explicit (non-type, non-dictionary) args
                                     --   That is, inl_sat describes the number of *source-code*
                                     --   arguments the thing must be applied to.  We add on the
                                     --   number of implicit, dictionary arguments when making
                                     --   the InlineRule, and don't look at inl_sat further

      , inl_act    :: Activation     -- Says during which phases inlining is allowed

      , inl_rule   :: RuleMatchInfo  -- Should the function be treated like a constructor?
    } deriving( Eq, Data )

data InlineSpec   -- What the user's INLINE pragma looked like
  = Inline
  | Inlinable
  | NoInline
  | EmptyInlineSpec  -- Used in a place-holder InlinePragma in SpecPrag or IdInfo,
                     -- where there isn't any real inline pragma at all
  deriving( Eq, Data, Show )
        -- Show needed for Lexer.x

{-
Note [InlinePragma]
~~~~~~~~~~~~~~~~~~~
This data type mirrors what you can write in an INLINE or NOINLINE pragma in
the source program.

If you write nothing at all, you get defaultInlinePragma:
   inl_inline = False
   inl_act    = AlwaysActive
   inl_rule   = FunLike

It's not possible to get that combination by *writing* something, so
if an Id has defaultInlinePragma it means the user didn't specify anything.

If inl_inline = True, then the Id should have an InlineRule unfolding.

Note [CONLIKE pragma]
~~~~~~~~~~~~~~~~~~~~~
The ConLike constructor of a RuleMatchInfo is aimed at the following.
Consider first
    {-# RULE "r/cons" forall a as. r (a:as) = f (a+1) #-}
    g b bs = let x = b:bs in ..x...x...(r x)...
Now, the rule applies to the (r x) term, because GHC "looks through"
the definition of 'x' to see that it is (b:bs).

Now consider
    {-# RULE "r/f" forall v. r (f v) = f (v+1) #-}
    g v = let x = f v in ..x...x...(r x)...
Normally the (r x) would *not* match the rule, because GHC would be
scared about duplicating the redex (f v), so it does not "look
through" the bindings.

However the CONLIKE modifier says to treat 'f' like a constructor in
this situation, and "look through" the unfolding for x.  So (r x)
fires, yielding (f (v+1)).

This is all controlled with a user-visible pragma:
     {-# NOINLINE CONLIKE [1] f #-}

The main effects of CONLIKE are:

    - The occurrence analyser (OccAnal) and simplifier (Simplify) treat
      CONLIKE thing like constructors, by ANF-ing them

    - New function coreUtils.exprIsExpandable is like exprIsCheap, but
      additionally spots applications of CONLIKE functions

    - A CoreUnfolding has a field that caches exprIsExpandable

    - The rule matcher consults this field.  See
      Note [Expanding variables] in Rules.hs.
-}


isFunLike :: RuleMatchInfo -> Bool
isFunLike FunLike = True
isFunLike _            = False

isEmptyInlineSpec :: InlineSpec -> Bool
isEmptyInlineSpec EmptyInlineSpec = True
isEmptyInlineSpec _               = False

inlinePragmaSpec :: InlinePragma -> InlineSpec
inlinePragmaSpec = inl_inline

-- A DFun has an always-active inline activation so that
-- exprIsConApp_maybe can "see" its unfolding
-- (However, its actual Unfolding is a DFunUnfolding, which is
--  never inlined other than via exprIsConApp_maybe.)
isDefaultInlinePragma :: InlinePragma -> Bool
isDefaultInlinePragma (InlinePragma { inl_act = activation
                                    , inl_rule = match_info
                                    , inl_inline = inline })
  = isEmptyInlineSpec inline && isAlwaysActive activation && isFunLike match_info

instance Outputable Activation where
   ppr AlwaysActive       = brackets (text "ALWAYS")
   ppr NeverActive        = brackets (text "NEVER")
   ppr (ActiveBefore _ n) = brackets (char '~' <> int n)
   ppr (ActiveAfter  _ n) = brackets (int n)

instance Outputable RuleMatchInfo where
   ppr ConLike = text "CONLIKE"
   ppr FunLike = text "FUNLIKE"

instance Outputable InlineSpec where
   ppr Inline          = text "INLINE"
   ppr NoInline        = text "NOINLINE"
   ppr Inlinable       = text "INLINABLE"
   ppr EmptyInlineSpec = empty

instance Outputable InlinePragma where
  ppr (InlinePragma { inl_inline = inline, inl_act = activation
                    , inl_rule = info, inl_sat = mb_arity })
    = ppr inline <> pp_act inline activation <+> pp_sat <+> pp_info
    where
      pp_act Inline   AlwaysActive = empty
      pp_act NoInline NeverActive  = empty
      pp_act _        act          = ppr act

      pp_sat | Just ar <- mb_arity = parens (text "sat-args=" <> int ar)
             | otherwise           = empty
      pp_info | isFunLike info = empty
              | otherwise      = ppr info


isAlwaysActive :: Activation -> Bool
isAlwaysActive AlwaysActive = True
isAlwaysActive _            = False

data FractionalLit
  = FL { fl_text :: String         -- How the value was written in the source
       , fl_value :: Rational      -- Numeric value of the literal
       }
  deriving (Data, Show)
  -- The Show instance is required for the derived Lexer.x:Token instance when DEBUG is on


-- Comparison operations are needed when grouping literals
-- for compiling pattern-matching (module MatchLit)

instance Eq FractionalLit where
  (==) = (==) `on` fl_value

instance Ord FractionalLit where
  compare = compare `on` fl_value

instance Outputable FractionalLit where
  ppr = text . fl_text
