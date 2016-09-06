{-# LANGUAGE CPP, ImplicitParams #-}
{-
(c) The University of Glasgow 2006-2012
(c) The GRASP Project, Glasgow University, 1992-1998
-}

-- | This module defines classes and functions for pretty-printing. It also
-- exports a number of helpful debugging and other utilities such as 'trace' and 'panic'.
--
-- The interface to this module is very similar to the standard Hughes-PJ pretty printing
-- module, except that it exports a number of additional functions that are rarely used,
-- and works over the 'SDoc' type.
module U.Outputable (SDoc,
                     showSDocUnsafe,
                     Outputable(..),
                     ($$),
                     (<>),
                     (<+>),
                     braces,
                     ifPprDebug,
                     int,
                     char,
                     parens,
                     comma,
                     colon,
                     ppWhen,
                     ppUnless,
                     pprFastFilePath,
                     hcat,
                     vcat,
                     ftext,
                     empty,
                     OutputableBndr(..),
                     text,
                     sep,
                     quotes,
                     vbar,
                     cparen,
                     punctuate,
                     fsep,
                     keyword,
                     getPprStyle,
                     codeStyle,
                     ztext,
                     qualModule,
                     PprStyle(..),
                     pprWithCommas,
                     PrintUnqualified,
                     isEmpty,
                     pprDebugAndThen,
                     mkUserStyle,
                     defaultDumpStyle,
                     Depth(..),
                     defaultUserStyle,
                     defaultErrStyle,
                     doublePrec,
                     mkErrStyle,
                     alwaysQualify,
                     sdocWithDynFlags,
                     showSDoc,
                     blankLine,
                     dot,
                     hsep,
                     hang,
                     withPprStyle,
                     pprPrefixVar,
                     pprInfixVar,
                     debugStyle,
                     brackets,
                     QualifyName(..),
                     qualName,
                     dumpStyle,
                     pprPanic,
                     ptext,
                     doubleQuotes,
                     space,
                     interpp'SP,
                     integer,
                     pprHsBytes,
                     pprHsString,
                     pprPrimWord64,
                     pprPrimInt64,
                     pprPrimWord,
                     primDoubleSuffix,
                     primFloatSuffix,
                     pprHsChar,
                     pprPrimChar,
                     pprPrimInt,
                     quote,
                     dcolon,
                     paBrackets,
                     interppSP,
                     forAllLit,
                     darrow,
                     nest,
                     pprDeeperList,
                     equals,
                     BindingSite(..),
                     speakNOf,
                     ($+$),
                     arrow,
                     underscore,
                     arrowtt,
                     pprDeeper,
                     larrow,
                     lbrace,
                     rbrace,
                     semi,
                     angleBrackets,
                     larrowtt,
                     arrowt,
                     larrowt,
                     fcat,
                     rparen,
                     lparen) where

import {-# SOURCE #-}   U.DynFlags( DynFlags,defaultDynFlag,
                                    pprUserLength, pprCols,
                                    useUnicode, useUnicodeSyntax)
import {-# SOURCE #-}   Module(ModuleName)
import {-# SOURCE #-}   OccName( OccName )

import U.FastString
import qualified U.Pretty
import U.Util
import U.Pretty           ( Doc, Mode(..) )
import U.Panic
import GHC.LanguageExtensions.Type (Extension)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char
import qualified Data.Map as M
import Data.Int
import qualified Data.IntMap as IM
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String
import Data.Word
import System.FilePath (normalise)
import Numeric (showFFloat)
-- import Data.Graph (SCC(..))

-- import GHC.Fingerprint
import GHC.Show         ( showMultiLineString )

{-
************************************************************************
*                                                                      *
\subsection{The @PprStyle@ data type}
*                                                                      *
************************************************************************
-}

data PprStyle
  = PprUser PrintUnqualified Depth
                -- Pretty-print in a way that will make sense to the
                -- ordinary user; must be very close to Haskell
                -- syntax, etc.
                -- Assumes printing tidied code: non-system names are
                -- printed without uniques.

  | PprDump PrintUnqualified
                -- For -ddump-foo; less verbose than PprDebug, but more than PprUser
                -- Does not assume tidied code: non-external names
                -- are printed with uniques.

  | PprDebug    -- Full debugging output

  | PprCode CodeStyle
                -- Print code; either C or assembler

data CodeStyle {- = CStyle         -- The format of labels differs for C and assembler
                | AsmStyle -}

data Depth = AllTheWay
           | PartWay Int        -- 0 => stop


-- -----------------------------------------------------------------------------
-- Printing original names

-- | When printing code that contains original names, we need to map the
-- original names back to something the user understands.  This is the
-- purpose of the triple of functions that gets passed around
-- when rendering 'SDoc'.
data PrintUnqualified = QueryQualify {
    queryQualifyName    :: QueryQualifyName,
    queryQualifyModule  :: QueryQualifyModule
--    queryQualifyPackage :: QueryQualifyPackage
}

-- | given an /original/ name, this function tells you which module
-- name it should be qualified with when printing for the user, if
-- any.  For example, given @Control.Exception.catch@, which is in scope
-- as @Exception.catch@, this function will return @Just "Exception"@.
-- Note that the return value is a ModuleName, not a Module, because
-- in source code, names are qualified by ModuleNames.
type QueryQualifyName = ModuleName -> OccName -> QualifyName

-- | For a given module, we need to know whether to print it with
-- a package name to disambiguate it.
type QueryQualifyModule = ModuleName -> Bool

-- | For a given package, we need to know whether to print it with
-- the unit id to disambiguate it.
-- type QueryQualifyPackage = UnitId -> Bool

-- See Note [Printing original names] in HscTypes
data QualifyName   -- Given P:M.T
  = NameUnqual           -- It's in scope unqualified as "T"
                         -- OR nothing called "T" is in scope

  | NameQual ModuleName  -- It's in scope qualified as "X.T"

  | NameNotInScope1      -- It's not in scope at all, but M.T is not bound
                         -- in the current scope, so we can refer to it as "M.T"

  | NameNotInScope2      -- It's not in scope at all, and M.T is already bound in
                         -- the current scope, so we must refer to it as "P:M.T"

-- | NB: This won't ever show package IDs
alwaysQualifyNames :: QueryQualifyName
alwaysQualifyNames m _ = NameQual m

neverQualifyNames :: QueryQualifyName
neverQualifyNames _ _ = NameUnqual

alwaysQualifyModules :: QueryQualifyModule
alwaysQualifyModules _ = True

neverQualifyModules :: QueryQualifyModule
neverQualifyModules _ = False

alwaysQualify, neverQualify :: PrintUnqualified
alwaysQualify = QueryQualify alwaysQualifyNames
                             alwaysQualifyModules
                   --          alwaysQualifyPackages
neverQualify  = QueryQualify neverQualifyNames
                             neverQualifyModules
                 --            neverQualifyPackages

defaultUserStyle, defaultDumpStyle :: PprStyle

defaultUserStyle = mkUserStyle neverQualify AllTheWay
 -- Print without qualifiers to reduce verbosity, unless -dppr-debug

defaultDumpStyle = PprDump neverQualify

defaultErrStyle :: DynFlags -> PprStyle
-- Default style for error messages, when we don't know PrintUnqualified
-- It's a bit of a hack because it doesn't take into account what's in scope
-- Only used for desugarer warnings, and typechecker errors in interface sigs
-- NB that -dppr-debug will still get into PprDebug style
defaultErrStyle dflags = mkErrStyle dflags neverQualify

-- | Style for printing error messages
mkErrStyle :: DynFlags -> PrintUnqualified -> PprStyle
mkErrStyle dflags qual = mkUserStyle qual (PartWay (pprUserLength dflags))

mkUserStyle :: PrintUnqualified -> Depth -> PprStyle
mkUserStyle unqual depth = PprUser unqual depth

instance Outputable PprStyle where
  ppr (PprUser {})  = text "user-style"
  ppr (PprCode {})  = text "code-style"
  ppr (PprDump {})  = text "dump-style"
  ppr (PprDebug {}) = text "debug-style"

{-
Orthogonal to the above printing styles are (possibly) some
command-line flags that affect printing (often carried with the
style).  The most likely ones are variations on how much type info is
shown.

The following test decides whether or not we are actually generating
code (either C or assembly), or generating interface files.

************************************************************************
*                                                                      *
\subsection{The @SDoc@ data type}
*                                                                      *
************************************************************************
-}

newtype SDoc = SDoc { runSDoc :: SDocContext -> Doc }

data SDocContext = SDC
  { sdocStyle      :: !PprStyle
  , sdocLastColour :: !PprColour
    -- ^ The most recently used colour.  This allows nesting colours.
  , sdocDynFlags   :: !DynFlags
  }

instance IsString SDoc where
  fromString = text

initSDocContext :: DynFlags -> PprStyle -> SDocContext
initSDocContext dflags sty = SDC
  { sdocStyle = sty
  , sdocLastColour = colReset
  , sdocDynFlags = dflags
  }

withPprStyle :: PprStyle -> SDoc -> SDoc
withPprStyle sty d = SDoc $ \ctxt -> runSDoc d ctxt{sdocStyle=sty}

pprDeeper :: SDoc -> SDoc
pprDeeper d = SDoc $ \ctx -> case ctx of
  SDC{sdocStyle=PprUser _ (PartWay 0)} -> U.Pretty.text "..."
  SDC{sdocStyle=PprUser q (PartWay n)} ->
    runSDoc d ctx{sdocStyle = PprUser q (PartWay (n-1))}
  _ -> runSDoc d ctx

-- | Truncate a list that is longer than the current depth.
pprDeeperList :: ([SDoc] -> SDoc) -> [SDoc] -> SDoc
pprDeeperList f ds
  | null ds   = f []
  | otherwise = SDoc work
 where
  work ctx@SDC{sdocStyle=PprUser q (PartWay n)}
   | n==0      = U.Pretty.text "..."
   | otherwise =
      runSDoc (f (go 0 ds)) ctx{sdocStyle = PprUser q (PartWay (n-1))}
   where
     go _ [] = []
     go i (d:ds) | i >= n    = [text "...."]
                 | otherwise = d : go (i+1) ds
  work other_ctx = runSDoc (f ds) other_ctx

getPprStyle :: (PprStyle -> SDoc) -> SDoc
getPprStyle df = SDoc $ \ctx -> runSDoc (df (sdocStyle ctx)) ctx

sdocWithDynFlags :: (DynFlags -> SDoc) -> SDoc
sdocWithDynFlags f = SDoc $ \ctx -> runSDoc (f (sdocDynFlags ctx)) ctx


qualName :: PprStyle -> QueryQualifyName
qualName (PprUser q _)  mod occ = queryQualifyName q mod occ
qualName (PprDump q)    mod occ = queryQualifyName q mod occ
qualName _other         mod _   = NameQual mod

qualModule :: PprStyle -> QueryQualifyModule
qualModule (PprUser q _)  m = queryQualifyModule q m
qualModule (PprDump q)    m = queryQualifyModule q m
qualModule _other        _m = True

codeStyle :: PprStyle -> Bool
codeStyle (PprCode _)     = True
codeStyle _               = False

dumpStyle :: PprStyle -> Bool
dumpStyle (PprDump {}) = True
dumpStyle _other       = False

debugStyle :: PprStyle -> Bool
debugStyle PprDebug = True
debugStyle _other   = False

ifPprDebug :: SDoc -> SDoc        -- Empty for non-debug style
ifPprDebug d = SDoc $ \ctx ->
    case ctx of
        SDC{sdocStyle=PprDebug} -> runSDoc d ctx
        _                       -> U.Pretty.empty

-- Can't make SDoc an instance of Show because SDoc is just a function type
-- However, Doc *is* an instance of Show
-- showSDoc just blasts it out as a string
showSDoc :: DynFlags -> SDoc -> String
showSDoc dflags sdoc = renderWithStyle dflags sdoc defaultUserStyle

showSDocDump :: DynFlags -> SDoc -> String
showSDocDump dflags d = renderWithStyle dflags d defaultDumpStyle

renderWithStyle :: DynFlags -> SDoc -> PprStyle -> String
renderWithStyle dflags sdoc sty
  = let s = U.Pretty.style{ U.Pretty.mode = PageMode,
                          U.Pretty.lineLength = pprCols dflags }
    in U.Pretty.renderStyle s $ runSDoc sdoc (initSDocContext dflags sty)

isEmpty :: DynFlags -> SDoc -> Bool
isEmpty dflags sdoc = U.Pretty.isEmpty $ runSDoc sdoc dummySDocContext
   where dummySDocContext = initSDocContext dflags PprDebug

docToSDoc :: Doc -> SDoc
docToSDoc d = SDoc (\_ -> d)

empty    :: SDoc
char     :: Char       -> SDoc
text     :: String     -> SDoc
ftext    :: FastString -> SDoc
ptext    :: LitString  -> SDoc
ztext    :: FastZString -> SDoc
int      :: Int        -> SDoc
integer  :: Integer    -> SDoc

empty       = docToSDoc $ U.Pretty.empty
char c      = docToSDoc $ U.Pretty.char c

text s      = docToSDoc $ U.Pretty.text s
{-# INLINE text #-}   -- Inline so that the RULE U.Pretty.text will fire

ftext s     = docToSDoc $ U.Pretty.ftext s
ptext s     = docToSDoc $ U.Pretty.ptext s
ztext s     = docToSDoc $ U.Pretty.ztext s
int n       = docToSDoc $ U.Pretty.int n
integer n   = docToSDoc $ U.Pretty.integer n

-- | @doublePrec p n@ shows a floating point number @n@ with @p@
-- digits of precision after the decimal point.
doublePrec :: Int -> Double -> SDoc
doublePrec p n = text (showFFloat (Just p) n "")

parens, braces, brackets, quotes, quote,
        paBrackets, doubleQuotes, angleBrackets :: SDoc -> SDoc

parens d        = SDoc $ U.Pretty.parens . runSDoc d
braces d        = SDoc $ U.Pretty.braces . runSDoc d
brackets d      = SDoc $ U.Pretty.brackets . runSDoc d
quote d         = SDoc $ U.Pretty.quote . runSDoc d
doubleQuotes d  = SDoc $ U.Pretty.doubleQuotes . runSDoc d
angleBrackets d = char '<' <> d <> char '>'
paBrackets d    = text "[:" <> d <> text ":]"

cparen :: Bool -> SDoc -> SDoc
cparen b d = SDoc $ U.Pretty.maybeParens b . runSDoc d

-- 'quotes' encloses something in single quotes...
-- but it omits them if the thing begins or ends in a single quote
-- so that we don't get `foo''.  Instead we just have foo'.
quotes d =
      sdocWithDynFlags $ \dflags ->
      if useUnicode dflags
      then char '‘' <> d <> char '’'
      else SDoc $ \sty ->
           let pp_d = runSDoc d sty
               str  = show pp_d
           in case (str, snocView str) of
             (_, Just (_, '\'')) -> pp_d
             ('\'' : _, _)       -> pp_d
             _other              -> U.Pretty.quotes pp_d

semi, comma, colon, equals, space, dcolon, underscore, dot, vbar :: SDoc
arrow, larrow, darrow, arrowt, larrowt, arrowtt, larrowtt :: SDoc
lparen, rparen, lbrace, rbrace, blankLine :: SDoc

blankLine  = docToSDoc $ U.Pretty.text ""
dcolon     = unicodeSyntax (char '∷') (docToSDoc $ U.Pretty.text "::")
arrow      = unicodeSyntax (char '→') (docToSDoc $ U.Pretty.text "->")
larrow     = unicodeSyntax (char '←') (docToSDoc $ U.Pretty.text "<-")
darrow     = unicodeSyntax (char '⇒') (docToSDoc $ U.Pretty.text "=>")
arrowt     = unicodeSyntax (char '⤚') (docToSDoc $ U.Pretty.text ">-")
larrowt    = unicodeSyntax (char '⤙') (docToSDoc $ U.Pretty.text "-<")
arrowtt    = unicodeSyntax (char '⤜') (docToSDoc $ U.Pretty.text ">>-")
larrowtt   = unicodeSyntax (char '⤛') (docToSDoc $ U.Pretty.text "-<<")
semi       = docToSDoc $ U.Pretty.semi
comma      = docToSDoc $ U.Pretty.comma
colon      = docToSDoc $ U.Pretty.colon
equals     = docToSDoc $ U.Pretty.equals
space      = docToSDoc $ U.Pretty.space
underscore = char '_'
dot        = char '.'
vbar       = char '|'
lparen     = docToSDoc $ U.Pretty.lparen
rparen     = docToSDoc $ U.Pretty.rparen
lbrace     = docToSDoc $ U.Pretty.lbrace
rbrace     = docToSDoc $ U.Pretty.rbrace

forAllLit :: SDoc
forAllLit = unicodeSyntax (char '∀') (text "forall")

unicodeSyntax :: SDoc -> SDoc -> SDoc
unicodeSyntax unicode plain = sdocWithDynFlags $ \dflags ->
    if useUnicode dflags && useUnicodeSyntax dflags
    then unicode
    else plain

nest :: Int -> SDoc -> SDoc
-- ^ Indent 'SDoc' some specified amount
(<>) :: SDoc -> SDoc -> SDoc
-- ^ Join two 'SDoc' together horizontally without a gap
(<+>) :: SDoc -> SDoc -> SDoc
-- ^ Join two 'SDoc' together horizontally with a gap between them
($$) :: SDoc -> SDoc -> SDoc
-- ^ Join two 'SDoc' together vertically; if there is
-- no vertical overlap it "dovetails" the two onto one line
($+$) :: SDoc -> SDoc -> SDoc
-- ^ Join two 'SDoc' together vertically

nest n d    = SDoc $ U.Pretty.nest n . runSDoc d
(<>) d1 d2  = SDoc $ \sty -> (U.Pretty.<>)  (runSDoc d1 sty) (runSDoc d2 sty)
(<+>) d1 d2 = SDoc $ \sty -> (U.Pretty.<+>) (runSDoc d1 sty) (runSDoc d2 sty)
($$) d1 d2  = SDoc $ \sty -> (U.Pretty.$$)  (runSDoc d1 sty) (runSDoc d2 sty)
($+$) d1 d2 = SDoc $ \sty -> (U.Pretty.$+$) (runSDoc d1 sty) (runSDoc d2 sty)

hcat :: [SDoc] -> SDoc
-- ^ Concatenate 'SDoc' horizontally
hsep :: [SDoc] -> SDoc
-- ^ Concatenate 'SDoc' horizontally with a space between each one
vcat :: [SDoc] -> SDoc
-- ^ Concatenate 'SDoc' vertically with dovetailing
sep :: [SDoc] -> SDoc
-- ^ Catenate: is either like 'hcat' or like 'vcat', depending on what fits
fsep :: [SDoc] -> SDoc
-- ^ A paragraph-fill combinator. It's much like sep, only it
-- keeps fitting things on one line until it can't fit any more.
fcat :: [SDoc] -> SDoc
-- ^ This behaves like 'fsep', but it uses '<>' for horizontal conposition rather than '<+>'


hcat ds = SDoc $ \sty -> U.Pretty.hcat [runSDoc d sty | d <- ds]
hsep ds = SDoc $ \sty -> U.Pretty.hsep [runSDoc d sty | d <- ds]
vcat ds = SDoc $ \sty -> U.Pretty.vcat [runSDoc d sty | d <- ds]
sep ds  = SDoc $ \sty -> U.Pretty.sep  [runSDoc d sty | d <- ds]
fsep ds = SDoc $ \sty -> U.Pretty.fsep [runSDoc d sty | d <- ds]
fcat ds = SDoc $ \sty -> U.Pretty.fcat [runSDoc d sty | d <- ds]

hang :: SDoc  -- ^ The header
      -> Int  -- ^ Amount to indent the hung body
      -> SDoc -- ^ The hung body, indented and placed below the header
      -> SDoc
hang d1 n d2   = SDoc $ \sty -> U.Pretty.hang (runSDoc d1 sty) n (runSDoc d2 sty)

punctuate :: SDoc   -- ^ The punctuation
          -> [SDoc] -- ^ The list that will have punctuation added between every adjacent pair of elements
          -> [SDoc] -- ^ Punctuated list
punctuate _ []     = []
punctuate p (d:ds) = go d ds
                   where
                     go d [] = [d]
                     go d (e:es) = (d <> p) : go e es

ppWhen, ppUnless :: Bool -> SDoc -> SDoc
ppWhen True  doc = doc
ppWhen False _   = empty

ppUnless True  _   = empty
ppUnless False doc = doc

-- | A colour\/style for use with 'coloured'.
newtype PprColour = PprColour String

colBold :: PprColour
colBold = PprColour "\27[;1m"

colReset :: PprColour
colReset = PprColour "\27[0m"

-- | Apply the given colour\/style for the argument.
--
-- Only takes effect if colours are enabled.
coloured :: PprColour -> SDoc -> SDoc
-- coloured _ sdoc ctxt | coloursDisabled = sdoc ctxt
coloured col@(PprColour c) sdoc =
  SDoc $ \ctx@SDC{ sdocLastColour = PprColour lc } ->
    let ctx' = ctx{ sdocLastColour = col } in
    U.Pretty.zeroWidthText c U.Pretty.<> runSDoc sdoc ctx' U.Pretty.<> U.Pretty.zeroWidthText lc

bold :: SDoc -> SDoc
bold = coloured colBold

keyword :: SDoc -> SDoc
keyword = bold

{-
************************************************************************
*                                                                      *
\subsection[Outputable-class]{The @Outputable@ class}
*                                                                      *
************************************************************************
-}

-- | Class designating that some type has an 'SDoc' representation
class Outputable a where
        ppr :: a -> SDoc
        pprPrec :: Rational -> a -> SDoc
                -- 0 binds least tightly
                -- We use Rational because there is always a
                -- Rational between any other two Rationals

        ppr = pprPrec 0
        pprPrec _ = ppr

instance Outputable Char where
    ppr c = text [c]

instance Outputable Bool where
    ppr True  = text "True"
    ppr False = text "False"

instance Outputable Ordering where
    ppr LT = text "LT"
    ppr EQ = text "EQ"
    ppr GT = text "GT"

instance Outputable Int32 where
   ppr n = integer $ fromIntegral n

instance Outputable Int64 where
   ppr n = integer $ fromIntegral n

instance Outputable Int where
    ppr n = int n

instance Outputable Word16 where
    ppr n = integer $ fromIntegral n

instance Outputable Word32 where
    ppr n = integer $ fromIntegral n

instance Outputable Word where
    ppr n = integer $ fromIntegral n

instance Outputable () where
    ppr _ = text "()"

instance (Outputable a) => Outputable [a] where
    ppr xs = brackets (fsep (punctuate comma (map ppr xs)))

instance (Outputable a) => Outputable (Set a) where
    ppr s = braces (fsep (punctuate comma (map ppr (Set.toList s))))

instance (Outputable a, Outputable b) => Outputable (a, b) where
    ppr (x,y) = parens (sep [ppr x <> comma, ppr y])

instance Outputable a => Outputable (Maybe a) where
    ppr Nothing  = text "Nothing"
    ppr (Just x) = text "Just" <+> ppr x

instance (Outputable a, Outputable b) => Outputable (Either a b) where
    ppr (Left x)  = text "Left"  <+> ppr x
    ppr (Right y) = text "Right" <+> ppr y

-- may not be used
instance (Outputable a, Outputable b, Outputable c) => Outputable (a, b, c) where
    ppr (x,y,z) =
      parens (sep [ppr x <> comma,
                   ppr y <> comma,
                   ppr z ])

instance (Outputable a, Outputable b, Outputable c, Outputable d) =>
         Outputable (a, b, c, d) where
    ppr (a,b,c,d) =
      parens (sep [ppr a <> comma,
                   ppr b <> comma,
                   ppr c <> comma,
                   ppr d])

instance (Outputable a, Outputable b, Outputable c, Outputable d, Outputable e) =>
         Outputable (a, b, c, d, e) where
    ppr (a,b,c,d,e) =
      parens (sep [ppr a <> comma,
                   ppr b <> comma,
                   ppr c <> comma,
                   ppr d <> comma,
                   ppr e])

instance (Outputable a, Outputable b, Outputable c, Outputable d, Outputable e, Outputable f) =>
         Outputable (a, b, c, d, e, f) where
    ppr (a,b,c,d,e,f) =
      parens (sep [ppr a <> comma,
                   ppr b <> comma,
                   ppr c <> comma,
                   ppr d <> comma,
                   ppr e <> comma,
                   ppr f])

instance (Outputable a, Outputable b, Outputable c, Outputable d, Outputable e, Outputable f, Outputable g) =>
         Outputable (a, b, c, d, e, f, g) where
    ppr (a,b,c,d,e,f,g) =
      parens (sep [ppr a <> comma,
                   ppr b <> comma,
                   ppr c <> comma,
                   ppr d <> comma,
                   ppr e <> comma,
                   ppr f <> comma,
                   ppr g])

instance Outputable FastString where
    ppr fs = ftext fs           -- Prints an unadorned string,
                                -- no double quotes or anything

instance (Outputable key, Outputable elt) => Outputable (M.Map key elt) where
    ppr m = ppr (M.toList m)
instance (Outputable elt) => Outputable (IM.IntMap elt) where
    ppr m = ppr (IM.toList m)

instance Outputable Extension where
    ppr = text . show

{-
************************************************************************
*                                                                      *
\subsection{The @OutputableBndr@ class}
*                                                                      *
************************************************************************
-}

-- | 'BindingSite' is used to tell the thing that prints binder what
-- language construct is binding the identifier.  This can be used
-- to decide how much info to print.
-- Also see Note [Binding-site specific printing] in PprCore
data BindingSite
    = LambdaBind  -- ^ The x in   (\x. e)
    | CaseBind    -- ^ The x in   case scrut of x { (y,z) -> ... }
    | CasePatBind -- ^ The y,z in case scrut of x { (y,z) -> ... }
    | LetBind     -- ^ The x in   (let x = rhs in e)

-- | When we print a binder, we often want to print its type too.
-- The @OutputableBndr@ class encapsulates this idea.
class Outputable a => OutputableBndr a where
   pprBndr :: BindingSite -> a -> SDoc
   pprBndr _b x = ppr x

   pprPrefixOcc, pprInfixOcc :: a -> SDoc
      -- Print an occurrence of the name, suitable either in the
      -- prefix position of an application, thus   (f a b) or  ((+) x)
      -- or infix position,                 thus   (a `f` b) or  (x + y)

{-
************************************************************************
*                                                                      *
\subsection{Random printing helpers}
*                                                                      *
************************************************************************
-}

-- We have 31-bit Chars and will simply use Show instances of Char and String.

-- | Special combinator for showing character literals.
pprHsChar :: Char -> SDoc
pprHsChar c | c > '\x10ffff' = char '\\' <> text (show (fromIntegral (ord c) :: Word32))
            | otherwise      = text (show c)

-- | Special combinator for showing string literals.
pprHsString :: FastString -> SDoc
pprHsString fs = vcat (map text (showMultiLineString (unpackFS fs)))

-- | Special combinator for showing bytestring literals.
pprHsBytes :: ByteString -> SDoc
pprHsBytes bs = let escaped = concatMap escape $ BS.unpack bs
                in vcat (map text (showMultiLineString escaped)) <> char '#'
    where escape :: Word8 -> String
          escape w = let c = chr (fromIntegral w)
                     in if isAscii c
                        then [c]
                        else '\\' : show w

-- Postfix modifiers for unboxed literals.
-- See Note [Printing of literals in Core] in `basicTypes/Literal.hs`.
primCharSuffix, primFloatSuffix, primIntSuffix :: SDoc
primDoubleSuffix, primWordSuffix, primInt64Suffix, primWord64Suffix :: SDoc
primCharSuffix   = char '#'
primFloatSuffix  = char '#'
primIntSuffix    = char '#'
primDoubleSuffix = text "##"
primWordSuffix   = text "##"
primInt64Suffix  = text "L#"
primWord64Suffix = text "L##"

-- | Special combinator for showing unboxed literals.
pprPrimChar :: Char -> SDoc
pprPrimInt, pprPrimWord, pprPrimInt64, pprPrimWord64 :: Integer -> SDoc
pprPrimChar c   = pprHsChar c <> primCharSuffix
pprPrimInt i    = integer i   <> primIntSuffix
pprPrimWord w   = integer w   <> primWordSuffix
pprPrimInt64 i  = integer i   <> primInt64Suffix
pprPrimWord64 w = integer w   <> primWord64Suffix

---------------------
-- Put a name in parens if it's an operator
pprPrefixVar :: Bool -> SDoc -> SDoc
pprPrefixVar is_operator pp_v
  | is_operator = parens pp_v
  | otherwise   = pp_v

-- Put a name in backquotes if it's not an operator
pprInfixVar :: Bool -> SDoc -> SDoc
pprInfixVar is_operator pp_v
  | is_operator = pp_v
  | otherwise   = char '`' <> pp_v <> char '`'

---------------------
pprFastFilePath :: FastString -> SDoc
pprFastFilePath path = text $ normalise $ unpackFS path

{-
************************************************************************
*                                                                      *
\subsection{Other helper functions}
*                                                                      *
************************************************************************
-}

pprWithCommas :: (a -> SDoc) -- ^ The pretty printing function to use
              -> [a]         -- ^ The things to be pretty printed
              -> SDoc        -- ^ 'SDoc' where the things have been pretty printed,
                             -- comma-separated and finally packed into a paragraph.
pprWithCommas pp xs = fsep (punctuate comma (map pp xs))

-- | Returns the separated concatenation of the pretty printed things.
interppSP  :: Outputable a => [a] -> SDoc
interppSP  xs = sep (map ppr xs)

-- | Returns the comma-separated concatenation of the pretty printed things.
interpp'SP :: Outputable a => [a] -> SDoc
interpp'SP xs = sep (punctuate comma (map ppr xs))

-- | Converts an integer to a verbal multiplicity:
--
-- > speakN 0 = text "none"
-- > speakN 5 = text "five"
-- > speakN 10 = text "10"
speakN :: Int -> SDoc
speakN 0 = text "none"  -- E.g.  "he has none"
speakN 1 = text "one"   -- E.g.  "he has one"
speakN 2 = text "two"
speakN 3 = text "three"
speakN 4 = text "four"
speakN 5 = text "five"
speakN 6 = text "six"
speakN n = int n

-- | Converts an integer and object description to a statement about the
-- multiplicity of those objects:
--
-- > speakNOf 0 (text "melon") = text "no melons"
-- > speakNOf 1 (text "melon") = text "one melon"
-- > speakNOf 3 (text "melon") = text "three melons"
speakNOf :: Int -> SDoc -> SDoc
speakNOf 0 d = text "no" <+> d <> char 's'
speakNOf 1 d = text "one" <+> d                 -- E.g. "one argument"
speakNOf n d = speakN n <+> d <> char 's'               -- E.g. "three arguments"

{-
************************************************************************
*                                                                      *
\subsection{Error handling}
*                                                                      *
************************************************************************
-}

pprPanic :: String -> SDoc -> a
-- ^ Throw an exception saying "bug in GHC"
pprPanic    = panicDoc

pprDebugAndThen :: DynFlags -> (String -> a) -> SDoc -> SDoc -> a
pprDebugAndThen dflags cont heading pretty_msg
 = cont (showSDocDump dflags doc)
 where
     doc = sep [heading, nest 2 pretty_msg]

showSDocUnsafe :: SDoc -> String
showSDocUnsafe = showSDoc defaultDynFlag
