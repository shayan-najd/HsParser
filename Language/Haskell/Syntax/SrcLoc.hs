-- (c) The Language.Haskell.Utility.iversity of Glasgow, 1992-2006

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
   -- Workaround for Trac #5252 crashes the bootstrap compiler without -O
   -- When the earliest compiler we want to boostrap with is
   -- GHC 7.2, we can make RealSocLoc properly abstract

-- | This module contains types that relate to the positions of things
-- in source files, and allow tagging of those things with locations
module Language.Haskell.Syntax.SrcLoc ( SrcLoc(..)
                                      , SrcSpan(..)
                                      , Located
                                      , GenLocated(..)
                                      , RealLocated(..)
                                      , RealSrcLoc(..)
                                      , RealSrcSpan(..)
                                      , noLoc
                                      , combineLocs
                                      , getLoc
                                      , unLoc
                                      , mkSrcSpan
                                      , mkSrcLoc
                                      , mkRealSrcSpan
                                      , mkRealSrcLoc
                                      , noSrcSpan
                                      , srcSpanEnd
                                      , isSubspanOf
                                      , isPointRealSpan
                                      , isOneLineRealSpan
                                      , srcSpanStart
                                      , advanceSrcLoc
                                      , realSrcSpanStart
                                      , srcSpanStartLine
                                      , srcSpanEndLine
                                      , srcLocCol
                                      , srcSpanStartCol
                                      , srcSpanEndCol
                                      , combineSrcSpans
                                      , addCLoc
                                      , srcSpanFirstCharacter
                                      , srcLocFile) where

import Language.Haskell.Utility.FastString

import Data.Bits
import Data.Data
import Data.List

{-
************************************************************************
*                                                                      *
\subsection[SrcLoc-SrcLocations]{Source-location information}
*                                                                      *
************************************************************************

We keep information about the {\em definition} point for each entity;
this is the obvious stuff:
-}

-- | Represents a single point within a file
data RealSrcLoc
  = SrcLoc      FastString              -- A precise location (file name)
                {-# Language.Haskell.Utility.PACK #-} !Int     -- line number, begins at 1
                {-# Language.Haskell.Utility.PACK #-} !Int     -- column number, begins at 1
  deriving (Eq, Ord)

data SrcLoc
  = RealSrcLoc {-# Language.Haskell.Utility.PACK #-}!RealSrcLoc
  | UnhelpfulLoc FastString     -- Just a general indication
  deriving (Eq, Ord, Show)

{-
************************************************************************
*                                                                      *
\subsection[SrcLoc-access-fns]{Access functions}
*                                                                      *
************************************************************************
-}

mkSrcLoc :: FastString -> Int -> Int -> SrcLoc
mkSrcLoc x line col = RealSrcLoc (mkRealSrcLoc x line col)

mkRealSrcLoc :: FastString -> Int -> Int -> RealSrcLoc
mkRealSrcLoc x line col = SrcLoc x line col

-- | Gives the filename of the 'RealSrcLoc'
srcLocFile :: RealSrcLoc -> FastString
srcLocFile (SrcLoc fname _ _) = fname

-- | Raises an error when used on a "bad" 'SrcLoc'
srcLocLine :: RealSrcLoc -> Int
srcLocLine (SrcLoc _ l _) = l

-- | Raises an error when used on a "bad" 'SrcLoc'
srcLocCol :: RealSrcLoc -> Int
srcLocCol (SrcLoc _ _ c) = c

-- | Move the 'SrcLoc' down by one line if the character is a newline,
-- to the next 8-char tabstop if it is a tab, and across by one
-- character in any other case
advanceSrcLoc :: RealSrcLoc -> Char -> RealSrcLoc
advanceSrcLoc (SrcLoc f l _) '\n' = SrcLoc f  (l + 1) 1
advanceSrcLoc (SrcLoc f l c) '\t' = SrcLoc f  l (((((c - 1) `shiftR` 3) + 1)
                                                  `shiftL` 3) + 1)
advanceSrcLoc (SrcLoc f l c) _    = SrcLoc f  l (c + 1)

{-
************************************************************************
*                                                                      *
\subsection[SrcLoc-instances]{Instance declarations for various names}
*                                                                      *
************************************************************************
-}


abstractConstr :: String -> Constr
abstractConstr n = mkConstr (abstractDataType n) ("{abstract:"++n++"}") [] Prefix

abstractDataType :: String -> DataType
abstractDataType n = mkDataType n [abstractConstr n]

instance Data RealSrcSpan where
  -- don't traverse?
  toConstr _   = abstractConstr "RealSrcSpan"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNoRepType "RealSrcSpan"

instance Data SrcSpan where
  -- don't traverse?
  toConstr _   = abstractConstr "SrcSpan"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNoRepType "SrcSpan"

{-
************************************************************************
*                                                                      *
\subsection[SrcSpan]{Source Spans}
*                                                                      *
************************************************************************
-}

{- |
A 'RealSrcSpan' delimits a portion of a text file.  It could be represented
by a pair of (line,column) coordinates, but in fact we optimise
slightly by using more compact representations for single-line and
zero-length spans, both of which are quite common.

The end position is defined to be the column /after/ the end of the
span.  That is, a span of (1,1)-(1,2) is one character long, and a
span of (1,1)-(1,1) is zero characters long.
-}
data RealSrcSpan
  = RealSrcSpan'
        { srcSpanFile     :: !FastString,
          srcSpanSoine    :: {-# Language.Haskell.Utility.PACK #-} !Int,
          srcSpanSool     :: {-# Language.Haskell.Utility.PACK #-} !Int,
          srcSpanELine    :: {-# Language.Haskell.Utility.PACK #-} !Int,
          srcSpanECol     :: {-# Language.Haskell.Utility.PACK #-} !Int
        }
  deriving Eq

-- | A 'SrcSpan' identifies either a specific portion of a text file
-- or a human-readable description of a location.
data SrcSpan =
    RealSrcSpan !RealSrcSpan
  | UnhelpfulSpan !FastString   -- Just a general indication
                                -- also used to indicate an empty span

  deriving (Eq, Ord, Show) -- Show is used by Lexer.x, because we
                           -- derive Show for Token

-- | Built-in "bad" 'SrcSpan's for common sources of location uncertainty
noSrcSpan :: SrcSpan
noSrcSpan          = UnhelpfulSpan (fsLit "<no location info>")

-- | Create a 'SrcSpan' between two points in a file
mkRealSrcSpan :: RealSrcLoc -> RealSrcLoc -> RealSrcSpan
mkRealSrcSpan loc1 loc2 = RealSrcSpan' file line1 col1 line2 col2
  where
        line1 = srcLocLine loc1
        line2 = srcLocLine loc2
        col1 = srcLocCol loc1
        col2 = srcLocCol loc2
        file = srcLocFile loc1

-- | 'True' if the span is known to straddle only one line.
isOneLineRealSpan :: RealSrcSpan -> Bool
isOneLineRealSpan (RealSrcSpan' _ line1 _ line2 _)
  = line1 == line2

-- | 'True' if the span is a single point
isPointRealSpan :: RealSrcSpan -> Bool
isPointRealSpan (RealSrcSpan' _ line1 col1 line2 col2)
  = line1 == line2 && col1 == col2

-- | Create a 'SrcSpan' between two points in a file
mkSrcSpan :: SrcLoc -> SrcLoc -> SrcSpan
mkSrcSpan (UnhelpfulLoc str) _ = UnhelpfulSpan str
mkSrcSpan _ (UnhelpfulLoc str) = UnhelpfulSpan str
mkSrcSpan (RealSrcLoc loc1) (RealSrcLoc loc2)
    = RealSrcSpan (mkRealSrcSpan loc1 loc2)

-- | Combines two 'SrcSpan' into one that spans at least all the characters
-- within both spans. Assumes the "file" part is the same in both inputs
combineSrcSpans :: SrcSpan -> SrcSpan -> SrcSpan
combineSrcSpans (UnhelpfulSpan _) r = r -- this seems more useful
combineSrcSpans l (UnhelpfulSpan _) = l
combineSrcSpans (RealSrcSpan span1) (RealSrcSpan span2)
    = RealSrcSpan (combineRealSrcSpans span1 span2)

-- | Combines two 'SrcSpan' into one that spans at least all the characters
-- within both spans. Assumes the "file" part is the same in both inputs
combineRealSrcSpans :: RealSrcSpan -> RealSrcSpan -> RealSrcSpan
combineRealSrcSpans span1 span2
  = RealSrcSpan' file line_start col_start line_end col_end
  where
    (line_start, col_start) = min (srcSpanStartLine span1, srcSpanStartCol span1)
                                  (srcSpanStartLine span2, srcSpanStartCol span2)
    (line_end, col_end)     = max (srcSpanEndLine span1, srcSpanEndCol span1)
                                  (srcSpanEndLine span2, srcSpanEndCol span2)
    file = srcSpanFile span1

-- | Convert a SrcSpan into one that represents only its first character
srcSpanFirstCharacter :: SrcSpan -> SrcSpan
srcSpanFirstCharacter l@(UnhelpfulSpan {}) = l
srcSpanFirstCharacter (RealSrcSpan span) = RealSrcSpan $ mkRealSrcSpan loc1 loc2
  where
    loc1@(SrcLoc f l c) = realSrcSpanStart span
    loc2 = SrcLoc f l (c+1)

{-
%************************************************************************
%*                                                                      *
\subsection[SrcSpan-unsafe-access-fns]{Language.Haskell.Utility.safe access functions}
*                                                                      *
************************************************************************
-}

srcSpanStartLine :: RealSrcSpan -> Int
srcSpanEndLine :: RealSrcSpan -> Int
srcSpanStartCol :: RealSrcSpan -> Int
srcSpanEndCol :: RealSrcSpan -> Int

srcSpanStartLine RealSrcSpan'{ srcSpanSoine=l } = l
srcSpanEndLine RealSrcSpan'{ srcSpanELine=l } = l
srcSpanStartCol RealSrcSpan'{ srcSpanSool=l } = l
srcSpanEndCol RealSrcSpan'{ srcSpanECol=c } = c

{-
************************************************************************
*                                                                      *
\subsection[SrcSpan-access-fns]{Access functions}
*                                                                      *
************************************************************************
-}

-- | Returns the location at the start of the 'SrcSpan' or a "bad" 'SrcSpan' if that is unavailable
srcSpanStart :: SrcSpan -> SrcLoc
srcSpanStart (UnhelpfulSpan str) = UnhelpfulLoc str
srcSpanStart (RealSrcSpan s) = RealSrcLoc (realSrcSpanStart s)

-- | Returns the location at the end of the 'SrcSpan' or a "bad" 'SrcSpan' if that is unavailable
srcSpanEnd :: SrcSpan -> SrcLoc
srcSpanEnd (UnhelpfulSpan str) = UnhelpfulLoc str
srcSpanEnd (RealSrcSpan s) = RealSrcLoc (realSrcSpanEnd s)

realSrcSpanStart :: RealSrcSpan -> RealSrcLoc
realSrcSpanStart s = mkRealSrcLoc (srcSpanFile s)
                                  (srcSpanStartLine s)
                                  (srcSpanStartCol s)

realSrcSpanEnd :: RealSrcSpan -> RealSrcLoc
realSrcSpanEnd s = mkRealSrcLoc (srcSpanFile s)
                                (srcSpanEndLine s)
                                (srcSpanEndCol s)

-- | Obtains the filename for a 'SrcSpan' if it is "good"
srcSpanFileName_maybe :: SrcSpan -> Maybe FastString
srcSpanFileName_maybe (RealSrcSpan s)   = Just (srcSpanFile s)
srcSpanFileName_maybe (UnhelpfulSpan _) = Nothing

{-
************************************************************************
*                                                                      *
\subsection[SrcSpan-instances]{Instances}
*                                                                      *
************************************************************************
-}

-- We want to order RealSrcSpans first by the start point, then by the
-- end point.
instance Ord RealSrcSpan where
  a `compare` b =
     (realSrcSpanStart a `compare` realSrcSpanStart b) `thenCmp`
     (realSrcSpanEnd   a `compare` realSrcSpanEnd   b)

infixr 9 `thenCmp`
thenCmp :: Ordering -> Ordering -> Ordering
{-# INLINE thenCmp #-}
thenCmp EQ       ordering = ordering
thenCmp ordering _        = ordering

instance Show RealSrcLoc where
  show (SrcLoc filename row col)
      = "SrcLoc " ++ show filename ++ " " ++ show row ++ " " ++ show col

-- Show is used by Lexer.x, because we derive Show for Token
instance Show RealSrcSpan where
  show span@(RealSrcSpan' file sl sc el ec)
    | isPointRealSpan span
    = "SrcSpanPoint " ++ show file ++ " " ++ intercalate " " (map show [sl,sc])

    | isOneLineRealSpan span
    = "SrcSpanOneLine " ++ show file ++ " "
                        ++ intercalate " " (map show [sl,sc,ec])

    | otherwise
    = "SrcSpanMultiLine " ++ show file ++ " "
                          ++ intercalate " " (map show [sl,sc,el,ec])
{-
************************************************************************
*                                                                      *
\subsection[Located]{Attaching SrcSpans to things}
*                                                                      *
************************************************************************
-}

-- | We attach SrcSpans to lots of things, so let's have a datatype for it.
data GenLocated l e = L l e
  deriving (Eq, Ord, Data, Functor, Foldable, Traversable)

type Located e = GenLocated SrcSpan e
type RealLocated e = GenLocated RealSrcSpan e

unLoc :: GenLocated l e -> e
unLoc (L _ e) = e

getLoc :: GenLocated l e -> l
getLoc (L l _) = l

noLoc :: e -> Located e
noLoc e = L noSrcSpan e

combineLocs :: Located a -> Located b -> SrcSpan
combineLocs a b = combineSrcSpans (getLoc a) (getLoc b)

-- | Combine locations from two 'Located' things and add them to a third thing
addCLoc :: Located a -> Located b -> c -> Located c
addCLoc a b c = L (combineSrcSpans (getLoc a) (getLoc b)) c

-- not clear whether to add a general Eq instance, but this is useful sometimes:

-- not clear whether to add a general Ord instance, but this is useful sometimes:

{-
************************************************************************
*                                                                      *
\subsection{Ordering SrcSpans for InteractiveLanguage.Haskell.Utility.}
*                                                                      *
************************************************************************
-}

-- | Determines whether a span is enclosed by another one
isSubspanOf :: SrcSpan -- ^ The span that may be enclosed by the other
            -> SrcSpan -- ^ The span it may be enclosed by
            -> Bool
isSubspanOf src parent
    | srcSpanFileName_maybe parent /= srcSpanFileName_maybe src = False
    | otherwise = srcSpanStart parent <= srcSpanStart src &&
                  srcSpanEnd parent   >= srcSpanEnd src
