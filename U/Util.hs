-- (c) The University of Glasgow 2006

{-# LANGUAGE CPP #-}

-- | Highly random utility functions
--
module U.Util (abstractConstr,snocView,thenCmp,isSingleton,partitionWith,
               dropWhileEndLE,debugIsOn,dropTail,readRational,fuzzyLookup,
               mapFst,takeList,looksLikePackageName) {-
        -- * Flags dependent on the compiler build
        ghciSupported, debugIsOn, ncgDebugIsOn,
        ghciTablesNextToCode,
        isWindowsHost, isDarwinHost,

        -- * General list processing
        zipEqual, zipWithEqual, zipWith3Equal, zipWith4Equal,
        zipLazy, stretchZipWith, zipWithAndUnzip,

        zipWithLazy, zipWith3Lazy,

        filterByList, filterByLists, partitionByList,

        unzipWith,

        mapFst, mapSnd, chkAppend,
        mapAndUnzip, mapAndUnzip3, mapAccumL2,
        nOfThem, filterOut, partitionWith, splitEithers,

        dropWhileEndLE, spanEnd,

        foldl1', foldl2, count, all2,

        lengthExceeds, lengthIs, lengthAtLeast,
        listLengthCmp, atLength,
        equalLength, compareLength, leLength,

        isSingleton, only, singleton,
        notNull, ,

        isIn, isn'tIn,

        chunkList,

        -- * Tuples
        fstOf3, sndOf3, thdOf3,
        firstM, first3M,
        fst3, snd3, third3,
        uncurry3,
        liftFst, liftSnd,

        -- * List operations controlled by another list
        takeList, dropList, splitAtList, split,
        dropTail,

        -- * For loop
        nTimes,

        -- * Sorting
        sortWith, minWith, nubSort,

        -- * Comparisons
        isEqual, eqListBy, eqMaybeBy,
        thenCmp, cmpList,
        removeSpaces,
        (<&&>), (<||>),

        -- * Edit distance
        fuzzyMatch, fuzzyLookup,

        -- * Transitive closures
        transitiveClosure,

        -- * Strictness
        seqList,

        -- * Module names
        looksLikeModuleName,
        looksLikePackageName,

        -- * Argument processing
        getCmd, toCmdArgs, toArgs,

        -- * Floating point
        readRational,

        -- * read helpers
        maybeRead, maybeReadFuzzy,

        -- * IO-ish utilities
        hSetTranslit,

        global, consIORef, globalM,

        -- * Filenames and paths
        Suffix,
        splitLongestPrefix,
        escapeSpaces,
        Direction(..), reslash,
        makeRelativeTo,

        -- * Utils for defining Data instances
         abstractDataType, mkNoRepType,

        -- * Utils for printing C code
        charToC,

        -- * Hashing
        hashString,
    )  -} where

#include "HsVersions.h"

import U.Panic

import Data.Data
import Data.List        hiding (group)
import Control.Applicative ( liftA2 )

import Data.Char        ( isAlphaNum, ord, isDigit )
import Data.Ratio       ( (%) )
import Data.Ord         ( comparing )
import Data.Bits
import Data.Word
import qualified Data.IntMap as IM

infixr 9 `thenCmp`

{-
************************************************************************
*                                                                      *
\subsection{Is DEBUG on, are we on Windows, etc?}
*                                                                      *
************************************************************************

These booleans are global constants, set by CPP flags.  They allow us to
recompile a single module (this one) to change whether or not debug output
appears. They sometimes let us avoid even running CPP elsewhere.

It's important that the flags are literal constants (True/False). Then,
with -0, tests of the flags in other modules will simplify to the correct
branch of the conditional, thereby dropping debug code altogether when
the flags are off.
-}
debugIsOn :: Bool
#ifdef DEBUG
debugIsOn = True
#else
debugIsOn = False
#endif

partitionWith :: (a -> Either b c) -> [a] -> ([b], [c])
-- ^ Uses a function to determine which of two output lists an input element should join
partitionWith _ [] = ([],[])
partitionWith f (x:xs) = case f x of
                         Left  b -> (b:bs, cs)
                         Right c -> (bs, c:cs)
    where (bs,cs) = partitionWith f xs

mapFst :: (a->c) -> [(a,b)] -> [(c,b)]
mapFst f xys = [(f x, y) | (x,y) <- xys]

isSingleton :: [a] -> Bool
isSingleton [_] = True
isSingleton _   = False

{-
************************************************************************
*                                                                      *
\subsection[Utils-accum]{Accumulating}
*                                                                      *
************************************************************************

A combination of foldl with zip.  It works with equal length lists.
-}

{-
@splitAt@, @take@, and @drop@ but with length of another
list giving the break-off point:
-}

takeList :: [b] -> [a] -> [a]
-- (takeList as bs) trims bs to the be same length
-- as as, unless as is longer in which case it's a no-op
takeList [] _ = []
takeList (_:xs) ls =
   case ls of
     [] -> []
     (y:ys) -> y : takeList xs ys

-- drop from the end of a list
dropTail :: Int -> [a] -> [a]
-- Specification: dropTail n = reverse . drop n . reverse
-- Better implemention due to Joachim Breitner
-- http://www.joachim-breitner.de/blog/archives/600-On-taking-the-last-n-elements-of-a-list.html
dropTail n xs
  = go (drop n xs) xs
  where
    go (_:ys) (x:xs) = x : go ys xs
    go _      _      = []  -- Stop when ys runs out
                           -- It'll always run out before xs does

-- dropWhile from the end of a list. This is similar to Data.List.dropWhileEnd,
-- but is lazy in the elements and strict in the spine. For reasonably short lists,
-- such as path names and typical lines of text, dropWhileEndLE is generally
-- faster than dropWhileEnd. Its advantage is magnified when the predicate is
-- expensive--using dropWhileEndLE isSpace to strip the space off a line of text
-- is generally much faster than using dropWhileEnd isSpace for that purpose.
-- Specification: dropWhileEndLE p = reverse . dropWhile p . reverse
-- Pay attention to the short-circuit (&&)! The order of its arguments is the only
-- difference between dropWhileEnd and dropWhileEndLE.
dropWhileEndLE :: (a -> Bool) -> [a] -> [a]
dropWhileEndLE p = foldr (\x r -> if null r && p x then [] else x:r) []


snocView :: [a] -> Maybe ([a],a)
        -- Split off the last element
snocView [] = Nothing
snocView xs = go [] xs
            where
                -- Invariant: second arg is non-empty
              go acc [x]    = Just (reverse acc, x)
              go acc (x:xs) = go (x:acc) xs
              go _   []     = panic "Util: snocView"

split :: Char -> String -> [String]
split c s = case rest of
                []     -> [chunk]
                _:rest -> chunk : split c rest
  where (chunk, rest) = break (==c) s

thenCmp :: Ordering -> Ordering -> Ordering
{-# INLINE thenCmp #-}
thenCmp EQ       ordering = ordering
thenCmp ordering _        = ordering

-- Boolean operators lifted to Applicative
(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)
infixr 3 <&&> -- same as (&&)

{-
************************************************************************
*                                                                      *
\subsection{Edit distance}
*                                                                      *
************************************************************************
-}

-- | Find the "restricted" Damerau-Levenshtein edit distance between two strings.
-- See: <http://en.wikipedia.org/wiki/Damerau-Levenshtein_distance>.
-- Based on the algorithm presented in "A Bit-Vector Algorithm for Computing
-- Levenshtein and Damerau Edit Distances" in PSC'02 (Heikki Hyyro).
-- See http://www.cs.uta.fi/~helmu/pubs/psc02.pdf and
--     http://www.cs.uta.fi/~helmu/pubs/PSCerr.html for an explanation
restrictedDamerauLevenshteinDistance :: String -> String -> Int
restrictedDamerauLevenshteinDistance str1 str2
  = restrictedDamerauLevenshteinDistanceWithLengths m n str1 str2
  where
    m = length str1
    n = length str2

restrictedDamerauLevenshteinDistanceWithLengths
  :: Int -> Int -> String -> String -> Int
restrictedDamerauLevenshteinDistanceWithLengths m n str1 str2
  | m <= n
  = if n <= 32 -- n must be larger so this check is sufficient
    then restrictedDamerauLevenshteinDistance' (undefined :: Word32) m n str1 str2
    else restrictedDamerauLevenshteinDistance' (undefined :: Integer) m n str1 str2

  | otherwise
  = if m <= 32 -- m must be larger so this check is sufficient
    then restrictedDamerauLevenshteinDistance' (undefined :: Word32) n m str2 str1
    else restrictedDamerauLevenshteinDistance' (undefined :: Integer) n m str2 str1

restrictedDamerauLevenshteinDistance'
  :: (Bits bv, Num bv) => bv -> Int -> Int -> String -> String -> Int
restrictedDamerauLevenshteinDistance' _bv_dummy m n str1 str2
  | [] <- str1 = n
  | otherwise  = extractAnswer $
                 foldl' (restrictedDamerauLevenshteinDistanceWorker
                             (matchVectors str1) top_bit_mask vector_mask)
                        (0, 0, m_ones, 0, m) str2
  where
    m_ones@vector_mask = (2 ^ m) - 1
    top_bit_mask = (1 `shiftL` (m - 1)) `asTypeOf` _bv_dummy
    extractAnswer (_, _, _, _, distance) = distance

restrictedDamerauLevenshteinDistanceWorker
      :: (Bits bv, Num bv) => IM.IntMap bv -> bv -> bv
      -> (bv, bv, bv, bv, Int) -> Char -> (bv, bv, bv, bv, Int)
restrictedDamerauLevenshteinDistanceWorker str1_mvs top_bit_mask vector_mask
                                           (pm, d0, vp, vn, distance) char2
  = seq str1_mvs $ seq top_bit_mask $ seq vector_mask $
    seq pm' $ seq d0' $ seq vp' $ seq vn' $
    seq distance'' $ seq char2 $
    (pm', d0', vp', vn', distance'')
  where
    pm' = IM.findWithDefault 0 (ord char2) str1_mvs

    d0' = ((((sizedComplement vector_mask d0) .&. pm') `shiftL` 1) .&. pm)
      .|. ((((pm' .&. vp) + vp) .&. vector_mask) `xor` vp) .|. pm' .|. vn
          -- No need to mask the shiftL because of the restricted range of pm

    hp' = vn .|. sizedComplement vector_mask (d0' .|. vp)
    hn' = d0' .&. vp

    hp'_shift = ((hp' `shiftL` 1) .|. 1) .&. vector_mask
    hn'_shift = (hn' `shiftL` 1) .&. vector_mask
    vp' = hn'_shift .|. sizedComplement vector_mask (d0' .|. hp'_shift)
    vn' = d0' .&. hp'_shift

    distance' = if hp' .&. top_bit_mask /= 0 then distance + 1 else distance
    distance'' = if hn' .&. top_bit_mask /= 0 then distance' - 1 else distance'

sizedComplement :: Bits bv => bv -> bv -> bv
sizedComplement vector_mask vect = vector_mask `xor` vect

matchVectors :: (Bits bv, Num bv) => String -> IM.IntMap bv
matchVectors = snd . foldl' go (0 :: Int, IM.empty)
  where
    go (ix, im) char = let ix' = ix + 1
                           im' = IM.insertWith (.|.) (ord char) (2 ^ ix) im
                       in seq ix' $ seq im' $ (ix', im')

{-# SPECIALIZE INLINE restrictedDamerauLevenshteinDistance'
                      :: Word32 -> Int -> Int -> String -> String -> Int #-}
{-# SPECIALIZE INLINE restrictedDamerauLevenshteinDistance'
                      :: Integer -> Int -> Int -> String -> String -> Int #-}

{-# SPECIALIZE restrictedDamerauLevenshteinDistanceWorker
               :: IM.IntMap Word32 -> Word32 -> Word32
               -> (Word32, Word32, Word32, Word32, Int)
               -> Char -> (Word32, Word32, Word32, Word32, Int) #-}
{-# SPECIALIZE restrictedDamerauLevenshteinDistanceWorker
               :: IM.IntMap Integer -> Integer -> Integer
               -> (Integer, Integer, Integer, Integer, Int)
               -> Char -> (Integer, Integer, Integer, Integer, Int) #-}

{-# SPECIALIZE INLINE sizedComplement :: Word32 -> Word32 -> Word32 #-}
{-# SPECIALIZE INLINE sizedComplement :: Integer -> Integer -> Integer #-}

{-# SPECIALIZE matchVectors :: String -> IM.IntMap Word32 #-}
{-# SPECIALIZE matchVectors :: String -> IM.IntMap Integer #-}

-- | Search for possible matches to the users input in the given list,
-- returning a small number of ranked results
fuzzyLookup :: String -> [(String,a)] -> [a]
fuzzyLookup user_entered possibilites
  = map fst $ take mAX_RESULTS $ sortBy (comparing snd)
    [ (poss_val, distance) | (poss_str, poss_val) <- possibilites
                       , let distance = restrictedDamerauLevenshteinDistance
                                            poss_str user_entered
                       , distance <= fuzzy_threshold ]
  where
    -- Work out an approriate match threshold:
    -- We report a candidate if its edit distance is <= the threshold,
    -- The threshhold is set to about a quarter of the # of characters the user entered
    --   Length    Threshold
    --     1         0          -- Don't suggest *any* candidates
    --     2         1          -- for single-char identifiers
    --     3         1
    --     4         1
    --     5         1
    --     6         2
    --
    fuzzy_threshold = truncate $ fromIntegral (length user_entered + 2) / (4 :: Rational)
    mAX_RESULTS = 3

-- Global variables:

-- Module names:


-- Similar to 'parse' for Distribution.Package.PackageName,
-- but we don't want to depend on Cabal.
looksLikePackageName :: String -> Bool
looksLikePackageName = all (all isAlphaNum <&&> not . (all isDigit)) . split '-'

{-
-- -----------------------------------------------------------------------------
-- Floats
-}

readRational__ :: ReadS Rational -- NB: doesn't handle leading "-"
readRational__ r = do
     (n,d,s) <- readFix r
     (k,t)   <- readExp s
     return ((n%1)*10^^(k-d), t)
 where
     readFix r = do
        (ds,s)  <- lexDecDigits r
        (ds',t) <- lexDotDigits s
        return (read (ds++ds'), length ds', t)

     readExp (e:s) | e `elem` "eE" = readExp' s
     readExp s                     = return (0,s)

     readExp' ('+':s) = readDec s
     readExp' ('-':s) = do (k,t) <- readDec s
                           return (-k,t)
     readExp' s       = readDec s

     readDec s = do
        (ds,r) <- nonnull isDigit s
        return (foldl1 (\n d -> n * 10 + d) [ ord d - ord '0' | d <- ds ],
                r)

     lexDecDigits = nonnull isDigit

     lexDotDigits ('.':s) = return (span isDigit s)
     lexDotDigits s       = return ("",s)

     nonnull p s = do (cs@(_:_),t) <- return (span p s)
                      return (cs,t)

readRational :: String -> Rational -- NB: *does* handle a leading "-"
readRational top_s
  = case top_s of
      '-' : xs -> - (read_me xs)
      xs       -> read_me xs
  where
    read_me s
      = case (do { (x,"") <- readRational__ s ; return x }) of
          [x] -> x
          []  -> error ("readRational: no parse:"        ++ top_s)
          _   -> error ("readRational: ambiguous parse:" ++ top_s)

{-
************************************************************************
*                                                                      *
\subsection[Utils-Data]{Utils for defining Data instances}
*                                                                      *
************************************************************************

These functions helps us to define Data instances for abstract types.
-}

abstractConstr :: String -> Constr
abstractConstr n = mkConstr (abstractDataType n) ("{abstract:"++n++"}") [] Prefix

abstractDataType :: String -> DataType
abstractDataType n = mkDataType n [abstractConstr n]
