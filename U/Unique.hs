{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


@Uniques@ are used to distinguish entities in the compiler (@Ids@,
@Classes@, etc.) from each other.  Thus, @Uniques@ are the basic
comparison key in the compiler.

If there is any single operation that needs to be fast, it is @Unique@
comparison.  Unsurprisingly, there is quite a bit of huff-and-puff
directed to that end.

Some of the other hair in this code is to be able to use a
``splittable @UniqueSupply@'' if requested/possible (not standard
Haskell).
-}

{-# LANGUAGE CPP, BangPatterns, MagicHash #-}

module U.Unique (Unique,
                 Uniquable(..),
                 mkVarOccUnique,
                 mkDataOccUnique,
                 mkTvOccUnique,
                 mkTcOccUnique,
                 hasKey,
                 getKey,
                 pprUnique,
                 mkPreludeTyConUnique,
                 mkUniqueGrimily) where

#include "HsVersions.h"

import U.FastString
import U.Outputable

-- just for implementing a fast [0,61) -> Char function
import GHC.Exts (indexCharOffAddr#, Char(..), Int(..))

import Data.Char        ( chr, ord )
import Data.Bits

{-
************************************************************************
*                                                                      *
\subsection[Unique-type]{@Unique@ type and operations}
*                                                                      *
************************************************************************

The @Chars@ are ``tag letters'' that identify the @UniqueSupply@.
Fast comparison is everything on @Uniques@:
-}

--why not newtype Int?

-- | The type of unique identifiers that are used in many places in GHC
-- for fast ordering and equality tests. You should generate these with
-- the functions from the 'UniqSupply' module
data Unique = MkUnique {-# UNPACK #-} !Int

{-
Now come the functions which construct uniques from their pieces, and vice versa.
The stuff about unique *supplies* is handled further down this module.
-}

unpkUnique      :: Unique -> (Char, Int)        -- The reverse

mkUniqueGrimily :: Int -> Unique                -- A trap-door for UniqSupply
getKey          :: Unique -> Int                -- for Var




mkUniqueGrimily = MkUnique

{-# INLINE getKey #-}
getKey (MkUnique x) = x


-- pop the Char in the top 8 bits of the Unique(Supply)

-- No 64-bit bugs here, as long as we have at least 32 bits. --JSM

-- and as long as the Char fits in 8 bits, which we assume anyway!

mkUnique :: Char -> Int -> Unique       -- Builds a unique from pieces
-- NOT EXPORTED, so that we can see all the Chars that
--               are used in this one module
mkUnique c i
  = MkUnique (tag .|. bits)
  where
    tag  = ord c `shiftL` 24
    bits = i .&. 16777215 {-``0x00ffffff''-}

unpkUnique (MkUnique u)
  = let
        -- as long as the Char may have its eighth bit set, we
        -- really do need the logical right-shift here!
        tag = chr (u `shiftR` 24)
        i   = u .&. 16777215 {-``0x00ffffff''-}
    in
    (tag, i)

{-
************************************************************************
*                                                                      *
\subsection[Uniquable-class]{The @Uniquable@ class}
*                                                                      *
************************************************************************
-}

-- | Class of things that we can obtain a 'Unique' from
class Uniquable a where
    getUnique :: a -> Unique

hasKey          :: Uniquable a => a -> Unique -> Bool
x `hasKey` k    = getUnique x == k

instance Uniquable FastString where
 getUnique fs = mkUniqueGrimily (uniqueOfFS fs)

instance Uniquable Int where
 getUnique i = mkUniqueGrimily i

{-
************************************************************************
*                                                                      *
\subsection[Unique-instances]{Instance declarations for @Unique@}
*                                                                      *
************************************************************************

And the whole point (besides uniqueness) is fast equality.  We don't
use `deriving' because we want {\em precise} control of ordering
(equality on @Uniques@ is v common).
-}

-- Note [Unique Determinism]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
-- The order of allocated @Uniques@ is not stable across rebuilds.
-- The main reason for that is that typechecking interface files pulls
-- @Uniques@ from @UniqSupply@ and the interface file for the module being
-- currently compiled can, but doesn't have to exist.
--
-- It gets more complicated if you take into account that the interface
-- files are loaded lazily and that building multiple files at once has to
-- work for any subset of interface files present. When you add parallelism
-- this makes @Uniques@ hopelessly random.
--
-- As such, to get deterministic builds, the order of the allocated
-- @Uniques@ should not affect the final result.
-- see also wiki/DeterministicBuilds

eqUnique, ltUnique, leUnique :: Unique -> Unique -> Bool
eqUnique (MkUnique u1) (MkUnique u2) = u1 == u2
ltUnique (MkUnique u1) (MkUnique u2) = u1 <  u2
leUnique (MkUnique u1) (MkUnique u2) = u1 <= u2

-- Provided here to make it explicit at the call-site that it can
-- introduce non-determinism.
-- See Note [Unique Determinism]
nonDetCmpUnique :: Unique -> Unique -> Ordering
nonDetCmpUnique (MkUnique u1) (MkUnique u2)
  = if u1 == u2 then EQ else if u1 < u2 then LT else GT

instance Eq Unique where
    a == b = eqUnique a b
    a /= b = not (eqUnique a b)

instance Ord Unique where
    a  < b = ltUnique a b
    a <= b = leUnique a b
    a  > b = not (leUnique a b)
    a >= b = not (ltUnique a b)
    compare a b = nonDetCmpUnique a b

-----------------
instance Uniquable Unique where
    getUnique u = u

-- We do sometimes make strings with @Uniques@ in them:

showUnique :: Unique -> String
showUnique uniq
  = case unpkUnique uniq of
      (tag, u) -> finish_show tag u (iToBase62 u)

finish_show :: Char -> Int -> String -> String
finish_show 't' u _pp_u | u < 26
  = -- Special case to make v common tyvars, t1, t2, ...
    -- come out as a, b, ... (shorter, easier to read)
    [chr (ord 'a' + u)]
finish_show tag _ pp_u = tag : pp_u

pprUnique :: Unique -> SDoc
pprUnique u = text (showUnique u)

instance Outputable Unique where
    ppr = pprUnique

instance Show Unique where
    show uniq = showUnique uniq

{-
************************************************************************
*                                                                      *
\subsection[Utils-base62]{Base-62 numbers}
*                                                                      *
************************************************************************

A character-stingy way to read/write numbers (notably Uniques).
The ``62-its'' are \tr{[0-9a-zA-Z]}.  We don't handle negative Ints.
Code stolen from Lennart.
-}

iToBase62 :: Int -> String
iToBase62 n_
  = go n_ ""
  where
    go n cs | n < 62
            = let !c = chooseChar62 n in c : cs
            | otherwise
            = go q (c : cs) where (q, r) = quotRem n 62
                                  !c = chooseChar62 r

    chooseChar62 :: Int -> Char
    {-# INLINE chooseChar62 #-}
    chooseChar62 (I# n) = C# (indexCharOffAddr# chars62 n)
    chars62 = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"#

{-
************************************************************************
*                                                                      *
\subsection[Uniques-prelude]{@Uniques@ for wired-in Prelude things}
*                                                                      *
************************************************************************

Allocation of unique supply characters:
        v,t,u : for renumbering value-, type- and usage- vars.
        B:   builtin
        C-E: pseudo uniques     (used in native-code generator)
        X:   uniques derived by deriveUnique
        _:   unifiable tyvars   (above)
        0-9: prelude things below
             (no numbers left any more..)
        ::   (prelude) parallel array data constructors

        other a-z: lower case chars for unique supplies.  Used so far:

        d       desugarer
        f       AbsC flattener
        g       SimplStg
        n       Native codegen
        r       Hsc name cache
        s       simplifier
-}

mkPreludeTyConUnique   :: Int -> Unique

--------------------------------------------------
-- Wired-in type constructor keys occupy *two* slots:
--    * u: the TyCon itself
--    * u+1: the TyConRepName of the TyCon
mkPreludeTyConUnique i                = mkUnique '3' (2*i)

mkVarOccUnique, mkDataOccUnique, mkTvOccUnique, mkTcOccUnique :: FastString -> Unique
-- See Note [The Unique of an OccName] in OccName
mkVarOccUnique  fs = mkUnique 'i' (uniqueOfFS fs)
mkDataOccUnique fs = mkUnique 'd' (uniqueOfFS fs)
mkTvOccUnique   fs = mkUnique 'v' (uniqueOfFS fs)
mkTcOccUnique   fs = mkUnique 'c' (uniqueOfFS fs)
