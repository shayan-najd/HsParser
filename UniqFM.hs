{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1994-1998


UniqFM: Specialised finite maps, for things with @Uniques@.

Basically, the things need to be in class @Uniquable@, and we use the
@getUnique@ method to grab their @Uniques@.

(A similar thing to @UniqSet@, as opposed to @Set@.)

The interface is based on @FiniteMap@s, but the implementation uses
@Data.IntMap@, which is both maintained and faster than the past
implementation (see commit log).

The @UniqFM@ interface maps directly to Data.IntMap, only
``Data.IntMap.union'' is left-biased and ``plusUFM'' right-biased
and ``addToUFM\_C'' and ``Data.IntMap.insertWith'' differ in the order
of arguments of combining function.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall #-}

module UniqFM (
        -- * Unique-keyed mappings
        UniqFM,       -- abstract type

        -- ** Manipulating those mappings
        emptyUFM,
        unitUFM,
        unitDirectlyUFM,
        listToUFM,
        listToUFM_Directly,
        listToUFM_C,
        addToUFM,addToUFM_C,addToUFM_Acc,
        addListToUFM,addListToUFM_C,
        addToUFM_Directly,
        addListToUFM_Directly,
        adjustUFM, alterUFM,
        adjustUFM_Directly,
        delFromUFM,
        delFromUFM_Directly,
        delListFromUFM,
        delListFromUFM_Directly,
        plusUFM,
        plusUFM_C,
        plusUFM_CD,
        minusUFM,
        intersectUFM,
        intersectUFM_C,
        disjointUFM,
        nonDetFoldUFM, foldUFM, nonDetFoldUFM_Directly,
        anyUFM, allUFM,
        mapUFM, mapUFM_Directly,
        elemUFM, elemUFM_Directly,
        filterUFM, filterUFM_Directly, partitionUFM,
        sizeUFM,
        isNullUFM,
        lookupUFM, lookupUFM_Directly,
        lookupWithDefaultUFM, lookupWithDefaultUFM_Directly,
        nonDetEltsUFM, eltsUFM, nonDetKeysUFM,
        ufmToSet_Directly,
        nonDetUFMToList, ufmToList, ufmToIntMap,
        pprUniqFM, pprUFM, pluralUFM
    ) where

import Unique           ( Uniquable(..), Unique, getKey )
import Outputable

import qualified Data.IntMap as M
import qualified Data.IntSet as S
import Data.Typeable
import Data.Data
#if __GLASGOW_HASKELL__ > 710
import Data.Semigroup   ( Semigroup )
import qualified Data.Semigroup as Semigroup
#endif

{-
************************************************************************
*                                                                      *
\subsection{The signature of the module}
*                                                                      *
************************************************************************
-}

emptyUFM        :: UniqFM elt
isNullUFM       :: UniqFM elt -> Bool
unitUFM         :: Uniquable key => key -> elt -> UniqFM elt
unitDirectlyUFM -- got the Unique already
                :: Unique -> elt -> UniqFM elt
listToUFM       :: Uniquable key => [(key,elt)] -> UniqFM elt
listToUFM_Directly
                :: [(Unique, elt)] -> UniqFM elt
listToUFM_C     :: Uniquable key => (elt -> elt -> elt)
                           -> [(key, elt)]
                           -> UniqFM elt

addToUFM        :: Uniquable key => UniqFM elt -> key -> elt  -> UniqFM elt
addListToUFM    :: Uniquable key => UniqFM elt -> [(key,elt)] -> UniqFM elt
addListToUFM_Directly :: UniqFM elt -> [(Unique,elt)] -> UniqFM elt
addToUFM_Directly
                :: UniqFM elt -> Unique -> elt -> UniqFM elt

addToUFM_C      :: Uniquable key => (elt -> elt -> elt) -- old -> new -> result
                           -> UniqFM elt                -- old
                           -> key -> elt                -- new
                           -> UniqFM elt                -- result

addToUFM_Acc    :: Uniquable key =>
                              (elt -> elts -> elts)     -- Add to existing
                           -> (elt -> elts)             -- New element
                           -> UniqFM elts               -- old
                           -> key -> elt                -- new
                           -> UniqFM elts               -- result

alterUFM        :: Uniquable key =>
                              (Maybe elt -> Maybe elt)  -- How to adjust
                           -> UniqFM elt                -- old
                           -> key                       -- new
                           -> UniqFM elt                -- result

addListToUFM_C  :: Uniquable key => (elt -> elt -> elt)
                           -> UniqFM elt -> [(key,elt)]
                           -> UniqFM elt

adjustUFM       :: Uniquable key => (elt -> elt) -> UniqFM elt -> key -> UniqFM elt
adjustUFM_Directly :: (elt -> elt) -> UniqFM elt -> Unique -> UniqFM elt

delFromUFM      :: Uniquable key => UniqFM elt -> key    -> UniqFM elt
delListFromUFM  :: Uniquable key => UniqFM elt -> [key] -> UniqFM elt
delListFromUFM_Directly :: UniqFM elt -> [Unique] -> UniqFM elt
delFromUFM_Directly :: UniqFM elt -> Unique -> UniqFM elt

-- Bindings in right argument shadow those in the left
plusUFM         :: UniqFM elt -> UniqFM elt -> UniqFM elt

plusUFM_C       :: (elt -> elt -> elt)
                -> UniqFM elt -> UniqFM elt -> UniqFM elt

-- | `plusUFM_CD f m1 d1 m2 d2` merges the maps using `f` as the
-- combinding function and `d1` resp. `d2` as the default value if
-- there is no entry in `m1` reps. `m2`. The domain is the union of
-- the domains of `m1` and `m2`.
--
-- Representative example:
--
-- @
-- plusUFM_CD f {A: 1, B: 2} 23 {B: 3, C: 4} 42
--    == {A: f 1 42, B: f 2 3, C: f 23 4 }
-- @
plusUFM_CD      :: (elt -> elt -> elt)
                -> UniqFM elt -> elt -> UniqFM elt -> elt -> UniqFM elt

minusUFM        :: UniqFM elt1 -> UniqFM elt2 -> UniqFM elt1

intersectUFM    :: UniqFM elt -> UniqFM elt -> UniqFM elt
intersectUFM_C  :: (elt1 -> elt2 -> elt3)
                -> UniqFM elt1 -> UniqFM elt2 -> UniqFM elt3
disjointUFM     :: UniqFM elt1 -> UniqFM elt2 -> Bool

foldUFM         :: (elt -> a -> a) -> a -> UniqFM elt -> a
mapUFM          :: (elt1 -> elt2) -> UniqFM elt1 -> UniqFM elt2
mapUFM_Directly :: (Unique -> elt1 -> elt2) -> UniqFM elt1 -> UniqFM elt2
filterUFM       :: (elt -> Bool) -> UniqFM elt -> UniqFM elt
filterUFM_Directly :: (Unique -> elt -> Bool) -> UniqFM elt -> UniqFM elt
partitionUFM    :: (elt -> Bool) -> UniqFM elt -> (UniqFM elt, UniqFM elt)

sizeUFM         :: UniqFM elt -> Int
--hashUFM               :: UniqFM elt -> Int
elemUFM         :: Uniquable key => key -> UniqFM elt -> Bool
elemUFM_Directly:: Unique -> UniqFM elt -> Bool

lookupUFM       :: Uniquable key => UniqFM elt -> key -> Maybe elt
lookupUFM_Directly  -- when you've got the Unique already
                :: UniqFM elt -> Unique -> Maybe elt
lookupWithDefaultUFM
                :: Uniquable key => UniqFM elt -> elt -> key -> elt
lookupWithDefaultUFM_Directly
                :: UniqFM elt -> elt -> Unique -> elt
eltsUFM         :: UniqFM elt -> [elt]
ufmToSet_Directly :: UniqFM elt -> S.IntSet
ufmToList       :: UniqFM elt -> [(Unique, elt)]

{-
************************************************************************
*                                                                      *
\subsection{Monoid interface}
*                                                                      *
************************************************************************
-}

#if __GLASGOW_HASKELL__ > 710
instance Semigroup (UniqFM a) where
  (<>) = plusUFM
#endif

instance Monoid (UniqFM a) where
    mempty = emptyUFM
    mappend = plusUFM

{-
************************************************************************
*                                                                      *
\subsection{Implementation using ``Data.IntMap''}
*                                                                      *
************************************************************************
-}


newtype UniqFM ele = UFM (M.IntMap ele)
  deriving (Data, Eq, Functor, Typeable)
  -- We used to derive Traversable and Foldable, but they were nondeterministic
  -- and not obvious at the call site. You can use explicit nonDetEltsUFM
  -- and fold a list if needed.
  -- See Note [Deterministic UniqFM] in UniqDFM to learn about determinism.

emptyUFM = UFM M.empty
isNullUFM (UFM m) = M.null m
unitUFM k v = UFM (M.singleton (getKey $ getUnique k) v)
unitDirectlyUFM u v = UFM (M.singleton (getKey u) v)
listToUFM = foldl (\m (k, v) -> addToUFM m k v) emptyUFM
listToUFM_Directly = foldl (\m (u, v) -> addToUFM_Directly m u v) emptyUFM
listToUFM_C f = foldl (\m (k, v) -> addToUFM_C f m k v) emptyUFM

alterUFM f (UFM m) k = UFM (M.alter f (getKey $ getUnique k) m)
addToUFM (UFM m) k v   = UFM (M.insert (getKey $ getUnique k) v m)
addListToUFM = foldl (\m (k, v) -> addToUFM m k v)
addListToUFM_Directly = foldl (\m (k, v) -> addToUFM_Directly m k v)
addToUFM_Directly (UFM m) u v = UFM (M.insert (getKey u) v m)

-- Arguments of combining function of M.insertWith and addToUFM_C are flipped.
addToUFM_C f (UFM m) k v =
  UFM (M.insertWith (flip f) (getKey $ getUnique k) v m)
addToUFM_Acc exi new (UFM m) k v =
  UFM (M.insertWith (\_new old -> exi v old) (getKey $ getUnique k) (new v) m)
addListToUFM_C f = foldl (\m (k, v) -> addToUFM_C f m k v)

adjustUFM f (UFM m) k = UFM (M.adjust f (getKey $ getUnique k) m)
adjustUFM_Directly f (UFM m) u = UFM (M.adjust f (getKey u) m)

delFromUFM (UFM m) k = UFM (M.delete (getKey $ getUnique k) m)
delListFromUFM = foldl delFromUFM
delFromUFM_Directly (UFM m) u = UFM (M.delete (getKey u) m)
delListFromUFM_Directly = foldl delFromUFM_Directly

-- M.union is left-biased, plusUFM should be right-biased.
plusUFM (UFM x) (UFM y) = UFM (M.union y x)
     -- Note (M.union y x), with arguments flipped
     -- M.union is left-biased, plusUFM should be right-biased.

plusUFM_C f (UFM x) (UFM y) = UFM (M.unionWith f x y)

plusUFM_CD f (UFM xm) dx (UFM ym) dy
    = UFM $ M.mergeWithKey
        (\_ x y -> Just (x `f` y))
        (M.map (\x -> x `f` dy))
        (M.map (\y -> dx `f` y))
        xm ym
minusUFM (UFM x) (UFM y) = UFM (M.difference x y)
intersectUFM (UFM x) (UFM y) = UFM (M.intersection x y)
intersectUFM_C f (UFM x) (UFM y) = UFM (M.intersectionWith f x y)
disjointUFM (UFM x) (UFM y) = M.null (M.intersection x y)

foldUFM k z (UFM m) = M.fold k z m


mapUFM f (UFM m) = UFM (M.map f m)
mapUFM_Directly f (UFM m) = UFM (M.mapWithKey (f . getUnique) m)
filterUFM p (UFM m) = UFM (M.filter p m)
filterUFM_Directly p (UFM m) = UFM (M.filterWithKey (p . getUnique) m)
partitionUFM p (UFM m) = case M.partition p m of
                           (left, right) -> (UFM left, UFM right)

sizeUFM (UFM m) = M.size m
elemUFM k (UFM m) = M.member (getKey $ getUnique k) m
elemUFM_Directly u (UFM m) = M.member (getKey u) m

lookupUFM (UFM m) k = M.lookup (getKey $ getUnique k) m
lookupUFM_Directly (UFM m) u = M.lookup (getKey u) m
lookupWithDefaultUFM (UFM m) v k = M.findWithDefault v (getKey $ getUnique k) m
lookupWithDefaultUFM_Directly (UFM m) v u = M.findWithDefault v (getKey u) m
eltsUFM (UFM m) = M.elems m
ufmToSet_Directly (UFM m) = M.keysSet m
ufmToList (UFM m) = map (\(k, v) -> (getUnique k, v)) $ M.toList m

anyUFM :: (elt -> Bool) -> UniqFM elt -> Bool
anyUFM p (UFM m) = M.fold ((||) . p) False m

allUFM :: (elt -> Bool) -> UniqFM elt -> Bool
allUFM p (UFM m) = M.fold ((&&) . p) True m

-- See Note [Deterministic UniqFM] to learn about nondeterminism.
-- If you use this please provide a justification why it doesn't introduce
-- nondeterminism.
nonDetEltsUFM :: UniqFM elt -> [elt]
nonDetEltsUFM (UFM m) = M.elems m

-- See Note [Deterministic UniqFM] to learn about nondeterminism.
-- If you use this please provide a justification why it doesn't introduce
-- nondeterminism.
nonDetKeysUFM :: UniqFM elt -> [Unique]
nonDetKeysUFM (UFM m) = map getUnique $ M.keys m

-- See Note [Deterministic UniqFM] to learn about nondeterminism.
-- If you use this please provide a justification why it doesn't introduce
-- nondeterminism.
nonDetFoldUFM :: (elt -> a -> a) -> a -> UniqFM elt -> a
nonDetFoldUFM k z (UFM m) = M.fold k z m

-- See Note [Deterministic UniqFM] to learn about nondeterminism.
-- If you use this please provide a justification why it doesn't introduce
-- nondeterminism.
nonDetFoldUFM_Directly:: (Unique -> elt -> a -> a) -> a -> UniqFM elt -> a
nonDetFoldUFM_Directly k z (UFM m) = M.foldWithKey (k . getUnique) z m

-- See Note [Deterministic UniqFM] to learn about nondeterminism.
-- If you use this please provide a justification why it doesn't introduce
-- nondeterminism.
nonDetUFMToList :: UniqFM elt -> [(Unique, elt)]
nonDetUFMToList (UFM m) = map (\(k, v) -> (getUnique k, v)) $ M.toList m

ufmToIntMap :: UniqFM elt -> M.IntMap elt
ufmToIntMap (UFM m) = m

{-
************************************************************************
*                                                                      *
\subsection{Output-ery}
*                                                                      *
************************************************************************
-}

instance Outputable a => Outputable (UniqFM a) where
    ppr ufm = pprUniqFM ppr ufm

pprUniqFM :: (a -> SDoc) -> UniqFM a -> SDoc
pprUniqFM ppr_elt ufm
  = brackets $ fsep $ punctuate comma $
    [ ppr uq <+> text ":->" <+> ppr_elt elt
    | (uq, elt) <- nonDetUFMToList ufm ]
  -- It's OK to use nonDetUFMToList here because we only use it for
  -- pretty-printing.

-- | Pretty-print a non-deterministic set.
-- The order of variables is non-deterministic and for pretty-printing that
-- shouldn't be a problem.
-- Having this function helps contain the non-determinism created with
-- nonDetEltsUFM.
pprUFM :: UniqFM a      -- ^ The things to be pretty printed
       -> ([a] -> SDoc) -- ^ The pretty printing function to use on the elements
       -> SDoc          -- ^ 'SDoc' where the things have been pretty
                        -- printed
pprUFM ufm pp = pp (nonDetEltsUFM ufm)

-- | Determines the pluralisation suffix appropriate for the length of a set
-- in the same way that plural from Outputable does for lists.
pluralUFM :: UniqFM a -> SDoc
pluralUFM ufm
  | sizeUFM ufm == 1 = empty
  | otherwise = char 's'
