{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

{-# LANGUAGE CPP #-}

module VarSet (
        -- * Var, Id and TyVar set types
        VarSet, IdSet, TyVarSet, CoVarSet, TyCoVarSet,

        -- ** Manipulating these sets
        emptyVarSet, unitVarSet, mkVarSet,
        extendVarSet, extendVarSetList, extendVarSet_C,
        elemVarSet, subVarSet,
        unionVarSet, unionVarSets, mapUnionVarSet,
        intersectVarSet, intersectsVarSet, disjointVarSet,
        isEmptyVarSet, delVarSet, delVarSetList, delVarSetByKey,
        minusVarSet, filterVarSet,
        varSetAny, varSetAll,
        transCloVarSet, fixVarSet,
        lookupVarSet, lookupVarSetByName,
        sizeVarSet, seqVarSet,
        elemVarSetByKey, partitionVarSet,
        pluralVarSet, pprVarSet,

        -- * Deterministic Var set types
        DVarSet, DIdSet, DTyVarSet, DTyCoVarSet,

        -- ** Manipulating these sets
        emptyDVarSet, unitDVarSet, mkDVarSet,
        extendDVarSet, extendDVarSetList,
        elemDVarSet, dVarSetElems, subDVarSet,
        unionDVarSet, unionDVarSets, mapUnionDVarSet,
        intersectDVarSet, intersectsDVarSet, disjointDVarSet,
        isEmptyDVarSet, delDVarSet, delDVarSetList,
        minusDVarSet, foldDVarSet, filterDVarSet,
        dVarSetMinusVarSet,
        transCloDVarSet,
        sizeDVarSet, seqDVarSet,
        partitionDVarSet,
        dVarSetToVarSet,
    ) where

#include "HsVersions.h"

import Var      ( Var, TyVar, CoVar, TyCoVar, Id )
import U.Unique
import Name     ( Name )
import U.UniqSet
import U.UniqDSet
import U.UniqFM( disjointUFM, pluralUFM, pprUFM )
import U.UniqDFM( disjointUDFM, udfmToUfm )
import U.Outputable (SDoc)

-- | A non-deterministic set of variables.
-- See Note [Deterministic UniqFM] in UniqDFM for explanation why it's not
-- deterministic and why it matters. Use DVarSet if the set eventually
-- gets converted into a list or folded over in a way where the order
-- changes the generated code, for example when abstracting variables.
type VarSet       = UniqSet Var
type IdSet        = UniqSet Id
type TyVarSet     = UniqSet TyVar
type CoVarSet     = UniqSet CoVar
type TyCoVarSet   = UniqSet TyCoVar

emptyVarSet     :: VarSet
intersectVarSet :: VarSet -> VarSet -> VarSet
unionVarSet     :: VarSet -> VarSet -> VarSet
unionVarSets    :: [VarSet] -> VarSet

mapUnionVarSet  :: (a -> VarSet) -> [a] -> VarSet
-- ^ map the function over the list, and union the results

unitVarSet      :: Var -> VarSet
extendVarSet    :: VarSet -> Var -> VarSet
extendVarSetList:: VarSet -> [Var] -> VarSet
elemVarSet      :: Var -> VarSet -> Bool
delVarSet       :: VarSet -> Var -> VarSet
delVarSetList   :: VarSet -> [Var] -> VarSet
minusVarSet     :: VarSet -> VarSet -> VarSet
isEmptyVarSet   :: VarSet -> Bool
mkVarSet        :: [Var] -> VarSet
lookupVarSet    :: VarSet -> Var -> Maybe Var
                        -- Returns the set element, which may be
                        -- (==) to the argument, but not the same as
lookupVarSetByName :: VarSet -> Name -> Maybe Var
sizeVarSet      :: VarSet -> Int
filterVarSet    :: (Var -> Bool) -> VarSet -> VarSet
extendVarSet_C  :: (Var->Var->Var) -> VarSet -> Var -> VarSet

delVarSetByKey  :: VarSet -> Unique -> VarSet
elemVarSetByKey :: Unique -> VarSet -> Bool
partitionVarSet :: (Var -> Bool) -> VarSet -> (VarSet, VarSet)

emptyVarSet     = emptyUniqSet
unitVarSet      = unitUniqSet
extendVarSet    = addOneToUniqSet
extendVarSetList= addListToUniqSet
intersectVarSet = intersectUniqSets

intersectsVarSet:: VarSet -> VarSet -> Bool     -- True if non-empty intersection
disjointVarSet  :: VarSet -> VarSet -> Bool     -- True if empty intersection
subVarSet       :: VarSet -> VarSet -> Bool     -- True if first arg is subset of second
        -- (s1 `intersectsVarSet` s2) doesn't compute s2 if s1 is empty;
        -- ditto disjointVarSet, subVarSet

unionVarSet     = unionUniqSets
unionVarSets    = unionManyUniqSets
elemVarSet      = elementOfUniqSet
minusVarSet     = minusUniqSet
delVarSet       = delOneFromUniqSet
delVarSetList   = delListFromUniqSet
isEmptyVarSet   = isEmptyUniqSet
mkVarSet        = mkUniqSet
lookupVarSet    = lookupUniqSet
lookupVarSetByName = lookupUniqSet
sizeVarSet      = sizeUniqSet
filterVarSet    = filterUniqSet
extendVarSet_C  = addOneToUniqSet_C
delVarSetByKey  = delOneFromUniqSet_Directly
elemVarSetByKey = elemUniqSet_Directly
partitionVarSet = partitionUniqSet

mapUnionVarSet get_set xs = foldr (unionVarSet . get_set) emptyVarSet xs

-- See comments with type signatures
intersectsVarSet s1 s2 = not (s1 `disjointVarSet` s2)
disjointVarSet   s1 s2 = disjointUFM s1 s2
subVarSet        s1 s2 = isEmptyVarSet (s1 `minusVarSet` s2)

varSetAny :: (Var -> Bool) -> VarSet -> Bool
varSetAny = uniqSetAny

varSetAll :: (Var -> Bool) -> VarSet -> Bool
varSetAll = uniqSetAll

-- There used to exist mapVarSet, see Note [Unsound mapUniqSet] in UniqSet for
-- why it got removed.

fixVarSet :: (VarSet -> VarSet)   -- Map the current set to a new set
          -> VarSet -> VarSet
-- (fixVarSet f s) repeatedly applies f to the set s,
-- until it reaches a fixed point.
fixVarSet fn vars
  | new_vars `subVarSet` vars = vars
  | otherwise                 = fixVarSet fn new_vars
  where
    new_vars = fn vars

transCloVarSet :: (VarSet -> VarSet)
                  -- Map some variables in the set to
                  -- extra variables that should be in it
               -> VarSet -> VarSet
-- (transCloVarSet f s) repeatedly applies f to new candidates, adding any
-- new variables to s that it finds thereby, until it reaches a fixed point.
--
-- The function fn could be (Var -> VarSet), but we use (VarSet -> VarSet)
-- for efficiency, so that the test can be batched up.
-- It's essential that fn will work fine if given new candidates
-- one at at time; ie  fn {v1,v2} = fn v1 `union` fn v2
-- Use fixVarSet if the function needs to see the whole set all at once
transCloVarSet fn seeds
  = go seeds seeds
  where
    go :: VarSet  -- Accumulating result
       -> VarSet  -- Work-list; un-processed subset of accumulating result
       -> VarSet
    -- Specification: go acc vs = acc `union` transClo fn vs

    go acc candidates
       | isEmptyVarSet new_vs = acc
       | otherwise            = go (acc `unionVarSet` new_vs) new_vs
       where
         new_vs = fn candidates `minusVarSet` acc

seqVarSet :: VarSet -> ()
seqVarSet s = sizeVarSet s `seq` ()

-- | Determines the pluralisation suffix appropriate for the length of a set
-- in the same way that plural from Outputable does for lists.
pluralVarSet :: VarSet -> SDoc
pluralVarSet = pluralUFM

-- | Pretty-print a non-deterministic set.
-- The order of variables is non-deterministic and for pretty-printing that
-- shouldn't be a problem.
-- Having this function helps contain the non-determinism created with
-- nonDetEltsUFM.
-- Passing a list to the pretty-printing function allows the caller
-- to decide on the order of Vars (eg. toposort them) without them having
-- to use nonDetEltsUFM at the call site. This prevents from let-binding
-- non-deterministically ordered lists and reusing them where determinism
-- matters.
pprVarSet :: VarSet          -- ^ The things to be pretty printed
          -> ([Var] -> SDoc) -- ^ The pretty printing function to use on the
                             -- elements
          -> SDoc            -- ^ 'SDoc' where the things have been pretty
                             -- printed
pprVarSet = pprUFM

-- Deterministic VarSet
-- See Note [Deterministic UniqFM] in UniqDFM for explanation why we need
-- DVarSet.

type DVarSet     = UniqDSet Var
type DIdSet      = UniqDSet Id
type DTyVarSet   = UniqDSet TyVar
type DTyCoVarSet = UniqDSet TyCoVar

emptyDVarSet :: DVarSet
emptyDVarSet = emptyUniqDSet

unitDVarSet :: Var -> DVarSet
unitDVarSet = unitUniqDSet

mkDVarSet :: [Var] -> DVarSet
mkDVarSet = mkUniqDSet

extendDVarSet :: DVarSet -> Var -> DVarSet
extendDVarSet = addOneToUniqDSet

elemDVarSet :: Var -> DVarSet -> Bool
elemDVarSet = elementOfUniqDSet

dVarSetElems :: DVarSet -> [Var]
dVarSetElems = uniqDSetToList

subDVarSet :: DVarSet -> DVarSet -> Bool
subDVarSet s1 s2 = isEmptyDVarSet (s1 `minusDVarSet` s2)

unionDVarSet :: DVarSet -> DVarSet -> DVarSet
unionDVarSet = unionUniqDSets

unionDVarSets :: [DVarSet] -> DVarSet
unionDVarSets = unionManyUniqDSets

-- | Map the function over the list, and union the results
mapUnionDVarSet  :: (a -> DVarSet) -> [a] -> DVarSet
mapUnionDVarSet get_set xs = foldr (unionDVarSet . get_set) emptyDVarSet xs

intersectDVarSet :: DVarSet -> DVarSet -> DVarSet
intersectDVarSet = intersectUniqDSets

-- | True if empty intersection
disjointDVarSet :: DVarSet -> DVarSet -> Bool
disjointDVarSet s1 s2 = disjointUDFM s1 s2

-- | True if non-empty intersection
intersectsDVarSet :: DVarSet -> DVarSet -> Bool
intersectsDVarSet s1 s2 = not (s1 `disjointDVarSet` s2)

isEmptyDVarSet :: DVarSet -> Bool
isEmptyDVarSet = isEmptyUniqDSet

delDVarSet :: DVarSet -> Var -> DVarSet
delDVarSet = delOneFromUniqDSet

minusDVarSet :: DVarSet -> DVarSet -> DVarSet
minusDVarSet = minusUniqDSet

dVarSetMinusVarSet :: DVarSet -> VarSet -> DVarSet
dVarSetMinusVarSet = uniqDSetMinusUniqSet

foldDVarSet :: (Var -> a -> a) -> a -> DVarSet -> a
foldDVarSet = foldUniqDSet

filterDVarSet :: (Var -> Bool) -> DVarSet -> DVarSet
filterDVarSet = filterUniqDSet

sizeDVarSet :: DVarSet -> Int
sizeDVarSet = sizeUniqDSet

-- | Partition DVarSet according to the predicate given
partitionDVarSet :: (Var -> Bool) -> DVarSet -> (DVarSet, DVarSet)
partitionDVarSet = partitionUniqDSet

-- | Delete a list of variables from DVarSet
delDVarSetList :: DVarSet -> [Var] -> DVarSet
delDVarSetList = delListFromUniqDSet

seqDVarSet :: DVarSet -> ()
seqDVarSet s = sizeDVarSet s `seq` ()

-- | Add a list of variables to DVarSet
extendDVarSetList :: DVarSet -> [Var] -> DVarSet
extendDVarSetList = addListToUniqDSet

-- | Convert a DVarSet to a VarSet by forgeting the order of insertion
dVarSetToVarSet :: DVarSet -> VarSet
dVarSetToVarSet = udfmToUfm

-- | transCloVarSet for DVarSet
transCloDVarSet :: (DVarSet -> DVarSet)
                  -- Map some variables in the set to
                  -- extra variables that should be in it
                -> DVarSet -> DVarSet
-- (transCloDVarSet f s) repeatedly applies f to new candidates, adding any
-- new variables to s that it finds thereby, until it reaches a fixed point.
--
-- The function fn could be (Var -> DVarSet), but we use (DVarSet -> DVarSet)
-- for efficiency, so that the test can be batched up.
-- It's essential that fn will work fine if given new candidates
-- one at at time; ie  fn {v1,v2} = fn v1 `union` fn v2
transCloDVarSet fn seeds
  = go seeds seeds
  where
    go :: DVarSet  -- Accumulating result
       -> DVarSet  -- Work-list; un-processed subset of accumulating result
       -> DVarSet
    -- Specification: go acc vs = acc `union` transClo fn vs

    go acc candidates
       | isEmptyDVarSet new_vs = acc
       | otherwise            = go (acc `unionDVarSet` new_vs) new_vs
       where
         new_vs = fn candidates `minusDVarSet` acc
