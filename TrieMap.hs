{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fwarn-unused-binds #-}
module TrieMap(
   TypeMap, emptyTypeMap, extendTypeMap, lookupTypeMap, foldTypeMap
    ) where

import Name
import Type
import TyCoRep
import Var
import U.UniqDFM
import U.Unique( Unique )
import U.FastString(FastString)

import qualified Data.Map    as Map
import qualified Data.IntMap as IntMap
import VarEnv
import NameEnv
import U.Outputable
import Control.Monad( (>=>) )

{-
This module implements TrieMaps, which are finite mappings
whose key is a structured value like a CoreExpr or Type.

The code is very regular and boilerplate-like, but there is
some neat handling of *binders*.  In effect they are deBruijn
numbered on the fly.

The regular pattern for handling TrieMaps on data structures was first
described (to my knowledge) in Connelly and Morris's 1995 paper "A
generalization of the Trie Data Structure"; there is also an accessible
description of the idea in Okasaki's book "Purely Functional Data
Structures", Section 10.3.2

************************************************************************
*                                                                      *
                   The TrieMap class
*                                                                      *
************************************************************************
-}

type XT a = Maybe a -> Maybe a  -- How to alter a non-existent elt (Nothing)
                                --               or an existing elt (Just)

class TrieMap m where
   type Key m :: *
   emptyTM  :: m a
   lookupTM :: forall b. Key m -> m b -> Maybe b
   alterTM  :: forall b. Key m -> XT b -> m b -> m b
   mapTM    :: (a->b) -> m a -> m b

   foldTM   :: (a -> b -> b) -> m a -> b -> b
      -- The unusual argument order here makes
      -- it easy to compose calls to foldTM;
      -- see for example fdE below

----------------------
-- Recall that
--   Control.Monad.(>=>) :: (a -> Maybe b) -> (b -> Maybe c) -> a -> Maybe c

(>.>) :: (a -> b) -> (b -> c) -> a -> c
-- Reverse function composition (do f first, then g)
infixr 1 >.>
(f >.> g) x = g (f x)
infixr 1 |>, |>>

(|>) :: a -> (a->b) -> b     -- Reverse application
x |> f = f x

----------------------
(|>>) :: TrieMap m2
      => (XT (m2 a) -> m1 (m2 a) -> m1 (m2 a))
      -> (m2 a -> m2 a)
      -> m1 (m2 a) -> m1 (m2 a)
(|>>) f g = f (Just . g . deMaybe)

deMaybe :: TrieMap m => Maybe (m a) -> m a
deMaybe Nothing  = emptyTM
deMaybe (Just m) = m

{-
************************************************************************
*                                                                      *
                   IntMaps
*                                                                      *
************************************************************************
-}

instance TrieMap IntMap.IntMap where
  type Key IntMap.IntMap = Int
  emptyTM = IntMap.empty
  lookupTM k m = IntMap.lookup k m
  alterTM = xtInt
  foldTM k m z = IntMap.fold k z m
  mapTM f m = IntMap.map f m

xtInt :: Int -> XT a -> IntMap.IntMap a -> IntMap.IntMap a
xtInt k f m = IntMap.alter f k m

instance Ord k => TrieMap (Map.Map k) where
  type Key (Map.Map k) = k
  emptyTM = Map.empty
  lookupTM = Map.lookup
  alterTM k f m = Map.alter f k m
  foldTM k m z = Map.fold k z m
  mapTM f m = Map.map f m


{-
Note [foldTM determinism]
~~~~~~~~~~~~~~~~~~~~~~~~~
We want foldTM to be deterministic, which is why we have an instance of
TrieMap for UniqDFM, but not for UniqFM. Here's an example of some things that
go wrong if foldTM is nondeterministic. Consider:

  f a b = return (a <> b)

Depending on the order that the typechecker generates constraints you
get either:

  f :: (Monad m, Monoid a) => a -> a -> m a

or:

  f :: (Monoid a, Monad m) => a -> a -> m a

The generated code will be different after desugaring as the dictionaries
will be bound in different orders, leading to potential ABI incompatibility.

One way to solve this would be to notice that the typeclasses could be
sorted alphabetically.

Unfortunately that doesn't quite work with this example:

  f a b = let x = a <> a; y = b <> b in x

where you infer:

  f :: (Monoid m, Monoid m1) => m1 -> m -> m1

or:

  f :: (Monoid m1, Monoid m) => m1 -> m -> m1

Here you could decide to take the order of the type variables in the type
according to depth first traversal and use it to order the constraints.

The real trouble starts when the user enables incoherent instances and
the compiler has to make an arbitrary choice. Consider:

  class T a b where
    go :: a -> b -> String

  instance (Show b) => T Int b where
    go a b = show a ++ show b

  instance (Show a) => T a Bool where
    go a b = show a ++ show b

  f = go 10 True

GHC is free to choose either dictionary to implement f, but for the sake of
determinism we'd like it to be consistent when compiling the same sources
with the same flags.

inert_dicts :: DictMap is implemented with a TrieMap. In getUnsolvedInerts it
gets converted to a bag of (Wanted) Cts using a fold. Then in
solve_simple_wanteds it's merged with other WantedConstraints. We want the
conversion to a bag to be deterministic. For that purpose we use UniqDFM
instead of UniqFM to implement the TrieMap.

See Note [Deterministic UniqFM] in UniqDFM for more details on how it's made
deterministic.
-}

instance TrieMap UniqDFM where
  type Key UniqDFM = Unique
  emptyTM = emptyUDFM
  lookupTM k m = lookupUDFM m k
  alterTM k f m = alterUDFM f m k
  foldTM k m z = foldUDFM k z m
  mapTM f m = mapUDFM f m

{-
************************************************************************
*                                                                      *
                   Maybes
*                                                                      *
************************************************************************

If              m is a map from k -> val
then (MaybeMap m) is a map from (Maybe k) -> val
-}

data MaybeMap m a = MM { mm_nothing  :: Maybe a, mm_just :: m a }

instance TrieMap m => TrieMap (MaybeMap m) where
   type Key (MaybeMap m) = Maybe (Key m)
   emptyTM  = MM { mm_nothing = Nothing, mm_just = emptyTM }
   lookupTM = lkMaybe lookupTM
   alterTM  = xtMaybe alterTM
   foldTM   = fdMaybe
   mapTM    = mapMb

mapMb :: TrieMap m => (a->b) -> MaybeMap m a -> MaybeMap m b
mapMb f (MM { mm_nothing = mn, mm_just = mj })
  = MM { mm_nothing = fmap f mn, mm_just = mapTM f mj }

lkMaybe :: (forall b. k -> m b -> Maybe b)
        -> Maybe k -> MaybeMap m a -> Maybe a
lkMaybe _  Nothing  = mm_nothing
lkMaybe lk (Just x) = mm_just >.> lk x

xtMaybe :: (forall b. k -> XT b -> m b -> m b)
        -> Maybe k -> XT a -> MaybeMap m a -> MaybeMap m a
xtMaybe _  Nothing  f m = m { mm_nothing  = f (mm_nothing m) }
xtMaybe tr (Just x) f m = m { mm_just = mm_just m |> tr x f }

fdMaybe :: TrieMap m => (a -> b -> b) -> MaybeMap m a -> b -> b
fdMaybe k m = foldMaybe k (mm_nothing m)
            . foldTM k (mm_just m)

{-
************************************************************************
*                                                                      *
                   Lists
*                                                                      *
************************************************************************
-}

data ListMap m a
  = LM { lm_nil  :: Maybe a
       , lm_cons :: m (ListMap m a) }

instance TrieMap m => TrieMap (ListMap m) where
   type Key (ListMap m) = [Key m]
   emptyTM  = LM { lm_nil = Nothing, lm_cons = emptyTM }
   lookupTM = lkList lookupTM
   alterTM  = xtList alterTM
   foldTM   = fdList
   mapTM    = mapList

mapList :: TrieMap m => (a->b) -> ListMap m a -> ListMap m b
mapList f (LM { lm_nil = mnil, lm_cons = mcons })
  = LM { lm_nil = fmap f mnil, lm_cons = mapTM (mapTM f) mcons }

lkList :: TrieMap m => (forall b. k -> m b -> Maybe b)
        -> [k] -> ListMap m a -> Maybe a
lkList _  []     = lm_nil
lkList lk (x:xs) = lm_cons >.> lk x >=> lkList lk xs

xtList :: TrieMap m => (forall b. k -> XT b -> m b -> m b)
        -> [k] -> XT a -> ListMap m a -> ListMap m a
xtList _  []     f m = m { lm_nil  = f (lm_nil m) }
xtList tr (x:xs) f m = m { lm_cons = lm_cons m |> tr x |>> xtList tr xs f }

fdList :: forall m a b. TrieMap m
       => (a -> b -> b) -> ListMap m a -> b -> b
fdList k m = foldMaybe k          (lm_nil m)
           . foldTM    (fdList k) (lm_cons m)

foldMaybe :: (a -> b -> b) -> Maybe a -> b -> b
foldMaybe _ Nothing  b = b
foldMaybe k (Just a) b = k a b

{-
************************************************************************
*                                                                      *
                   Basic maps
*                                                                      *
************************************************************************
-}

lkDNamed :: NamedThing n => n -> DNameEnv a -> Maybe a
lkDNamed n env = lookupDNameEnv env (getName n)

xtDNamed :: NamedThing n => n -> XT a -> DNameEnv a -> DNameEnv a
xtDNamed tc f m = alterDNameEnv f m (getName tc)


{-
************************************************************************
*                                                                      *
                   GenMap
*                                                                      *
************************************************************************

Note [Compressed TrieMap]
~~~~~~~~~~~~~~~~~~~~~~~~~

The GenMap constructor augments TrieMaps with leaf compression.  This helps
solve the performance problem detailed in #9960: suppose we have a handful
H of entries in a TrieMap, each with a very large key, size K. If you fold over
such a TrieMap you'd expect time O(H). That would certainly be true of an
association list! But with TrieMap we actually have to navigate down a long
singleton structure to get to the elements, so it takes time O(K*H).  This
can really hurt on many type-level computation benchmarks:
see for example T9872d.

The point of a TrieMap is that you need to navigate to the point where only one
key remains, and then things should be fast.  So the point of a SingletonMap
is that, once we are down to a single (key,value) pair, we stop and
just use SingletonMap.

'EmptyMap' provides an even more basic (but essential) optimization: if there is
nothing in the map, don't bother building out the (possibly infinite) recursive
TrieMap structure!
-}

data GenMap m a
   = EmptyMap
   | SingletonMap (Key m) a
   | MultiMap (m a)

instance (Outputable a, Outputable (m a)) => Outputable (GenMap m a) where
  ppr EmptyMap = text "Empty map"
  ppr (SingletonMap _ v) = text "Singleton map" <+> ppr v
  ppr (MultiMap m) = ppr m

-- TODO undecidable instance
instance (Eq (Key m), TrieMap m) => TrieMap (GenMap m) where
   type Key (GenMap m) = Key m
   emptyTM  = EmptyMap
   lookupTM = lkG
   alterTM  = xtG
   foldTM   = fdG
   mapTM    = mapG

-- NB: Be careful about RULES and type families (#5821).  So we should make sure
-- to specify @Key TypeMapX@ (and not @DeBruijn Type@, the reduced form)

{-# SPECIALIZE lkG :: Key TypeMapX     -> TypeMapG a     -> Maybe a #-}
lkG :: (Eq (Key m), TrieMap m) => Key m -> GenMap m a -> Maybe a
lkG _ EmptyMap                         = Nothing
lkG k (SingletonMap k' v') | k == k'   = Just v'
                           | otherwise = Nothing
lkG k (MultiMap m)                     = lookupTM k m

{-# SPECIALIZE xtG :: Key TypeMapX     -> XT a -> TypeMapG a -> TypeMapG a #-}
xtG :: (Eq (Key m), TrieMap m) => Key m -> XT a -> GenMap m a -> GenMap m a
xtG k f EmptyMap
    = case f Nothing of
        Just v  -> SingletonMap k v
        Nothing -> EmptyMap
xtG k f m@(SingletonMap k' v')
    | k' == k
    -- The new key matches the (single) key already in the tree.  Hence,
    -- apply @f@ to @Just v'@ and build a singleton or empty map depending
    -- on the 'Just'/'Nothing' response respectively.
    = case f (Just v') of
        Just v'' -> SingletonMap k' v''
        Nothing  -> EmptyMap
    | otherwise
    -- We've hit a singleton tree for a different key than the one we are
    -- searching for. Hence apply @f@ to @Nothing@. If result is @Nothing@ then
    -- we can just return the old map. If not, we need a map with *two*
    -- entries. The easiest way to do that is to insert two items into an empty
    -- map of type @m a@.
    = case f Nothing of
        Nothing  -> m
        Just v   -> emptyTM |> alterTM k' (const (Just v'))
                           >.> alterTM k  (const (Just v))
                           >.> MultiMap
xtG k f (MultiMap m) = MultiMap (alterTM k f m)

{-# SPECIALIZE mapG :: (a -> b) -> TypeMapG a     -> TypeMapG b #-}
mapG :: TrieMap m => (a -> b) -> GenMap m a -> GenMap m b
mapG _ EmptyMap = EmptyMap
mapG f (SingletonMap k v) = SingletonMap k (f v)
mapG f (MultiMap m) = MultiMap (mapTM f m)

{-# SPECIALIZE fdG :: (a -> b -> b) -> TypeMapG a     -> b -> b #-}
fdG :: TrieMap m => (a -> b -> b) -> GenMap m a -> b -> b
fdG _ EmptyMap = \z -> z
fdG k (SingletonMap _ v) = \z -> k v z
fdG k (MultiMap m) = foldTM k m

{-
************************************************************************
*                                                                      *
                   Types
*                                                                      *
************************************************************************
-}

-- | @TypeMapG a@ is a map from @DeBruijn Type@ to @a@.  The extended
-- key makes it suitable for recursive traversal, since it can track binders,
-- but it is strictly internal to this module.  If you are including a 'TypeMap'
-- inside another 'TrieMap', this is the type you want. Note that this
-- lookup does not do a kind-check. Thus, all keys in this map must have
-- the same kind.
type TypeMapG = GenMap TypeMapX

-- | @TypeMapX a@ is the base map from @DeBruijn Type@ to @a@, but without the
-- 'GenMap' optimization.
data TypeMapX a
  = TM { tm_var    :: VarMap a
       , tm_app    :: TypeMapG (TypeMapG a)
       , tm_tycon  :: DNameEnv a
       , tm_forall :: TypeMapG (BndrMap a) -- See Note [Binders]
       , tm_tylit  :: TyLitMap a
       , tm_coerce :: Maybe a
       }
    -- Note that there is no tyconapp case; see Note [Equality on AppTys] in Type

-- | squeeze out any synonyms, convert Constraint to *, and change TyConApps
-- to nested AppTys. Why the last one? See Note [Equality on AppTys] in Type
trieMapView :: Type -> Maybe Type
trieMapView ty | Just ty' <- coreViewOneStarKind ty = Just ty'
trieMapView (TyConApp tc tys@(_:_)) = Just $ foldl AppTy (TyConApp tc []) tys
trieMapView (ForAllTy (Anon arg) res)
  = Just ((TyConApp funTyCon [] `AppTy` arg) `AppTy` res)
trieMapView _ = Nothing

instance TrieMap TypeMapX where
   type Key TypeMapX = DeBruijn Type
   emptyTM  = emptyT
   lookupTM = lkT
   alterTM  = xtT
   foldTM   = fdT
   mapTM    = mapT

instance Eq (DeBruijn Type) where
  env_t@(D env t) == env_t'@(D env' t')
    | Just new_t  <- coreViewOneStarKind t  = D env new_t == env_t'
    | Just new_t' <- coreViewOneStarKind t' = env_t       == D env' new_t'
    | otherwise
    = case (t, t') of
        (CastTy t1 _, _)  -> D env t1 == D env t'
        (_, CastTy t1' _) -> D env t  == D env t1'

        (TyVarTy v, TyVarTy v')
            -> case (lookupCME env v, lookupCME env' v') of
                (Just bv, Just bv') -> bv == bv'
                (Nothing, Nothing)  -> v == v'
                _ -> False
                -- See Note [Equality on AppTys] in Type
        (AppTy t1 t2, s) | Just (t1', t2') <- repSplitAppTy_maybe s
            -> D env t1 == D env' t1' && D env t2 == D env' t2'
        (s, AppTy t1' t2') | Just (t1, t2) <- repSplitAppTy_maybe s
            -> D env t1 == D env' t1' && D env t2 == D env' t2'
        (ForAllTy (Anon t1) t2, ForAllTy (Anon t1') t2')
            -> D env t1 == D env' t1' && D env t2 == D env' t2'
        (TyConApp tc tys, TyConApp tc' tys')
            -> tc == tc' && D env tys == D env' tys'
        (LitTy l, LitTy l')
            -> l == l'
        (ForAllTy (Named tv _) ty, ForAllTy (Named tv' _) ty')
            -> D env (tyVarKind tv)    == D env' (tyVarKind tv') &&
               D (extendCME env tv) ty == D (extendCME env' tv') ty'
        (CoercionTy {}, CoercionTy {})
            -> True
        _ -> False

instance Outputable a => Outputable (TypeMapG a) where
  ppr m = text "TypeMap elts" <+> ppr (foldTM (:) m [])

emptyT :: TypeMapX a
emptyT = TM { tm_var  = emptyTM
            , tm_app  = EmptyMap
            , tm_tycon  = emptyDNameEnv
            , tm_forall = EmptyMap
            , tm_tylit  = emptyTyLitMap
            , tm_coerce = Nothing }

mapT :: (a->b) -> TypeMapX a -> TypeMapX b
mapT f (TM { tm_var  = tvar, tm_app = tapp, tm_tycon = ttycon
           , tm_forall = tforall, tm_tylit = tlit
           , tm_coerce = tcoerce })
  = TM { tm_var    = mapTM f tvar
       , tm_app    = mapTM (mapTM f) tapp
       , tm_tycon  = mapTM f ttycon
       , tm_forall = mapTM (mapTM f) tforall
       , tm_tylit  = mapTM f tlit
       , tm_coerce = fmap f tcoerce }

-----------------
lkT :: DeBruijn Type -> TypeMapX a -> Maybe a
lkT (D env ty) m = go ty m
  where
    go ty | Just ty' <- trieMapView ty = go ty'
    go (TyVarTy v)                 = tm_var    >.> lkVar env v
    go (AppTy t1 t2)               = tm_app    >.> lkG (D env t1)
                                               >=> lkG (D env t2)
    go (TyConApp tc [])            = tm_tycon  >.> lkDNamed tc
    go ty@(TyConApp _ (_:_))       = pprPanic "lkT TyConApp" (ppr ty)
    go (LitTy l)                   = tm_tylit  >.> lkTyLit l
    go (ForAllTy (Named tv _) ty)  = tm_forall >.> lkG (D (extendCME env tv) ty)
                                               >=> lkBndr env tv
    go ty@(ForAllTy (Anon _) _)    = pprPanic "lkT FunTy" (ppr ty)
    go (CastTy t _)                = go t
    go (CoercionTy {})             = tm_coerce

-----------------
xtT :: DeBruijn Type -> XT a -> TypeMapX a -> TypeMapX a
xtT (D env ty) f m | Just ty' <- trieMapView ty = xtT (D env ty') f m

xtT (D env (TyVarTy v))       f m = m { tm_var    = tm_var m |> xtVar env v f }
xtT (D env (AppTy t1 t2))     f m = m { tm_app    = tm_app m |> xtG (D env t1)
                                                            |>> xtG (D env t2) f }
xtT (D _   (TyConApp tc []))  f m = m { tm_tycon  = tm_tycon m |> xtDNamed tc f }
xtT (D _   (LitTy l))         f m = m { tm_tylit  = tm_tylit m |> xtTyLit l f }
xtT (D env (CastTy t _))      f m = xtT (D env t) f m
xtT (D _   (CoercionTy {}))   f m = m { tm_coerce = tm_coerce m |> f }
xtT (D env (ForAllTy (Named tv _) ty))  f m
  = m { tm_forall = tm_forall m |> xtG (D (extendCME env tv) ty)
                                |>> xtBndr env tv f }
xtT (D _   ty@(TyConApp _ (_:_)))    _ _ = pprPanic "xtT TyConApp" (ppr ty)
xtT (D _   ty@(ForAllTy (Anon _) _)) _ _ = pprPanic "xtT FunTy" (ppr ty)

fdT :: (a -> b -> b) -> TypeMapX a -> b -> b
fdT k m = foldTM k (tm_var m)
        . foldTM (foldTM k) (tm_app m)
        . foldTM k (tm_tycon m)
        . foldTM (foldTM k) (tm_forall m)
        . foldTyLit k (tm_tylit m)
        . foldMaybe k (tm_coerce m)

------------------------
data TyLitMap a = TLM { tlm_number :: Map.Map Integer a
                      , tlm_string :: Map.Map FastString a
                      }

instance TrieMap TyLitMap where
   type Key TyLitMap = TyLit
   emptyTM  = emptyTyLitMap
   lookupTM = lkTyLit
   alterTM  = xtTyLit
   foldTM   = foldTyLit
   mapTM    = mapTyLit

emptyTyLitMap :: TyLitMap a
emptyTyLitMap = TLM { tlm_number = Map.empty, tlm_string = Map.empty }

mapTyLit :: (a->b) -> TyLitMap a -> TyLitMap b
mapTyLit f (TLM { tlm_number = tn, tlm_string = ts })
  = TLM { tlm_number = Map.map f tn, tlm_string = Map.map f ts }

lkTyLit :: TyLit -> TyLitMap a -> Maybe a
lkTyLit l =
  case l of
    NumTyLit n -> tlm_number >.> Map.lookup n
    StrTyLit n -> tlm_string >.> Map.lookup n

xtTyLit :: TyLit -> XT a -> TyLitMap a -> TyLitMap a
xtTyLit l f m =
  case l of
    NumTyLit n -> m { tlm_number = tlm_number m |> Map.alter f n }
    StrTyLit n -> m { tlm_string = tlm_string m |> Map.alter f n }

foldTyLit :: (a -> b -> b) -> TyLitMap a -> b -> b
foldTyLit l m = flip (Map.fold l) (tlm_string m)
              . flip (Map.fold l) (tlm_number m)

-------------------------------------------------
-- | @TypeMap a@ is a map from 'Type' to @a@.  If you are a client, this
-- is the type you want. The keys in this map may have different kinds.
newtype TypeMap a = TypeMap (TypeMapG (TypeMapG a))

lkTT :: DeBruijn Type -> TypeMap a -> Maybe a
lkTT (D env ty) (TypeMap m) = lkG (D env $ typeKind ty) m
                          >>= lkG (D env ty)

xtTT :: DeBruijn Type -> XT a -> TypeMap a -> TypeMap a
xtTT (D env ty) f (TypeMap m)
  = TypeMap (m |> xtG (D env $ typeKind ty)
               |>> xtG (D env ty) f)

-- Below are some client-oriented functions which operate on 'TypeMap'.

instance TrieMap TypeMap where
    type Key TypeMap = Type
    emptyTM = TypeMap emptyTM
    lookupTM k m = lkTT (deBruijnize k) m
    alterTM k f m = xtTT (deBruijnize k) f m
    foldTM k (TypeMap m) = foldTM (foldTM k) m
    mapTM f (TypeMap m) = TypeMap (mapTM (mapTM f) m)

foldTypeMap :: (a -> b -> b) -> b -> TypeMap a -> b
foldTypeMap k z m = foldTM k m z

emptyTypeMap :: TypeMap a
emptyTypeMap = emptyTM

lookupTypeMap :: TypeMap a -> Type -> Maybe a
lookupTypeMap cm t = lookupTM t cm

extendTypeMap :: TypeMap a -> Type -> a -> TypeMap a
extendTypeMap m t v = alterTM t (const (Just v)) m

-- | A 'LooseTypeMap' doesn't do a kind-check. Thus, when lookup up (t |> g),
-- you'll find entries inserted under (t), even if (g) is non-reflexive.
newtype LooseTypeMap a
  = LooseTypeMap (TypeMapG a)

instance TrieMap LooseTypeMap where
  type Key LooseTypeMap = Type
  emptyTM = LooseTypeMap emptyTM
  lookupTM k (LooseTypeMap m) = lookupTM (deBruijnize k) m
  alterTM k f (LooseTypeMap m) = LooseTypeMap (alterTM (deBruijnize k) f m)
  foldTM f (LooseTypeMap m) = foldTM f m
  mapTM f (LooseTypeMap m) = LooseTypeMap (mapTM f m)

{-
************************************************************************
*                                                                      *
                   Variables
*                                                                      *
************************************************************************
-}

type BoundVar = Int  -- Bound variables are deBruijn numbered
type BoundVarMap a = IntMap.IntMap a

data CmEnv = CME { cme_next :: BoundVar
                 , cme_env  :: VarEnv BoundVar }

emptyCME :: CmEnv
emptyCME = CME { cme_next = 0, cme_env = emptyVarEnv }

extendCME :: CmEnv -> Var -> CmEnv
extendCME (CME { cme_next = bv, cme_env = env }) v
  = CME { cme_next = bv+1, cme_env = extendVarEnv env v bv }


lookupCME :: CmEnv -> Var -> Maybe BoundVar
lookupCME (CME { cme_env = env }) v = lookupVarEnv env v

-- | @DeBruijn a@ represents @a@ modulo alpha-renaming.  This is achieved
-- by equipping the value with a 'CmEnv', which tracks an on-the-fly deBruijn
-- numbering.  This allows us to define an 'Eq' instance for @DeBruijn a@, even
-- if this was not (easily) possible for @a@.  Note: we purposely don't
-- export the constructor.  Make a helper function if you find yourself
-- needing it.
data DeBruijn a = D CmEnv a

-- | Synthesizes a @DeBruijn a@ from an @a@, by assuming that there are no
-- bound binders (an empty 'CmEnv').  This is usually what you want if there
-- isn't already a 'CmEnv' in scope.
deBruijnize :: a -> DeBruijn a
deBruijnize = D emptyCME

instance Eq (DeBruijn a) => Eq (DeBruijn [a]) where
    D _   []     == D _    []       = True
    D env (x:xs) == D env' (x':xs') = D env x  == D env' x' &&
                                      D env xs == D env' xs'
    _            == _               = False

--------- Variable binders -------------

-- | A 'BndrMap' is a 'TypeMapG' which allows us to distinguish between
-- binding forms whose binders have different types.  For example,
-- if we are doing a 'TrieMap' lookup on @\(x :: Int) -> ()@, we should
-- not pick up an entry in the 'TrieMap' for @\(x :: Bool) -> ()@:
-- we can disambiguate this by matching on the type (or kind, if this
-- a binder in a type) of the binder.
type BndrMap = TypeMapG

-- Note [Binders]
-- ~~~~~~~~~~~~~~
-- We need to use 'BndrMap' for 'Coercion', 'CoreExpr' AND 'Type', since all
-- of these data types have binding forms.

lkBndr :: CmEnv -> Var -> BndrMap a -> Maybe a
lkBndr env v m = lkG (D env (varType v)) m

xtBndr :: CmEnv -> Var -> XT a -> BndrMap a -> BndrMap a
xtBndr env v f = xtG (D env (varType v)) f

--------- Variable occurrence -------------
data VarMap a = VM { vm_bvar   :: BoundVarMap a  -- Bound variable
                   , vm_fvar   :: DVarEnv a }      -- Free variable

instance TrieMap VarMap where
   type Key VarMap = Var
   emptyTM  = VM { vm_bvar = IntMap.empty, vm_fvar = emptyDVarEnv }
   lookupTM = lkVar emptyCME
   alterTM  = xtVar emptyCME
   foldTM   = fdVar
   mapTM    = mapVar

mapVar :: (a->b) -> VarMap a -> VarMap b
mapVar f (VM { vm_bvar = bv, vm_fvar = fv })
  = VM { vm_bvar = mapTM f bv, vm_fvar = mapTM f fv }

lkVar :: CmEnv -> Var -> VarMap a -> Maybe a
lkVar env v
  | Just bv <- lookupCME env v = vm_bvar >.> lookupTM bv
  | otherwise                  = vm_fvar >.> lkDFreeVar v

xtVar :: CmEnv -> Var -> XT a -> VarMap a -> VarMap a
xtVar env v f m
  | Just bv <- lookupCME env v = m { vm_bvar = vm_bvar m |> alterTM bv f }
  | otherwise                  = m { vm_fvar = vm_fvar m |> xtDFreeVar v f }

fdVar :: (a -> b -> b) -> VarMap a -> b -> b
fdVar k m = foldTM k (vm_bvar m)
          . foldTM k (vm_fvar m)

lkDFreeVar :: Var -> DVarEnv a -> Maybe a
lkDFreeVar var env = lookupDVarEnv env var

xtDFreeVar :: Var -> XT a -> DVarEnv a -> DVarEnv a
xtDFreeVar v f m = alterDVarEnv f m v
