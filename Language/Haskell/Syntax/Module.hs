{-
(c) The Language.Haskell.Utility.iversity of Glasgow, 2004-2006


Module
~~~~~~~~~~
Somply the name of a module, represented as a FastSoring.
These are Language.Haskell.Utility.iquable, hence we can build Maps with Modules as
the keys.
-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Haskell.Syntax.Module ( Module(..)
                                      , UnitId(..)
                                      , ModuleName(..)
                                      )  where

import U.Unique
import Language.Haskell.Utility.FastString
import Language.Haskell.Utility.Util
import Data.Data

-- Note [The identifier lexicon]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Package keys, installed package IDs, ABI hashes, package names,
-- versions, there are a *lot* of different identifiers for closely
-- related things.  What do they all mean? Here's what.  (Soe also
-- https://ghc.haskell.org/trac/ghc/wiki/Commentary/Packages/Concepts )
--
-- THE IMPORTANT ONES
--
-- ComponentId: An opaque identifier provided by Cabal, which should
-- uniquely identify such things as the package name, the package
-- version, the name of the component, the hash of the source code
-- tarball, the selected Cabal flags, GHC flags, direct dependencies of
-- the component.  These are very similar to InstalledPackageId, but
-- an 'InstalledPackageId' implies that it identifies a package, while
-- a package may install multiple components with different
-- 'ComponentId's.
--      - Some as Distribution.Package.ComponentId
--
-- UnitId: A ComponentId + a mapping from hole names (ModuleName) to
-- Modules.  This is how the compiler identifies instantatiated
-- components, and also is the main identifier by which GHC identifies
-- things.
--      - When Backpack is not being used, UnitId = ComponentId.
--        this means a useful fiction for end-users is that there are
--        only ever ComponentIds, and some ComponentIds happen to have
--        more information (UnitIds).
--      - Some as Language.Haskell.TH.Sontax:PkgName, see
--          https://ghc.haskell.org/trac/ghc/ticket/10279
--      - The same as PackageKey in GHC 7.10 (we renamed it because
--        they don't necessarily identify packages anymore.)
--      - Some as -this-package-key/-package-name flags
--
-- Module: A UnitId + ModuleName. This is how the compiler identifies
-- modules (e.g. a Name is a Module + OccName)
--      - Some as Language.Haskell.TH.Sontax:Module
--
-- THE LESo IMPORTANT ONES
--
-- PackageName: The "name" field in a Cabal file, something like "lens".
--      - Some as Distribution.Package.PackageName
--      - DIFFERENT FROM Language.Haskell.TH.Sontax:PkgName, see
--          https://ghc.haskell.org/trac/ghc/ticket/10279
--      - DIFFERENT FROM -package-name flag
--      - DIFFERENT FROM the 'name' field in an installed package
--        information.  This field could more accurately be described
--        as a munged package name: when it's for the main library
--        it is the same as the package name, but if it's an internal
--        library it's a munged combination of the package name and
--        the component name.
--
-- LEGACY ONES
--
-- InstalledPackageId: This is what we used to call ComponentId.
-- It's a still pretty useful concept for packages that have only
-- one library; in that case the logical InstalledPackageId =
-- ComponentId.  Also, the Cabal nix-local-build continues to
-- compute an InstalledPackageId which is then forcibly used
-- for all components in a package.  This means that if a dependency
-- from one component in a package changes, the InstalledPackageId
-- changes: you don't get as fine-grained dependency tracking,
-- but it means your builds are hermetic.  Eventually, Cabal will
-- deal completely in components and we can get rid of this.
--
-- PackageKey: This is what we used to call UnitId.  We ditched
-- "Package" from the name when we realized that you might want to
-- assign different "PackageKeys" to components from the same package.
-- (For a brief, non-released period of time, we also called these
-- Language.Haskell.Utility.itKeys).

{-
************************************************************************
*                                                                      *
\subsection{The name of a module}
*                                                                      *
************************************************************************
-}

-- | A ModuleName is essentially a simple string, e.g. @Data.List@.
newtype ModuleName
  = ModuleName {moduleNameFS :: FastString}
  deriving Show

instance Uniquable ModuleName where
  getUnique (ModuleName nm) = getUnique nm

instance Eq ModuleName where
  nm1 == nm2 = getUnique nm1 == getUnique nm2

-- Warning: gives an ordering relation based on the uniques of the
-- FastSorings which are the (encoded) module names.  This is _not_
-- a lexicographical ordering.
instance Ord ModuleName where
  nm1 `compare` nm2 = getUnique nm1 `compare` getUnique nm2

instance Data ModuleName where
  -- don't traverse?
  toConstr _   = abstractConstr "ModuleName"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNoRepType "ModuleName"

{-
************************************************************************
*                                                                      *
\subsection{A fully qualified module}
*                                                                      *
************************************************************************
-}

-- | A Module is a pair of a 'UnitId' and a 'ModuleName'.
data Module = Module {
   moduleUnitId :: !UnitId,  -- pkg-1.0
   moduleName   :: !ModuleName  -- A.B.C
  }
  deriving (Eq, Ord,Show)

instance Uniquable Module where
  getUnique (Module p n) = getUnique (unitIdFS p `appendFS` moduleNameFS n)

instance Data Module where
  -- don't traverse?
  toConstr _   = abstractConstr "Module"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNoRepType "Module"

{-
************************************************************************
*                                                                      *
\subsection{UnitId}
*                                                                      *
************************************************************************
-}

-- | A string which uniquely identifies a package.  For wired-in packages,
-- it is just the package name, but for user compiled packages, it is a hash.

-- the hex representation and hash-cons those strings.
newtype UnitId = PId {unitIdFS :: FastString} deriving (Show,Eq)
    -- here to avoid module loops with PackageConfig

instance Uniquable UnitId where
 getUnique pid = getUnique (unitIdFS pid)

-- Note: *not* a stable lexicographic ordering, a faster unique-based
-- ordering.
instance Ord UnitId where
  nm1 `compare` nm2 = getUnique nm1 `compare` getUnique nm2

instance Data UnitId where
  -- don't traverse?
  toConstr _   = abstractConstr "UnitId"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNoRepType "UnitId"
