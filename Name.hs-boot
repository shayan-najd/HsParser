module Name where

import {-# SOURCE #-} Module
import U.Unique
import U.FastString
import U.Outputable
import Data.Data
import {-# SOURCE #-} OccName
import SrcLoc

data Name
class NamedThing a where
    {-# MINIMAL getName #-}
    getName    :: a -> Name
    getOccName :: a -> OccName
    getOccName n = nameOccName (getName n)      -- Default method

nameModule :: Name -> Module
nameUnique :: Name -> Unique
setNameUnique :: Name -> Unique -> Name
nameOccName   :: Name -> OccName
isExternalName :: Name -> Bool
isInternalName :: Name -> Bool
isWiredInName :: Name -> Bool
localiseName :: Name -> Name
nameIsLocalOrFrom :: Module -> Name -> Bool

instance Outputable Name
instance Uniquable Name
instance Data Name
instance Eq Name
instance Ord Name
instance NamedThing Name

pprModulePrefix :: PprStyle -> Module -> OccName -> SDoc
pprPrefixName :: NamedThing a => a -> SDoc
pprInfixName :: (Outputable a, NamedThing a) => a -> SDoc
mkInternalName :: Unique -> OccName -> SrcSpan -> Name
mkExternalName :: Unique -> Module -> OccName -> SrcSpan -> Name
mkSystemVarName :: Unique -> FastString -> Name
nameSrcSpan :: Name -> SrcSpan
tidyNameOcc :: Name -> OccName -> Name
isSystemName :: Name -> Bool
getSrcSpan :: NamedThing a => a -> SrcSpan
mkSysTvName :: Unique -> FastString -> Name

stableNameCmp :: Name -> Name -> Ordering
mkSystemNameAt :: Unique -> OccName -> SrcSpan -> Name

data BuiltInSyntax = BuiltInSyntax | UserSyntax
