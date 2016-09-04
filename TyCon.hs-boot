module TyCon where

import {-# SOURCE #-} Name (Name)
import U.Unique (Unique)

data TyCon

tyConName           :: TyCon -> Name
tyConUnique         :: TyCon -> Unique
isTupleTyCon        :: TyCon -> Bool
isUnboxedTupleTyCon :: TyCon -> Bool
isFunTyCon          :: TyCon -> Bool
