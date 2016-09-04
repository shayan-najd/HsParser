module MkId where
import {-# SOURCE #-} Name( Name )
import Var( Id )
import Class( Class )
import {-# SOURCE #-} DataCon( DataCon )

data DataConBoxer

mkDataConWorkId :: Name -> DataCon -> Id
mkDictSelId     :: Name -> Class   -> Id

magicDictId :: Id
