{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1998


This module contains definitions for the IdInfo for things that
have a standard form, namely:

- data constructors
- record selectors
- method and superclass selectors
- primitive operations
-}

{-# LANGUAGE CPP #-}

module MkId (DataConBoxer,mkDataConWorkId,mkDictSelId,magicDictId) where

#include "HsVersions.h"

import DataCon
import {-# SOURCE #-} Name
import Id
import Class

data DataConBoxer -- SHAYAN: todo!

mkDataConWorkId :: Name -> DataCon -> Id
mkDataConWorkId = error "SHAYAN: TODO!"

mkDictSelId :: Name -> Class -> Id
mkDictSelId =  error "SHAYAN: TODO!"

magicDictId :: Id
magicDictId = error "SHAYAN: TODO!"
