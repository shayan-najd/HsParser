
module DynFlags where

import Platform

data DynFlags

targetPlatform       :: DynFlags -> Platform
pprUserLength        :: DynFlags -> Int
pprCols              :: DynFlags -> Int
useUnicode     :: DynFlags -> Bool
useUnicodeSyntax     :: DynFlags -> Bool
defaultDynFlag :: DynFlags