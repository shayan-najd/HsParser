
module DynFlags where

data DynFlags

pprUserLength        :: DynFlags -> Int
pprCols              :: DynFlags -> Int
useUnicode     :: DynFlags -> Bool
useUnicodeSyntax     :: DynFlags -> Bool
defaultDynFlag :: DynFlags