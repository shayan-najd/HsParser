module Language.Haskell.Syntax.Module where

data Module
data ModuleName
data UnitId
moduleName :: Module -> ModuleName
moduleUnitId :: Module -> UnitId
