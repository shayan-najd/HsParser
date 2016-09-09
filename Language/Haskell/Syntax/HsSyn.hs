{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section{Haskell abstract syntax definition}

This module glues together the pieces of the Haskell abstract syntax,
which is declared in the various \tr{Hs*} modules.  This module,
therefore, is almost nothing but re-exporting.
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder
{-# LANGUAGE ConstraintKinds #-}

module Language.Haskell.Syntax.HsSyn (
        module Language.Haskell.Syntax.HsBinds,
        module Language.Haskell.Syntax.HsDecls,
        module Language.Haskell.Syntax.HsExpr,
        module Language.Haskell.Syntax.HsImpExp,
        module Language.Haskell.Syntax.HsLit,
        module Language.Haskell.Syntax.HsPat,
        module Language.Haskell.Syntax.HsTypes,
        module Language.Haskell.Syntax.HsUtils,
        module Language.Haskell.Syntax.HsDoc,
        Fixity,
        IsBootInterface,
        HsModule(..)
) where

-- friends:
import Language.Haskell.Syntax.HsDecls
import Language.Haskell.Syntax.HsBinds
import Language.Haskell.Syntax.HsExpr
import Language.Haskell.Syntax.HsImpExp
import Language.Haskell.Syntax.HsLit
import Language.Haskell.Syntax.HsPat
import Language.Haskell.Syntax.HsTypes
import Language.Haskell.Syntax.BasicTypes       ( Fixity, WarningTxt )
import Language.Haskell.Syntax.HsUtils
import Language.Haskell.Syntax.HsDoc

-- others:
import Language.Haskell.Syntax.SrcLoc
import Language.Haskell.Syntax.Module           ( ModuleName )

-- libraries:
import Data.Data hiding ( Fixity )

-- | All we actually declare here is the top-level structure for a module.
data HsModule name
  = HsModule {
      hsmodName :: Maybe (Located ModuleName),
        -- ^ @Nothing@: \"module X where\" is omitted (in which case the next
        --     field is Nothing too)
      hsmodExports :: Maybe (Located [LIE name]),
        -- ^ Export list
        --
        --  - @Nothing@: export list omitted, so export everything
        --
        --  - @Just []@: export /nothing/
        --
        --  - @Just [...]@: as you would expect...
        --
        --
        --  - 'ApiAnnotation.AnnKeywordId's : 'ApiAnnotation.AnnOpen'
        --                                   ,'ApiAnnotation.AnnClose'

        -- For details on above see note [Api annotations] in ApiAnnotation
      hsmodImports :: [LImportDecl name],
        -- ^ We snaffle interesting stuff out of the imported interfaces early
        -- on, adding that info to TyDecls/etc; so this list is often empty,
        -- downstream.
      hsmodDecls :: [LHsDecl name],
        -- ^ Type, class, value, and interface signature decls
      hsmodDeprecMessage :: Maybe (Located WarningTxt),
        -- ^ reason\/explanation for warning/deprecation of this module
        --
        --  - 'ApiAnnotation.AnnKeywordId's : 'ApiAnnotation.AnnOpen'
        --                                   ,'ApiAnnotation.AnnClose'
        --

        -- For details on above see note [Api annotations] in ApiAnnotation
      hsmodHaddockModHeader :: Maybe LHsDocString
        -- ^ Haddock module info and description, unparsed
        --
        --  - 'ApiAnnotation.AnnKeywordId's : 'ApiAnnotation.AnnOpen'
        --                                   ,'ApiAnnotation.AnnClose'

        -- For details on above see note [Api annotations] in ApiAnnotation
   }
     -- ^ 'ApiAnnotation.AnnKeywordId's
     --
     --  - 'ApiAnnotation.AnnModule','ApiAnnotation.AnnWhere'
     --
     --  - 'ApiAnnotation.AnnOpen','ApiAnnotation.AnnSemi',
     --    'ApiAnnotation.AnnClose' for explicit braces and semi around
     --    hsmodImports,hsmodDecls if this style is used.

     -- For details on above see note [Api annotations] in ApiAnnotation
deriving instance (Data name) => Data (HsModule name)

type IsBootInterface = Bool
