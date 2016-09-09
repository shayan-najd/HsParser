{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


HsImpExp: Abstract syntax: imports, exports, interfaces
-}

{-# LANGUAGE DeriveDataTypeable #-}

module Language.Haskell.Syntax.HsImpExp  ( LIE
                , LImportDecl
                , ImportDecl(..)
                , IE(..)
                , IEWildcard(..)) where

import Language.Haskell.Syntax.Module           ( ModuleName )
import Language.Haskell.Syntax.HsDoc            ( HsDocString )
import Language.Haskell.Syntax.BasicTypes     ( SourceText, StringLiteral(..) )
import Language.Haskell.Syntax.FieldLabel       ( FieldLbl(..) )
import Language.Haskell.Syntax.SrcLoc

import Data.Data

type LImportDecl name = Located (ImportDecl name)

-- | A single Haskell @import@ declaration.
data ImportDecl name
  = ImportDecl {
      ideclSourceSrc :: Maybe SourceText,
                                 -- Note [Pragma source text] in BasicTypes
      ideclName      :: Located ModuleName, -- ^ Module name.
      ideclPkgQual   :: Maybe StringLiteral,  -- ^ Package qualifier.
      ideclSource    :: Bool,          -- ^ True <=> {-\# SOURCE \#-} import
      ideclSafe      :: Bool,          -- ^ True => safe import
      ideclQualified :: Bool,          -- ^ True => qualified
      ideclImplicit  :: Bool,          -- ^ True => implicit import (of Prelude)
      ideclAs        :: Maybe ModuleName,  -- ^ as Module
      ideclHiding    :: Maybe (Bool, Located [LIE name])
                                       -- ^ (True => hiding, names)
    }
       deriving Data

type LIE name = Located (IE name)

data IE name
  = IEVar       (Located name)
        -- ^ - 'ApiAnnotation.AnnKeywordId's : 'ApiAnnotation.AnnPattern',
        --             'ApiAnnotation.AnnType'

        -- For details on above see note [Api annotations] in ApiAnnotation
        -- See Note [Located RdrNames] in HsExpr
  | IEThingAbs  (Located name)     -- ^ Class/Type (can't tell)
        --  - 'ApiAnnotation.AnnKeywordId's : 'ApiAnnotation.AnnPattern',
        --             'ApiAnnotation.AnnType','ApiAnnotation.AnnVal'

        -- For details on above see note [Api annotations] in ApiAnnotation
        -- See Note [Located RdrNames] in HsExpr
  | IEThingAll  (Located name)     -- ^ Class/Type plus all methods/constructors
        --
        -- - 'ApiAnnotation.AnnKeywordId's : 'ApiAnnotation.AnnOpen',
        --       'ApiAnnotation.AnnDotdot','ApiAnnotation.AnnClose',
        --                                 'ApiAnnotation.AnnType'

        -- For details on above see note [Api annotations] in ApiAnnotation
        -- See Note [Located RdrNames] in HsExpr

  | IEThingWith (Located name)
                IEWildcard
                [Located name]
                [Located (FieldLbl name)]
                 -- ^ Class/Type plus some methods/constructors
                 -- and record fields; see Note [IEThingWith]
        -- - 'ApiAnnotation.AnnKeywordId's : 'ApiAnnotation.AnnOpen',
        --                                   'ApiAnnotation.AnnClose',
        --                                   'ApiAnnotation.AnnComma',
        --                                   'ApiAnnotation.AnnType'

        -- For details on above see note [Api annotations] in ApiAnnotation
  | IEModuleContents  (Located ModuleName) -- ^ (Export Only)
        --
        -- - 'ApiAnnotation.AnnKeywordId's : 'ApiAnnotation.AnnModule'

        -- For details on above see note [Api annotations] in ApiAnnotation
  | IEGroup             Int HsDocString  -- ^ Doc section heading
  | IEDoc               HsDocString      -- ^ Some documentation
  | IEDocNamed          String           -- ^ Reference to named doc
  deriving ({- Eq, -} Data)

data IEWildcard = NoIEWildcard | IEWildcard Int deriving (Eq, Data)
