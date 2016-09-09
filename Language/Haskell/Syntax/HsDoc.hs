{-# LANGUAGE CPP, DeriveDataTypeable #-}

module Language.Haskell.Syntax.HsDoc ( LHsDocString
                                     , HsDocString(..)
                                     ) where

import Language.Haskell.Syntax.SrcLoc
import Language.Haskell.Utility.FastString

import Data.Data

newtype HsDocString = HsDocString FastString
  deriving (Eq, Show, Data)

type LHsDocString = Located HsDocString
