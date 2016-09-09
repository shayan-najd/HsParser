{-# LANGUAGE DeriveDataTypeable,
             DeriveFunctor,
             DeriveFoldable,
             DeriveTraversable #-}

module Language.Haskell.Syntax.BooleanFormula ( LBooleanFormula
                                              , BooleanFormula(..)) where

import Data.Data
import Language.Haskell.Syntax.SrcLoc

type LBooleanFormula a
  = Located (BooleanFormula a)

data BooleanFormula a
  = Var    a
  | And    [LBooleanFormula a]
  | Or     [LBooleanFormula a]
  | Parens (LBooleanFormula a)
  deriving (Eq, Data, Functor, Foldable, Traversable)
