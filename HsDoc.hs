{-# LANGUAGE CPP, DeriveDataTypeable #-}

module HsDoc {- (
  HsDocString(..),
  LHsDocString,
  ppr_mbDoc
  ) -} where

#include "HsVersions.h"

import SrcLoc
import U.FastString

import Data.Data

newtype HsDocString = HsDocString FastString
  deriving (Eq, Show, Data)

type LHsDocString = Located HsDocString
