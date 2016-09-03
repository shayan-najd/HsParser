{-# LINE 1 "compiler/utils/Fingerprint.hsc" #-}
{-# LANGUAGE CPP #-}
{-# LINE 2 "compiler/utils/Fingerprint.hsc" #-}

-- ----------------------------------------------------------------------------
--
--  (c) The University of Glasgow 2006
--
-- Fingerprints for recompilation checking and ABI versioning.
--
-- http://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/RecompilationAvoidance
--
-- ----------------------------------------------------------------------------

module Fingerprint (
        Fingerprint(..), fingerprint0,
        readHexFingerprint,
        fingerprintData,
        fingerprintString,
        -- Re-exported from GHC.Fingerprint
        getFileHash
   ) where


{-# LINE 23 "compiler/utils/Fingerprint.hsc" #-}
#include "HsVersions.h"

import Numeric          ( readHex )

import GHC.Fingerprint

-- useful for parsing the output of 'md5sum', should we want to do that.
readHexFingerprint :: String -> Fingerprint
readHexFingerprint s = Fingerprint w1 w2
 where (s1,s2) = splitAt 16 s
       [(w1,"")] = readHex s1
       [(w2,"")] = readHex (take 16 s2)
