{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-cse #-}
-- -fno-cse is needed for GLOBAL_VAR's to behave properly

module U.DynFlags
  (DynFlags (DynFlags,
             pprUserLength,
             pprCols,
             useUnicode,
             log_action,
             reverseErrors,
             verbosity,
             extensionFlags,
             warningFlags,
             thisPackage),
   defaultDynFlag,
   xopt,
   GeneralFlag(..),
   gopt,
   gopt_set,
   gopt_unset,
   WarnReason(..),
   WarningFlag(..),
   safeImportsOn,
   FatalMessager,
   LogAction,
   useUnicodeSyntax) where

#include "HsVersions.h"

import Module
import SrcLoc
import U.Outputable
import {-# SOURCE #-} U.ErrUtils ( Severity(..), MsgDoc)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified GHC.LanguageExtensions as LangExt

data DynFlags = DynFlags {
  pprUserLength         :: Int,
  pprCols               :: Int,
  useUnicode            :: Bool,
  log_action            :: LogAction,
  reverseErrors         :: Bool,
  verbosity             :: Int,
  extensionFlags        :: IntSet,
  warningFlags          :: IntSet,
  thisPackage           :: UnitId,
  generalFlags          :: IntSet,
  safeHaskell           :: SafeHaskellMode
}

defaultDynFlag :: DynFlags
defaultDynFlag =  DynFlags {
  pprUserLength  = 5,
  pprCols        = 100,
  useUnicode     = True,
  log_action     = error "no log action",
  reverseErrors  = False,
  verbosity      = 0,
  extensionFlags = IntSet.fromList [5,8,10,22,40,67,77,88,90],
  warningFlags   = IntSet.fromList [0,8,9,10,12,16,27,28,37,38,
                                    39,41,42,48,49,50,51,52,56,61],
  thisPackage    = stringToUnitId "main",
  generalFlags   = IntSet.fromList [34,36,43,47,52,55,59,60,64,81,82,
                                    83,84,87,88,89,98,101,103,104,116],
  safeHaskell    = Sf_None}



-- | Test whether a 'LangExt.Extension' is set
xopt :: LangExt.Extension -> DynFlags -> Bool
xopt f dflags = fromEnum f `IntSet.member` extensionFlags dflags

-- | Enumerates the simple on-or-off dynamic flags
data GeneralFlag
-- See Note [Updating flag description in the User's Guide]
   = Opt_DumpToFile                     -- ^ Append dump output to files instead of stdout.
   | Opt_D_faststring_stats
   | Opt_D_dump_minimal_imports
   | Opt_DoCoreLinting
   | Opt_DoStgLinting
   | Opt_DoCmmLinting
   | Opt_DoAsmLinting
   | Opt_DoAnnotationLinting
   | Opt_NoLlvmMangler                 -- hidden flag

   | Opt_WarnIsError                    -- -Werror; makes warnings fatal
   | Opt_ShowWarnGroups                 -- Show the group a warning belongs to

   | Opt_PrintExplicitForalls
   | Opt_PrintExplicitKinds
   | Opt_PrintExplicitCoercions
   | Opt_PrintExplicitRuntimeReps
   | Opt_PrintEqualityRelations
   | Opt_PrintUnicodeSyntax
   | Opt_PrintExpandedSynonyms
   | Opt_PrintPotentialInstances
   | Opt_PrintTypecheckerElaboration

   -- optimisation opts
   | Opt_CallArity
   | Opt_Strictness
   | Opt_LateDmdAnal
   | Opt_KillAbsence
   | Opt_KillOneShot
   | Opt_FullLaziness
   | Opt_FloatIn
   | Opt_Specialise
   | Opt_SpecialiseAggressively
   | Opt_CrossModuleSpecialise
   | Opt_StaticArgumentTransformation
   | Opt_CSE
   | Opt_LiberateCase
   | Opt_SpecConstr
   | Opt_DoLambdaEtaExpansion
   | Opt_IgnoreAsserts
   | Opt_DoEtaReduction
   | Opt_CaseMerge
   | Opt_UnboxStrictFields
   | Opt_UnboxSmallStrictFields
   | Opt_DictsCheap
   | Opt_EnableRewriteRules             -- Apply rewrite rules during simplification
   | Opt_Vectorise
   | Opt_VectorisationAvoidance
   | Opt_RegsGraph                      -- do graph coloring register allocation
   | Opt_RegsIterative                  -- do iterative coalescing graph coloring register allocation
   | Opt_PedanticBottoms                -- Be picky about how we treat bottom
   | Opt_LlvmTBAA                       -- Use LLVM TBAA infastructure for improving AA (hidden flag)
   | Opt_LlvmPassVectorsInRegisters     -- Pass SIMD vectors in registers (requires a patched LLVM) (hidden flag)
   | Opt_LlvmFillUndefWithGarbage       -- Testing for undef bugs (hidden flag)
   | Opt_IrrefutableTuples
   | Opt_CmmSink
   | Opt_CmmElimCommonBlocks
   | Opt_OmitYields
   | Opt_FunToThunk               -- allow WwLib.mkWorkerArgs to remove all value lambdas
   | Opt_DictsStrict                     -- be strict in argument dictionaries
   | Opt_DmdTxDictSel              -- use a special demand transformer for dictionary selectors
   | Opt_Loopification                  -- See Note [Self-recursive tail calls]
   | Opt_CprAnal
   | Opt_WorkerWrapper

   -- Interface files
   | Opt_IgnoreInterfacePragmas
   | Opt_OmitInterfacePragmas
   | Opt_ExposeAllUnfoldings
   | Opt_WriteInterface -- forces .hi files to be written even with -fno-code

   -- profiling opts
   | Opt_AutoSccsOnIndividualCafs
   | Opt_ProfCountEntries

   -- misc opts
   | Opt_Pp
   | Opt_ForceRecomp
   | Opt_ExcessPrecision
   | Opt_EagerBlackHoling
   | Opt_NoHsMain
   | Opt_SplitObjs
   | Opt_SplitSections
   | Opt_StgStats
   | Opt_HideAllPackages
   | Opt_HideAllPluginPackages
   | Opt_PrintBindResult
   | Opt_Haddock
   | Opt_HaddockOptions
   | Opt_BreakOnException
   | Opt_BreakOnError
   | Opt_PrintEvldWithShow
   | Opt_PrintBindContents
   | Opt_GenManifest
   | Opt_EmbedManifest
   | Opt_SharedImplib
   | Opt_BuildingCabalPackage
   | Opt_IgnoreDotGhci
   | Opt_GhciSandbox
   | Opt_GhciHistory
   | Opt_HelpfulErrors
   | Opt_DeferTypeErrors
   | Opt_DeferTypedHoles
   | Opt_PIC
   | Opt_SccProfilingOn
   | Opt_Ticky
   | Opt_Ticky_Allocd
   | Opt_Ticky_LNE
   | Opt_Ticky_Dyn_Thunk
   | Opt_RPath
   | Opt_RelativeDynlibPaths
   | Opt_Hpc
   | Opt_FlatCache
   | Opt_ExternalInterpreter
   | Opt_OptimalApplicativeDo
   | Opt_VersionMacros

   -- PreInlining is on by default. The option is there just to see how
   -- bad things get if you turn it off!
   | Opt_SimplPreInlining

   -- output style opts
   | Opt_ErrorSpans -- Include full span info in error messages,
                    -- instead of just the start position.
   | Opt_PprCaseAsLet
   | Opt_PprShowTicks

   -- Suppress all coercions, them replacing with '...'
   | Opt_SuppressCoercions
   | Opt_SuppressVarKinds
   -- Suppress module id prefixes on variables.
   | Opt_SuppressModulePrefixes
   -- Suppress type applications.
   | Opt_SuppressTypeApplications
   -- Suppress info such as arity and unfoldings on identifiers.
   | Opt_SuppressIdInfo
   -- Suppress separate type signatures in core, but leave types on
   -- lambda bound vars
   | Opt_SuppressUnfoldings
   -- Suppress the details of even stable unfoldings
   | Opt_SuppressTypeSignatures
   -- Suppress unique ids on variables.
   -- Except for uniques, as some simplifier phases introduce new
   -- variables that have otherwise identical names.
   | Opt_SuppressUniques

   -- temporary flags
   | Opt_AutoLinkPackages
   | Opt_ImplicitImportQualified

   -- keeping stuff
   | Opt_KeepHiDiffs
   | Opt_KeepHcFiles
   | Opt_KeepSFiles
   | Opt_KeepTmpFiles
   | Opt_KeepRawTokenStream
   | Opt_KeepLlvmFiles
   | Opt_KeepHiFiles
   | Opt_KeepOFiles

   | Opt_BuildDynamicToo

   -- safe haskell flags
   | Opt_DistrustAllPackages
   | Opt_PackageTrust
   deriving (Eq, Show, Enum)


-- | Test whether a 'GeneralFlag' is set
gopt :: GeneralFlag -> DynFlags -> Bool
gopt f dflags  = fromEnum f `IntSet.member` generalFlags dflags

-- | Set a 'GeneralFlag'
gopt_set :: DynFlags -> GeneralFlag -> DynFlags
gopt_set dfs f = dfs{ generalFlags = IntSet.insert (fromEnum f)
                      (generalFlags dfs) }

-- | Unset a 'GeneralFlag'
gopt_unset :: DynFlags -> GeneralFlag -> DynFlags
gopt_unset dfs f = dfs{ generalFlags = IntSet.delete (fromEnum f)
                        (generalFlags dfs) }

-- | Used when outputting warnings: if a reason is given, it is
-- displayed. If a warning isn't controlled by a flag, this is made
-- explicit at the point of use.
data WarnReason = NoReason | Reason !WarningFlag

data WarningFlag =
-- See Note [Updating flag description in the User's Guide]
     Opt_WarnDuplicateExports
   | Opt_WarnDuplicateConstraints
   | Opt_WarnRedundantConstraints
   | Opt_WarnHiShadows
   | Opt_WarnImplicitPrelude
   | Opt_WarnIncompletePatterns
   | Opt_WarnIncompleteUniPatterns
   | Opt_WarnIncompletePatternsRecUpd
   | Opt_WarnOverflowedLiterals
   | Opt_WarnEmptyEnumerations
   | Opt_WarnMissingFields
   | Opt_WarnMissingImportList
   | Opt_WarnMissingMethods
   | Opt_WarnMissingSignatures
   | Opt_WarnMissingLocalSignatures
   | Opt_WarnNameShadowing
   | Opt_WarnOverlappingPatterns
   | Opt_WarnTypeDefaults
   | Opt_WarnMonomorphism
   | Opt_WarnUnusedTopBinds
   | Opt_WarnUnusedLocalBinds
   | Opt_WarnUnusedPatternBinds
   | Opt_WarnUnusedImports
   | Opt_WarnUnusedMatches
   | Opt_WarnUnusedTypePatterns
   | Opt_WarnUnusedForalls
   | Opt_WarnContextQuantification -- remove in 8.2
   | Opt_WarnWarningsDeprecations
   | Opt_WarnDeprecatedFlags
   | Opt_WarnAMP -- Introduced in GHC 7.8, obsolete since 7.10
   | Opt_WarnMissingMonadFailInstances -- since 8.0
   | Opt_WarnSemigroup -- since 8.0
   | Opt_WarnDodgyExports
   | Opt_WarnDodgyImports
   | Opt_WarnOrphans
   | Opt_WarnAutoOrphans
   | Opt_WarnIdentities
   | Opt_WarnTabs
   | Opt_WarnUnrecognisedPragmas
   | Opt_WarnDodgyForeignImports
   | Opt_WarnUnusedDoBind
   | Opt_WarnWrongDoBind
   | Opt_WarnAlternativeLayoutRuleTransitional
   | Opt_WarnUnsafe
   | Opt_WarnSafe
   | Opt_WarnTrustworthySafe
   | Opt_WarnMissedSpecs
   | Opt_WarnAllMissedSpecs
   | Opt_WarnUnsupportedCallingConventions
   | Opt_WarnUnsupportedLlvmVersion
   | Opt_WarnInlineRuleShadowing
   | Opt_WarnTypedHoles
   | Opt_WarnPartialTypeSignatures
   | Opt_WarnMissingExportedSignatures
   | Opt_WarnUntickedPromotedConstructors
   | Opt_WarnDerivingTypeable
   | Opt_WarnDeferredTypeErrors
   | Opt_WarnNonCanonicalMonadInstances   -- since 8.0
   | Opt_WarnNonCanonicalMonadFailInstances -- since 8.0
   | Opt_WarnNonCanonicalMonoidInstances  -- since 8.0
   | Opt_WarnMissingPatternSynonymSignatures -- since 8.0
   | Opt_WarnUnrecognisedWarningFlags     -- since 8.0
   | Opt_WarnSimplifiableClassConstraints -- Since 8.2
   deriving (Eq, Show, Enum)

-- | The various Safe Haskell modes
data SafeHaskellMode
   = Sf_None
   | Sf_Unsafe
   | Sf_Trustworthy
   | Sf_Safe
   deriving (Eq)

safeImportsOn :: DynFlags -> Bool
safeImportsOn dflags = safeHaskell dflags == Sf_Unsafe ||
                       safeHaskell dflags == Sf_Trustworthy ||
                       safeHaskell dflags == Sf_Safe

type FatalMessager = String -> IO ()

type LogAction = DynFlags -> WarnReason -> Severity -> SrcSpan ->
                 PprStyle -> MsgDoc -> IO ()

useUnicodeSyntax :: DynFlags -> Bool
useUnicodeSyntax = gopt Opt_PrintUnicodeSyntax
