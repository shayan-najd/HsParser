{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-cse #-}
-- -fno-cse is needed for GLOBAL_VAR's to behave properly

module DynFlags (
        defaultDynFlag,
        PlatformConstants(..),

        GeneralFlag(..),
        gopt,gopt_unset,gopt_set,

        DumpFlag(..),
        dopt,

        xopt,

        WarnReason(..),
        FatalMessager, LogAction,WarningFlag(..),
        useUnicodeSyntax,

        DynFlags(DynFlags,
                 pprUserLength,pprCols,useUnicode,settings,log_action
                ,verbosity,reverseErrors,extensionFlags,warningFlags,
                thisPackage),
        Settings(Settings,sPlatformConstants),
        wORD_SIZE ,
        cINT_SIZE,
        defaultLogActionHPrintDoc,
        safeImportsOn,
        tARGET_MIN_INT,
        tARGET_MAX_INT,
        tARGET_MAX_WORD
  ) where

#include "HsVersions.h"

import Module
import Panic
import qualified U.Pretty
import SrcLoc
import U.Outputable
import {-# SOURCE #-} U.ErrUtils ( Severity(..), MsgDoc)

import Data.Int
import Data.Word
import System.IO

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import qualified GHC.LanguageExtensions as LangExt

-- Note [Updating flag description in the User's Guide]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- If you modify anything in this file please make sure that your changes are
-- described in the User's Guide. Usually at least two sections need to be
-- updated:
--
--  * Flag Reference section generated from the modules in
--    utils/mkUserGuidePart/Options
--
--  * Flag description in docs/users_guide/using.rst provides a detailed
--    explanation of flags' usage.

-- Note [Supporting CLI completion]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- The command line interface completion (in for example bash) is an easy way
-- for the developer to learn what flags are available from GHC.
-- GHC helps by separating which flags are available when compiling with GHC,
-- and which flags are available when using GHCi.
-- A flag is assumed to either work in both these modes, or only in one of them.
-- When adding or changing a flag, please consider for which mode the flag will
-- have effect, and annotate it accordingly. For Flags use defFlag, defGhcFlag,
-- defGhciFlag, and for FlagSpec use flagSpec or flagGhciSpec.

-- Note [Adding a language extension]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- There are a few steps to adding (or removing) a language extension,
--
--  * Adding the extension to GHC.LanguageExtensions
--
--    The Extension type in libraries/ghc-boot-th/GHC/LanguageExtensions/Type.hs
--    is the canonical list of language extensions known by GHC.
--
--  * Adding a flag to DynFlags.xFlags
--
--    This is fairly self-explanatory. The name should be concise, memorable,
--    and consistent with any previous implementations of the similar idea in
--    other Haskell compilers.
--
--  * Adding the flag to the documentation
--
--    This is the same as any other flag. See
--    Note [Updating flag description in the User's Guide]
--
--  * Adding the flag to Cabal
--
--    The Cabal library has its own list of all language extensions supported
--    by all major compilers. This is the list that user code being uploaded
--    to Hackage is checked against to ensure language extension validity.
--    Consequently, it is very important that this list remains up-to-date.
--
--    To this end, there is a testsuite test (testsuite/tests/driver/T4437.hs)
--    whose job it is to ensure these GHC's extensions are consistent with
--    Cabal.
--
--    The recommended workflow is,
--
--     1. Temporarily add your new language extension to the
--        expectedGhcOnlyExtensions list in T4437 to ensure the test doesn't
--        break while Cabal is updated.
--
--     2. After your GHC change is accepted, submit a Cabal pull request adding
--        your new extension to Cabal's list (found in
--        Cabal/Language/Haskell/Extension.hs).
--
--     3. After your Cabal change is accepted, let the GHC developers know so
--        they can update the Cabal submodule and remove the extensions from
--        expectedGhcOnlyExtensions.
--
--  * Adding the flag to the GHC Wiki
--
--    There is a change log tracking language extension additions and removals
--    on the GHC wiki:  https://ghc.haskell.org/trac/ghc/wiki/LanguagePragmaHistory
--
--  See Trac #4437 and #8176.

-- -----------------------------------------------------------------------------
-- DynFlags

data DumpFlag
-- See Note [Updating flag description in the User's Guide]

   -- debugging flags
   = Opt_D_dump_cmm
   | Opt_D_dump_cmm_raw
   -- All of the cmm subflags (there are a lot!)  Automatically
   -- enabled if you run -ddump-cmm
   | Opt_D_dump_cmm_cfg
   | Opt_D_dump_cmm_cbe
   | Opt_D_dump_cmm_switch
   | Opt_D_dump_cmm_proc
   | Opt_D_dump_cmm_sink
   | Opt_D_dump_cmm_sp
   | Opt_D_dump_cmm_procmap
   | Opt_D_dump_cmm_split
   | Opt_D_dump_cmm_info
   | Opt_D_dump_cmm_cps
   -- end cmm subflags
   | Opt_D_dump_asm
   | Opt_D_dump_asm_native
   | Opt_D_dump_asm_liveness
   | Opt_D_dump_asm_regalloc
   | Opt_D_dump_asm_regalloc_stages
   | Opt_D_dump_asm_conflicts
   | Opt_D_dump_asm_stats
   | Opt_D_dump_asm_expanded
   | Opt_D_dump_llvm
   | Opt_D_dump_core_stats
   | Opt_D_dump_deriv
   | Opt_D_dump_ds
   | Opt_D_dump_foreign
   | Opt_D_dump_inlinings
   | Opt_D_dump_rule_firings
   | Opt_D_dump_rule_rewrites
   | Opt_D_dump_simpl_trace
   | Opt_D_dump_occur_anal
   | Opt_D_dump_parsed
   | Opt_D_dump_rn
   | Opt_D_dump_simpl
   | Opt_D_dump_simpl_iterations
   | Opt_D_dump_spec
   | Opt_D_dump_prep
   | Opt_D_dump_stg
   | Opt_D_dump_call_arity
   | Opt_D_dump_stranal
   | Opt_D_dump_str_signatures
   | Opt_D_dump_tc
   | Opt_D_dump_types
   | Opt_D_dump_rules
   | Opt_D_dump_cse
   | Opt_D_dump_worker_wrapper
   | Opt_D_dump_rn_trace
   | Opt_D_dump_rn_stats
   | Opt_D_dump_opt_cmm
   | Opt_D_dump_simpl_stats
   | Opt_D_dump_cs_trace -- Constraint solver in type checker
   | Opt_D_dump_tc_trace
   | Opt_D_dump_if_trace
   | Opt_D_dump_vt_trace
   | Opt_D_dump_splices
   | Opt_D_th_dec_file
   | Opt_D_dump_BCOs
   | Opt_D_dump_vect
   | Opt_D_dump_ticked
   | Opt_D_dump_rtti
   | Opt_D_source_stats
   | Opt_D_verbose_stg2stg
   | Opt_D_dump_hi
   | Opt_D_dump_hi_diffs
   | Opt_D_dump_mod_cycles
   | Opt_D_dump_mod_map
   | Opt_D_dump_view_pattern_commoning
   | Opt_D_verbose_core2core
   | Opt_D_dump_debug

   deriving (Eq, Show, Enum)

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

data Language = Haskell98 | Haskell2010
   deriving (Eq, Enum, Show)

-- | The various Safe Haskell modes
data SafeHaskellMode
   = Sf_None
   | Sf_Unsafe
   | Sf_Trustworthy
   | Sf_Safe
   deriving (Eq)

-- | Contains not only a collection of 'GeneralFlag's but also a plethora of
-- information relating to the compilation of a single file or GHC session
data DynFlags = DynFlags {
  settings              :: Settings,
  verbosity             :: Int,
  thisPackage           :: UnitId,
  dumpFlags             :: IntSet,
  generalFlags          :: IntSet,
  warningFlags          :: IntSet,
  safeHaskell           :: SafeHaskellMode,
  extensionFlags        :: IntSet,
  log_action            :: LogAction,
  pprUserLength         :: Int,
  pprCols               :: Int,
  useUnicode            :: Bool,
  reverseErrors         :: Bool
}


data Settings = Settings {
  sTargetPlatform        :: {-# UNPACK #-} !Int,    -- Filled in by SysTools
  sPlatformConstants     :: PlatformConstants
 }

targetPlatform :: DynFlags -> Int
targetPlatform dflags = sTargetPlatform (settings dflags)


type FatalMessager = String -> IO ()

type LogAction = DynFlags -> WarnReason -> Severity -> SrcSpan -> PprStyle ->
                 MsgDoc   -> IO ()

defaultLogActionHPrintDoc :: DynFlags -> Handle -> SDoc -> PprStyle -> IO ()
defaultLogActionHPrintDoc dflags h d sty
 = defaultLogActionHPutStrDoc dflags h (d $$ text "") sty
      -- Adds a newline

defaultLogActionHPutStrDoc :: DynFlags -> Handle -> SDoc -> PprStyle -> IO ()
defaultLogActionHPutStrDoc dflags h d sty
  = U.Pretty.printDoc_ U.Pretty.PageMode (pprCols dflags) h doc
  where   -- Don't add a newline at the end, so that successive
          -- calls to this log-action can output all on the same line
    doc = runSDoc d (initSDocContext dflags sty)




--- | Test whether a 'DumpFlag' is set
dopt :: DumpFlag -> DynFlags -> Bool
dopt f dflags = (fromEnum f `IntSet.member` dumpFlags dflags)
             || (verbosity dflags >= 4 && enableIfVerbose f)
    where enableIfVerbose Opt_D_dump_tc_trace               = False
          enableIfVerbose Opt_D_dump_rn_trace               = False
          enableIfVerbose Opt_D_dump_cs_trace               = False
          enableIfVerbose Opt_D_dump_if_trace               = False
          enableIfVerbose Opt_D_dump_vt_trace               = False
          enableIfVerbose Opt_D_dump_tc                     = False
          enableIfVerbose Opt_D_dump_rn                     = False
          enableIfVerbose Opt_D_dump_rn_stats               = False
          enableIfVerbose Opt_D_dump_hi_diffs               = False
          enableIfVerbose Opt_D_verbose_core2core           = False
          enableIfVerbose Opt_D_verbose_stg2stg             = False
          enableIfVerbose Opt_D_dump_splices                = False
          enableIfVerbose Opt_D_th_dec_file                 = False
          enableIfVerbose Opt_D_dump_rule_firings           = False
          enableIfVerbose Opt_D_dump_rule_rewrites          = False
          enableIfVerbose Opt_D_dump_simpl_trace            = False
          enableIfVerbose Opt_D_dump_rtti                   = False
          enableIfVerbose Opt_D_dump_inlinings              = False
          enableIfVerbose Opt_D_dump_core_stats             = False
          enableIfVerbose Opt_D_dump_asm_stats              = False
          enableIfVerbose Opt_D_dump_types                  = False
          enableIfVerbose Opt_D_dump_simpl_iterations       = False
          enableIfVerbose Opt_D_dump_ticked                 = False
          enableIfVerbose Opt_D_dump_view_pattern_commoning = False
          enableIfVerbose Opt_D_dump_mod_cycles             = False
          enableIfVerbose Opt_D_dump_mod_map                = False
          enableIfVerbose _                                 = True

-- | Test whether a 'GeneralFlag' is set
gopt :: GeneralFlag -> DynFlags -> Bool
gopt f dflags  = fromEnum f `IntSet.member` generalFlags dflags

-- | Set a 'GeneralFlag'
gopt_set :: DynFlags -> GeneralFlag -> DynFlags
gopt_set dfs f = dfs{ generalFlags = IntSet.insert (fromEnum f) (generalFlags dfs) }

-- | Unset a 'GeneralFlag'
gopt_unset :: DynFlags -> GeneralFlag -> DynFlags
gopt_unset dfs f = dfs{ generalFlags = IntSet.delete (fromEnum f) (generalFlags dfs) }


-- | Test whether a 'LangExt.Extension' is set
xopt :: LangExt.Extension -> DynFlags -> Bool
xopt f dflags = fromEnum f `IntSet.member` extensionFlags dflags


-- | An internal helper to check whether to use unicode syntax for output.
--
-- Note: You should very likely be using 'Outputable.unicodeSyntax' instead
-- of this function.
useUnicodeSyntax :: DynFlags -> Bool
useUnicodeSyntax = gopt Opt_PrintUnicodeSyntax

safeImportsOn :: DynFlags -> Bool
safeImportsOn dflags = safeHaskell dflags == Sf_Unsafe ||
                       safeHaskell dflags == Sf_Trustworthy ||
                       safeHaskell dflags == Sf_Safe

tARGET_MIN_INT, tARGET_MAX_INT, tARGET_MAX_WORD :: DynFlags -> Integer
tARGET_MIN_INT dflags
    = case targetPlatform dflags of
      4 -> toInteger (minBound :: Int32)
      8 -> toInteger (minBound :: Int64)
      w -> panic ("tARGET_MIN_INT: Unknown platformWordSize: " ++ show w)
tARGET_MAX_INT dflags
    = case targetPlatform dflags of
      4 -> toInteger (maxBound :: Int32)
      8 -> toInteger (maxBound :: Int64)
      w -> panic ("tARGET_MAX_INT: Unknown platformWordSize: " ++ show w)
tARGET_MAX_WORD dflags
    = case targetPlatform dflags of
      4 -> toInteger (maxBound :: Word32)
      8 -> toInteger (maxBound :: Word64)
      w -> panic ("tARGET_MAX_WORD: Unknown platformWordSize: " ++ show w)

defaultDynFlag :: DynFlags
defaultDynFlag = error "TODO:SHAYAN"


wORD_SIZE :: DynFlags -> Int
wORD_SIZE dflags = pc_WORD_SIZE (sPlatformConstants (settings dflags))


cINT_SIZE :: DynFlags -> Int
cINT_SIZE dflags = pc_CINT_SIZE (sPlatformConstants (settings dflags))

data PlatformConstants = PlatformConstants {
      pc_REP_CostCentreStack_mem_alloc :: Int
    , pc_REP_CostCentreStack_scc_count :: Int
    , pc_REP_StgEntCounter_allocs :: Int
    , pc_REP_StgEntCounter_allocd :: Int
    , pc_WORD_SIZE :: Int
    , pc_CINT_SIZE :: Int} deriving Read
