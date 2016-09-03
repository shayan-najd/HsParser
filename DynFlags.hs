{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}

-------------------------------------------------------------------------------
--
-- | Dynamic flags
--
-- Most flags are dynamic flags, which means they can change from compilation
-- to compilation using @OPTIONS_GHC@ pragmas, and in a multi-session GHC each
-- session can be using different dynamic flags. Dynamic flags can also be set
-- at the prompt in GHCi.
--
-- (c) The University of Glasgow 2005
--
-------------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-cse #-}
-- -fno-cse is needed for GLOBAL_VAR's to behave properly

module DynFlags (
        defaultDynFlag,
        -- * Dynamic flags and associated configuration types
        DumpFlag(..),
        GeneralFlag(..),
        WarningFlag(..), WarnReason(..),
        Language(..),
        PlatformConstants(..),
        FatalMessager, LogAction, FlushOut(..), FlushErr(..),
        ProfAuto(..),
        glasgowExtsFlags,
        dopt, dopt_set, dopt_unset,
        gopt, gopt_set, gopt_unset,
        wopt, wopt_set, wopt_unset,
        xopt, xopt_set, xopt_unset,
        lang_set,
        useUnicodeSyntax,
        whenGeneratingDynamicToo, ifGeneratingDynamicToo,
        whenCannotGenerateDynamicToo,
        DynFlags(..),
        HasDynFlags(..), ContainsDynFlags(..),
        RtsOptsEnabled(..),
        HscTarget(..), isObjectTarget, defaultObjectTarget,
        targetRetainsAllBindings,
        GhcMode(..), isOneShot,
        GhcLink(..), isNoLink,
        PackageFlag(..), PackageArg(..), ModRenaming(..),
        IgnorePackageFlag(..), TrustFlag(..),
        PkgConfRef(..),
        Option(..), showOpt,
        DynLibLoader(..),
        dynFlagDependencies,
        tablesNextToCode, mkTablesNextToCode,
        SigOf, getSigOf,
        makeDynFlagsConsistent,

        Way(..), mkBuildTag, wayRTSOnly, updateWays,
        wayGeneralFlags, wayUnsetGeneralFlags,

        -- ** Safe Haskell
        SafeHaskellMode(..),
        safeHaskellOn, safeImportsOn, safeLanguageOn, safeInferOn,
        packageTrustOn,
        safeDirectImpsReq, safeImplicitImpsReq,
        unsafeFlags, unsafeFlagsForInfer,

        -- ** System tool settings and locations
        Settings(..),
        targetPlatform, programName, projectVersion,
        ghcUsagePath, ghciUsagePath, topDir, tmpDir, rawSettings,
        versionedAppDir,
        extraGccViaCFlags, systemPackageConfig,
        pgm_L, pgm_P, pgm_F, pgm_c, pgm_s, pgm_a, pgm_l, pgm_dll, pgm_T,
        pgm_windres, pgm_libtool, pgm_lo, pgm_lc, pgm_i,
        opt_L, opt_P, opt_F, opt_c, opt_a, opt_l, opt_i,
        opt_windres, opt_lo, opt_lc,


        -- ** Manipulating DynFlags
        defaultWays,
        interpWays,
        interpreterProfiled, interpreterDynamic,
        initDynFlags,                   -- DynFlags -> IO DynFlags
        defaultFatalMessager,
        defaultLogActionHPrintDoc,
        defaultLogActionHPutStrDoc,
        defaultFlushOut,
        defaultFlushErr,

        getOpts,                        -- DynFlags -> (DynFlags -> [a]) -> [a]
        getVerbFlags,
        updOptLevel,
        setUnitId,



        languageExtensions,

        -- ** DynFlags C compiler options
        picCCOpts, picPOpts,

        -- * Compiler configuration suitable for display to the user
        compilerInfo,

#ifdef GHCI
        rtsIsProfiled,
#endif
        dynamicGhc,

#include "GHCConstantsHaskellExports.hs"
        bLOCK_SIZE_W,
        wORD_SIZE_IN_BITS,
        tAG_MASK,
        mAX_PTR_TAG,
        tARGET_MIN_INT, tARGET_MAX_INT, tARGET_MAX_WORD,

{-
        -- * SSE and AVX
        isSseEnabled,
        isSse2Enabled,
        isSse4_2Enabled,
        isAvxEnabled,
        isAvx2Enabled,
        isAvx512cdEnabled,
        isAvx512erEnabled,
        isAvx512fEnabled,
        isAvx512pfEnabled,
-}
        -- * Linker/compiler information
        LinkerInfo(..),
        CompilerInfo(..),
  ) where

#include "HsVersions.h"

import Platform
import PlatformConstants
import Module
import Config
import Panic
import U.Util
import U.Maybes
import U.MonadUtils
import qualified U.Pretty
import SrcLoc
import U.FastString
import U.Outputable
import Foreign.C        ( CInt(..) )
import System.IO.Unsafe ( unsafeDupablePerformIO )
import {-# SOURCE #-} ErrUtils ( Severity(..), MsgDoc, mkLocMessageAnn )

import System.IO.Unsafe ( unsafePerformIO )
import Data.IORef
import Control.Arrow ((&&&))
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Exception (throwIO)

import Data.Bits
import Data.Char
import Data.Int
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word
import System.FilePath
import System.Directory
import System.Environment (getEnv)
import System.IO
import System.IO.Error
import Text.ParserCombinators.ReadP hiding (char)
import Text.ParserCombinators.ReadP as R

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import GHC.Foreign (withCString, peekCString)
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

instance Outputable Language where
    ppr = text . show

-- | The various Safe Haskell modes
data SafeHaskellMode
   = Sf_None
   | Sf_Unsafe
   | Sf_Trustworthy
   | Sf_Safe
   deriving (Eq)

instance Show SafeHaskellMode where
    show Sf_None         = "None"
    show Sf_Unsafe       = "Unsafe"
    show Sf_Trustworthy  = "Trustworthy"
    show Sf_Safe         = "Safe"

instance Outputable SafeHaskellMode where
    ppr = text . show

type SigOf = Map ModuleName Module

getSigOf :: DynFlags -> ModuleName -> Maybe Module
getSigOf dflags n = Map.lookup n (sigOf dflags)

-- | Contains not only a collection of 'GeneralFlag's but also a plethora of
-- information relating to the compilation of a single file or GHC session
data DynFlags = DynFlags {
  ghcMode               :: GhcMode,
  ghcLink               :: GhcLink,
  hscTarget             :: HscTarget,
  settings              :: Settings,
  -- See Note [Signature parameters in TcGblEnv and DynFlags]
  sigOf                 :: SigOf,       -- ^ Compiling an hs-boot against impl.
  verbosity             :: Int,         -- ^ Verbosity level: see Note [Verbosity levels]
  optLevel              :: Int,         -- ^ Optimisation level
  debugLevel            :: Int,         -- ^ How much debug information to produce
  simplPhases           :: Int,         -- ^ Number of simplifier phases
  maxSimplIterations    :: Int,         -- ^ Max simplifier iterations
  maxPmCheckIterations  :: Int,         -- ^ Max no iterations for pm checking
  ruleCheck             :: Maybe String,
  strictnessBefore      :: [Int],       -- ^ Additional demand analysis

  parMakeCount          :: Maybe Int,   -- ^ The number of modules to compile in parallel
                                        --   in --make mode, where Nothing ==> compile as
                                        --   many in parallel as there are CPUs.

  enableTimeStats       :: Bool,        -- ^ Enable RTS timing statistics?
  ghcHeapSize           :: Maybe Int,   -- ^ The heap size to set.

  maxRelevantBinds      :: Maybe Int,   -- ^ Maximum number of bindings from the type envt
                                        --   to show in type error messages
  maxUncoveredPatterns  :: Int,         -- ^ Maximum number of unmatched patterns to show
                                        --   in non-exhaustiveness warnings
  simplTickFactor       :: Int,         -- ^ Multiplier for simplifier ticks
  specConstrThreshold   :: Maybe Int,   -- ^ Threshold for SpecConstr
  specConstrCount       :: Maybe Int,   -- ^ Max number of specialisations for any one function
  specConstrRecursive   :: Int,         -- ^ Max number of specialisations for recursive types
                                        --   Not optional; otherwise ForceSpecConstr can diverge.
  liberateCaseThreshold :: Maybe Int,   -- ^ Threshold for LiberateCase
  floatLamArgs          :: Maybe Int,   -- ^ Arg count for lambda floating
                                        --   See CoreMonad.FloatOutSwitches

  historySize           :: Int,         -- ^ Simplification history size

  importPaths           :: [FilePath],
  mainModIs             :: Module,
  mainFunIs             :: Maybe String,

  thisPackage           :: UnitId,   -- ^ key of package currently being compiled

  -- ways
  ways                  :: [Way],       -- ^ Way flags from the command line
  buildTag              :: String,      -- ^ The global \"way\" (e.g. \"p\" for prof)
  rtsBuildTag           :: String,      -- ^ The RTS \"way\"

  -- For object splitting
  splitInfo             :: Maybe (String,Int),

  -- paths etc.
  objectDir             :: Maybe String,
  dylibInstallName      :: Maybe String,
  hiDir                 :: Maybe String,
  stubDir               :: Maybe String,
  dumpDir               :: Maybe String,

  objectSuf             :: String,
  hcSuf                 :: String,
  hiSuf                 :: String,

  canGenerateDynamicToo :: IORef Bool,
  dynObjectSuf          :: String,
  dynHiSuf              :: String,

  -- Packages.isDllName needs to know whether a call is within a
  -- single DLL or not. Normally it does this by seeing if the call
  -- is to the same package, but for the ghc package, we split the
  -- package between 2 DLLs. The dllSplit tells us which sets of
  -- modules are in which package.
  dllSplitFile          :: Maybe FilePath,
  dllSplit              :: Maybe [Set String],

  outputFile            :: Maybe String,
  dynOutputFile         :: Maybe String,
  outputHi              :: Maybe String,
  dynLibLoader          :: DynLibLoader,

  -- | This is set by 'DriverPipeline.runPipeline' based on where
  --    its output is going.
  dumpPrefix            :: Maybe FilePath,

  -- | Override the 'dumpPrefix' set by 'DriverPipeline.runPipeline'.
  --    Set by @-ddump-file-prefix@
  dumpPrefixForce       :: Maybe FilePath,

  ldInputs              :: [Option],

  includePaths          :: [String],
  libraryPaths          :: [String],
  frameworkPaths        :: [String],    -- used on darwin only
  cmdlineFrameworks     :: [String],    -- ditto

  rtsOpts               :: Maybe String,
  rtsOptsEnabled        :: RtsOptsEnabled,
  rtsOptsSuggestions    :: Bool,

  hpcDir                :: String,      -- ^ Path to store the .mix files

  -- Plugins
  pluginModNames        :: [ModuleName],
  pluginModNameOpts     :: [(ModuleName,String)],
  frontendPluginOpts    :: [String],

  --  For ghc -M
  depMakefile           :: FilePath,
  depIncludePkgDeps     :: Bool,
  depExcludeMods        :: [ModuleName],
  depSuffixes           :: [String],

  --  Package flags
  extraPkgConfs         :: [PkgConfRef] -> [PkgConfRef],
        -- ^ The @-package-db@ flags given on the command line, in the order
        -- they appeared.

  ignorePackageFlags    :: [IgnorePackageFlag],
        -- ^ The @-ignore-package@ flags from the command line
  packageFlags          :: [PackageFlag],
        -- ^ The @-package@ and @-hide-package@ flags from the command-line
  pluginPackageFlags    :: [PackageFlag],
        -- ^ The @-plugin-package-id@ flags from command line
  trustFlags            :: [TrustFlag],
        -- ^ The @-trust@ and @-distrust@ flags
  packageEnv            :: Maybe FilePath,
        -- ^ Filepath to the package environment file (if overriding default)

  -- Package state
  -- NB. do not modify this field, it is calculated by
  -- Packages.initPackages

  -- Temporary files
  -- These have to be IORefs, because the defaultCleanupHandler needs to
  -- know what to clean when an exception happens
  filesToClean          :: IORef [FilePath],
  dirsToClean           :: IORef (Map FilePath FilePath),
  filesToNotIntermediateClean :: IORef [FilePath],
  -- The next available suffix to uniquely name a temp file, updated atomically
  nextTempSuffix        :: IORef Int,

  -- Names of files which were generated from -ddump-to-file; used to
  -- track which ones we need to truncate because it's our first run
  -- through
  generatedDumps        :: IORef (Set FilePath),

  -- hsc dynamic flags
  dumpFlags             :: IntSet,
  generalFlags          :: IntSet,
  warningFlags          :: IntSet,
  -- Don't change this without updating extensionFlags:
  language              :: Maybe Language,
  -- | Safe Haskell mode
  safeHaskell           :: SafeHaskellMode,
  safeInfer             :: Bool,
  safeInferred          :: Bool,
  -- We store the location of where some extension and flags were turned on so
  -- we can produce accurate error messages when Safe Haskell fails due to
  -- them.
  thOnLoc               :: SrcSpan,
  newDerivOnLoc         :: SrcSpan,
  overlapInstLoc        :: SrcSpan,
  incoherentOnLoc       :: SrcSpan,
  pkgTrustOnLoc         :: SrcSpan,
  warnSafeOnLoc         :: SrcSpan,
  warnUnsafeOnLoc       :: SrcSpan,
  trustworthyOnLoc      :: SrcSpan,
  -- Don't change this without updating extensionFlags:
  extensions            :: [OnOff LangExt.Extension],
  -- extensionFlags should always be equal to
  --     flattenExtensionFlags language extensions
  -- LangExt.Extension is defined in libraries/ghc-boot so that it can be used
  -- by template-haskell
  extensionFlags        :: IntSet,

  -- Unfolding control
  -- See Note [Discounts and thresholds] in CoreUnfold
  ufCreationThreshold   :: Int,
  ufUseThreshold        :: Int,
  ufFunAppDiscount      :: Int,
  ufDictDiscount        :: Int,
  ufKeenessFactor       :: Float,
  ufDearOp              :: Int,

  maxWorkerArgs         :: Int,

  ghciHistSize          :: Int,

  -- | MsgDoc output action: use "ErrUtils" instead of this if you can
  log_action            :: LogAction,
  flushOut              :: FlushOut,
  flushErr              :: FlushErr,

  haddockOptions        :: Maybe String,

  -- | GHCi scripts specified by -ghci-script, in reverse order
  ghciScripts           :: [String],

  -- Output style options
  pprUserLength         :: Int,
  pprCols               :: Int,
  traceLevel            :: Int, -- Standard level is 1. Less verbose is 0.

  useUnicode      :: Bool,

  -- | what kind of {-# SCC #-} to add automatically
  profAuto              :: ProfAuto,

  interactivePrint      :: Maybe String,

  nextWrapperNum        :: IORef (ModuleEnv Int),

  -- | Machine dependant flags (-m<blah> stuff)
  sseVersion            :: Maybe SseVersion,
  avx                   :: Bool,
  avx2                  :: Bool,
  avx512cd              :: Bool, -- Enable AVX-512 Conflict Detection Instructions.
  avx512er              :: Bool, -- Enable AVX-512 Exponential and Reciprocal Instructions.
  avx512f               :: Bool, -- Enable AVX-512 instructions.
  avx512pf              :: Bool, -- Enable AVX-512 PreFetch Instructions.

  -- | Run-time linker information (what options we need, etc.)
  rtldInfo              :: IORef (Maybe LinkerInfo),

  -- | Run-time compiler information
  rtccInfo              :: IORef (Maybe CompilerInfo),

  -- Constants used to control the amount of optimization done.

  -- | Max size, in bytes, of inline array allocations.
  maxInlineAllocSize    :: Int,

  -- | Only inline memcpy if it generates no more than this many
  -- pseudo (roughly: Cmm) instructions.
  maxInlineMemcpyInsns  :: Int,

  -- | Only inline memset if it generates no more than this many
  -- pseudo (roughly: Cmm) instructions.
  maxInlineMemsetInsns  :: Int,

  -- | Reverse the order of error messages in GHC/GHCi
  reverseErrors :: Bool,

  -- | Unique supply configuration for testing build determinism
  initialUnique         :: Int,
  uniqueIncrement       :: Int
}

class HasDynFlags m where
    getDynFlags :: m DynFlags

{- It would be desirable to have the more generalised

  instance (MonadTrans t, Monad m, HasDynFlags m) => HasDynFlags (t m) where
      getDynFlags = lift getDynFlags

instance definition. However, that definition would overlap with the
`HasDynFlags (GhcT m)` instance. Instead we define instances for a
couple of common Monad transformers explicitly. -}

instance (Monoid a, Monad m, HasDynFlags m) => HasDynFlags (WriterT a m) where
    getDynFlags = lift getDynFlags

instance (Monad m, HasDynFlags m) => HasDynFlags (ReaderT a m) where
    getDynFlags = lift getDynFlags

instance (Monad m, HasDynFlags m) => HasDynFlags (MaybeT m) where
    getDynFlags = lift getDynFlags

instance (Monad m, HasDynFlags m) => HasDynFlags (ExceptT e m) where
    getDynFlags = lift getDynFlags

class ContainsDynFlags t where
    extractDynFlags :: t -> DynFlags

data ProfAuto
  = NoProfAuto         -- ^ no SCC annotations added
  | ProfAutoAll        -- ^ top-level and nested functions are annotated
  | ProfAutoTop        -- ^ top-level functions annotated only
  | ProfAutoExports    -- ^ exported functions annotated only
  | ProfAutoCalls      -- ^ annotate call-sites
  deriving (Eq,Enum)

data Settings = Settings {
  sTargetPlatform        :: Platform,    -- Filled in by SysTools
  sGhcUsagePath          :: FilePath,    -- Filled in by SysTools
  sGhciUsagePath         :: FilePath,    -- ditto
  sTopDir                :: FilePath,
  sTmpDir                :: String,      -- no trailing '/'
  sProgramName           :: String,
  sProjectVersion        :: String,
  -- You shouldn't need to look things up in rawSettings directly.
  -- They should have their own fields instead.
  sRawSettings           :: [(String, String)],
  sExtraGccViaCFlags     :: [String],
  sSystemPackageConfig   :: FilePath,
  sLdSupportsCompactUnwind :: Bool,
  sLdSupportsBuildId       :: Bool,
  sLdSupportsFilelist      :: Bool,
  sLdIsGnuLd               :: Bool,
  -- commands for particular phases
  sPgm_L                 :: String,
  sPgm_P                 :: (String,[Option]),
  sPgm_F                 :: String,
  sPgm_c                 :: (String,[Option]),
  sPgm_s                 :: (String,[Option]),
  sPgm_a                 :: (String,[Option]),
  sPgm_l                 :: (String,[Option]),
  sPgm_dll               :: (String,[Option]),
  sPgm_T                 :: String,
  sPgm_windres           :: String,
  sPgm_libtool           :: String,
  sPgm_lo                :: (String,[Option]), -- LLVM: opt llvm optimiser
  sPgm_lc                :: (String,[Option]), -- LLVM: llc static compiler
  sPgm_i                 :: String,
  -- options for particular phases
  sOpt_L                 :: [String],
  sOpt_P                 :: [String],
  sOpt_F                 :: [String],
  sOpt_c                 :: [String],
  sOpt_a                 :: [String],
  sOpt_l                 :: [String],
  sOpt_windres           :: [String],
  sOpt_lo                :: [String], -- LLVM: llvm optimiser
  sOpt_lc                :: [String], -- LLVM: llc static compiler
  sOpt_i                 :: [String], -- iserv options

  sPlatformConstants     :: PlatformConstants
 }

targetPlatform :: DynFlags -> Platform
targetPlatform dflags = sTargetPlatform (settings dflags)
programName :: DynFlags -> String
programName dflags = sProgramName (settings dflags)
projectVersion :: DynFlags -> String
projectVersion dflags = sProjectVersion (settings dflags)
ghcUsagePath          :: DynFlags -> FilePath
ghcUsagePath dflags = sGhcUsagePath (settings dflags)
ghciUsagePath         :: DynFlags -> FilePath
ghciUsagePath dflags = sGhciUsagePath (settings dflags)
topDir                :: DynFlags -> FilePath
topDir dflags = sTopDir (settings dflags)
tmpDir                :: DynFlags -> String
tmpDir dflags = sTmpDir (settings dflags)
rawSettings           :: DynFlags -> [(String, String)]
rawSettings dflags = sRawSettings (settings dflags)
extraGccViaCFlags     :: DynFlags -> [String]
extraGccViaCFlags dflags = sExtraGccViaCFlags (settings dflags)
systemPackageConfig   :: DynFlags -> FilePath
systemPackageConfig dflags = sSystemPackageConfig (settings dflags)
pgm_L                 :: DynFlags -> String
pgm_L dflags = sPgm_L (settings dflags)
pgm_P                 :: DynFlags -> (String,[Option])
pgm_P dflags = sPgm_P (settings dflags)
pgm_F                 :: DynFlags -> String
pgm_F dflags = sPgm_F (settings dflags)
pgm_c                 :: DynFlags -> (String,[Option])
pgm_c dflags = sPgm_c (settings dflags)
pgm_s                 :: DynFlags -> (String,[Option])
pgm_s dflags = sPgm_s (settings dflags)
pgm_a                 :: DynFlags -> (String,[Option])
pgm_a dflags = sPgm_a (settings dflags)
pgm_l                 :: DynFlags -> (String,[Option])
pgm_l dflags = sPgm_l (settings dflags)
pgm_dll               :: DynFlags -> (String,[Option])
pgm_dll dflags = sPgm_dll (settings dflags)
pgm_T                 :: DynFlags -> String
pgm_T dflags = sPgm_T (settings dflags)
pgm_windres           :: DynFlags -> String
pgm_windres dflags = sPgm_windres (settings dflags)
pgm_libtool           :: DynFlags -> String
pgm_libtool dflags = sPgm_libtool (settings dflags)
pgm_lo                :: DynFlags -> (String,[Option])
pgm_lo dflags = sPgm_lo (settings dflags)
pgm_lc                :: DynFlags -> (String,[Option])
pgm_lc dflags = sPgm_lc (settings dflags)
pgm_i                 :: DynFlags -> String
pgm_i dflags = sPgm_i (settings dflags)
opt_L                 :: DynFlags -> [String]
opt_L dflags = sOpt_L (settings dflags)
opt_P                 :: DynFlags -> [String]
opt_P dflags = concatMap (wayOptP (targetPlatform dflags)) (ways dflags)
            ++ sOpt_P (settings dflags)
opt_F                 :: DynFlags -> [String]
opt_F dflags = sOpt_F (settings dflags)
opt_c                 :: DynFlags -> [String]
opt_c dflags = concatMap (wayOptc (targetPlatform dflags)) (ways dflags)
            ++ sOpt_c (settings dflags)
opt_a                 :: DynFlags -> [String]
opt_a dflags = sOpt_a (settings dflags)
opt_l                 :: DynFlags -> [String]
opt_l dflags = concatMap (wayOptl (targetPlatform dflags)) (ways dflags)
            ++ sOpt_l (settings dflags)
opt_windres           :: DynFlags -> [String]
opt_windres dflags = sOpt_windres (settings dflags)
opt_lo                :: DynFlags -> [String]
opt_lo dflags = sOpt_lo (settings dflags)
opt_lc                :: DynFlags -> [String]
opt_lc dflags = sOpt_lc (settings dflags)
opt_i                 :: DynFlags -> [String]
opt_i dflags = sOpt_i (settings dflags)

-- | The directory for this version of ghc in the user's app directory
-- (typically something like @~/.ghc/x86_64-linux-7.6.3@)
--
versionedAppDir :: DynFlags -> MaybeT IO FilePath
versionedAppDir dflags = do
  -- Make sure we handle the case the HOME isn't set (see #11678)
  appdir <- tryMaybeT $ getAppUserDataDirectory (programName dflags)
  return $ appdir </> versionedFilePath dflags

-- | A filepath like @x86_64-linux-7.6.3@ with the platform string to use when
-- constructing platform-version-dependent files that need to co-exist.
--
versionedFilePath :: DynFlags -> FilePath
versionedFilePath dflags =     TARGET_ARCH
                        ++ '-':TARGET_OS
                        ++ '-':projectVersion dflags
  -- NB: This functionality is reimplemented in Cabal, so if you
  -- change it, be sure to update Cabal.

-- | The target code type of the compilation (if any).
--
-- Whenever you change the target, also make sure to set 'ghcLink' to
-- something sensible.
--
-- 'HscNothing' can be used to avoid generating any output, however, note
-- that:
--
--  * If a program uses Template Haskell the typechecker may try to run code
--    from an imported module.  This will fail if no code has been generated
--    for this module.  You can use 'GHC.needsTemplateHaskell' to detect
--    whether this might be the case and choose to either switch to a
--    different target or avoid typechecking such modules.  (The latter may be
--    preferable for security reasons.)
--
data HscTarget
  = HscC           -- ^ Generate C code.
  | HscAsm         -- ^ Generate assembly using the native code generator.
  | HscLlvm        -- ^ Generate assembly using the llvm code generator.
  | HscInterpreted -- ^ Generate bytecode.  (Requires 'LinkInMemory')
  | HscNothing     -- ^ Don't generate any code.  See notes above.
  deriving (Eq, Show)

-- | Will this target result in an object file on the disk?
isObjectTarget :: HscTarget -> Bool
isObjectTarget HscC     = True
isObjectTarget HscAsm   = True
isObjectTarget HscLlvm  = True
isObjectTarget _        = False

-- | Does this target retain *all* top-level bindings for a module,
-- rather than just the exported bindings, in the TypeEnv and compiled
-- code (if any)?  In interpreted mode we do this, so that GHCi can
-- call functions inside a module.  In HscNothing mode we also do it,
-- so that Haddock can get access to the GlobalRdrEnv for a module
-- after typechecking it.
targetRetainsAllBindings :: HscTarget -> Bool
targetRetainsAllBindings HscInterpreted = True
targetRetainsAllBindings HscNothing     = True
targetRetainsAllBindings _              = False

-- | The 'GhcMode' tells us whether we're doing multi-module
-- compilation (controlled via the "GHC" API) or one-shot
-- (single-module) compilation.  This makes a difference primarily to
-- the "Finder": in one-shot mode we look for interface files for
-- imported modules, but in multi-module mode we look for source files
-- in order to check whether they need to be recompiled.
data GhcMode
  = CompManager         -- ^ @\-\-make@, GHCi, etc.
  | OneShot             -- ^ @ghc -c Foo.hs@
  | MkDepend            -- ^ @ghc -M@, see "Finder" for why we need this
  deriving Eq

instance Outputable GhcMode where
  ppr CompManager = text "CompManager"
  ppr OneShot     = text "OneShot"
  ppr MkDepend    = text "MkDepend"

isOneShot :: GhcMode -> Bool
isOneShot OneShot = True
isOneShot _other  = False

-- | What to do in the link step, if there is one.
data GhcLink
  = NoLink              -- ^ Don't link at all
  | LinkBinary          -- ^ Link object code into a binary
  | LinkInMemory        -- ^ Use the in-memory dynamic linker (works for both
                        --   bytecode and object code).
  | LinkDynLib          -- ^ Link objects into a dynamic lib (DLL on Windows, DSO on ELF platforms)
  | LinkStaticLib       -- ^ Link objects into a static lib
  deriving (Eq, Show)

isNoLink :: GhcLink -> Bool
isNoLink NoLink = True
isNoLink _      = False

-- | We accept flags which make packages visible, but how they select
-- the package varies; this data type reflects what selection criterion
-- is used.
data PackageArg =
      PackageArg String    -- ^ @-package@, by 'PackageName'
    | UnitIdArg String     -- ^ @-package-id@, by 'UnitId'
  deriving (Eq, Show)

-- | Represents the renaming that may be associated with an exposed
-- package, e.g. the @rns@ part of @-package "foo (rns)"@.
--
-- Here are some example parsings of the package flags (where
-- a string literal is punned to be a 'ModuleName':
--
--      * @-package foo@ is @ModRenaming True []@
--      * @-package foo ()@ is @ModRenaming False []@
--      * @-package foo (A)@ is @ModRenaming False [("A", "A")]@
--      * @-package foo (A as B)@ is @ModRenaming False [("A", "B")]@
--      * @-package foo with (A as B)@ is @ModRenaming True [("A", "B")]@
data ModRenaming = ModRenaming {
    modRenamingWithImplicit :: Bool, -- ^ Bring all exposed modules into scope?
    modRenamings :: [(ModuleName, ModuleName)] -- ^ Bring module @m@ into scope
                                               --   under name @n@.
  } deriving (Eq)

-- | Flags for manipulating the set of non-broken packages.
newtype IgnorePackageFlag = IgnorePackage String -- ^ @-ignore-package@
  deriving (Eq)

-- | Flags for manipulating package trust.
data TrustFlag
  = TrustPackage    String -- ^ @-trust@
  | DistrustPackage String -- ^ @-distrust@
  deriving (Eq)

-- | Flags for manipulating packages visibility.
data PackageFlag
  = ExposePackage   String PackageArg ModRenaming -- ^ @-package@, @-package-id@
  | HidePackage     String -- ^ @-hide-package@
  deriving (Eq)
-- NB: equality instance is used by InteractiveUI to test if
-- package flags have changed.

defaultHscTarget :: Platform -> HscTarget
defaultHscTarget = defaultObjectTarget

-- | The 'HscTarget' value corresponding to the default way to create
-- object files on the current platform.
defaultObjectTarget :: Platform -> HscTarget
defaultObjectTarget platform
  | platformUnregisterised platform     =  HscC
  | cGhcWithNativeCodeGen == "YES"      =  HscAsm
  | otherwise                           =  HscLlvm

tablesNextToCode :: DynFlags -> Bool
tablesNextToCode dflags
    = mkTablesNextToCode (platformUnregisterised (targetPlatform dflags))

-- Determines whether we will be compiling
-- info tables that reside just before the entry code, or with an
-- indirection to the entry code.  See TABLES_NEXT_TO_CODE in
-- includes/rts/storage/InfoTables.h.
mkTablesNextToCode :: Bool -> Bool
mkTablesNextToCode unregisterised
    = not unregisterised && cGhcEnableTablesNextToCode == "YES"

data DynLibLoader
  = Deployable
  | SystemDependent
  deriving Eq

data RtsOptsEnabled = RtsOptsNone | RtsOptsSafeOnly | RtsOptsAll
  deriving (Show)

-----------------------------------------------------------------------------
-- Ways

-- The central concept of a "way" is that all objects in a given
-- program must be compiled in the same "way".  Certain options change
-- parameters of the virtual machine, eg. profiling adds an extra word
-- to the object header, so profiling objects cannot be linked with
-- non-profiling objects.

-- After parsing the command-line options, we determine which "way" we
-- are building - this might be a combination way, eg. profiling+threaded.

-- We then find the "build-tag" associated with this way, and this
-- becomes the suffix used to find .hi files and libraries used in
-- this compilation.

data Way
  = WayCustom String -- for GHC API clients building custom variants
  | WayThreaded
  | WayDebug
  | WayProf
  | WayEventLog
  | WayDyn
  deriving (Eq, Ord, Show)

allowed_combination :: [Way] -> Bool
allowed_combination way = and [ x `allowedWith` y
                              | x <- way, y <- way, x < y ]
  where
        -- Note ordering in these tests: the left argument is
        -- <= the right argument, according to the Ord instance
        -- on Way above.

        -- dyn is allowed with everything
        _ `allowedWith` WayDyn                  = True
        WayDyn `allowedWith` _                  = True

        -- debug is allowed with everything
        _ `allowedWith` WayDebug                = True
        WayDebug `allowedWith` _                = True

        (WayCustom {}) `allowedWith` _          = True
        WayThreaded `allowedWith` WayProf       = True
        WayThreaded `allowedWith` WayEventLog   = True
        _ `allowedWith` _                       = False

mkBuildTag :: [Way] -> String
mkBuildTag ways = concat (intersperse "_" (map wayTag ways))

wayTag :: Way -> String
wayTag (WayCustom xs) = xs
wayTag WayThreaded = "thr"
wayTag WayDebug    = "debug"
wayTag WayDyn      = "dyn"
wayTag WayProf     = "p"
wayTag WayEventLog = "l"

wayRTSOnly :: Way -> Bool
wayRTSOnly (WayCustom {}) = False
wayRTSOnly WayThreaded = True
wayRTSOnly WayDebug    = True
wayRTSOnly WayDyn      = False
wayRTSOnly WayProf     = False
wayRTSOnly WayEventLog = True

wayDesc :: Way -> String
wayDesc (WayCustom xs) = xs
wayDesc WayThreaded = "Threaded"
wayDesc WayDebug    = "Debug"
wayDesc WayDyn      = "Dynamic"
wayDesc WayProf     = "Profiling"
wayDesc WayEventLog = "RTS Event Logging"

-- Turn these flags on when enabling this way
wayGeneralFlags :: Platform -> Way -> [GeneralFlag]
wayGeneralFlags _ (WayCustom {}) = []
wayGeneralFlags _ WayThreaded = []
wayGeneralFlags _ WayDebug    = []
wayGeneralFlags _ WayDyn      = [Opt_PIC]
    -- We could get away without adding -fPIC when compiling the
    -- modules of a program that is to be linked with -dynamic; the
    -- program itself does not need to be position-independent, only
    -- the libraries need to be.  HOWEVER, GHCi links objects into a
    -- .so before loading the .so using the system linker.  Since only
    -- PIC objects can be linked into a .so, we have to compile even
    -- modules of the main program with -fPIC when using -dynamic.
wayGeneralFlags _ WayProf     = [Opt_SccProfilingOn]
wayGeneralFlags _ WayEventLog = []

-- Turn these flags off when enabling this way
wayUnsetGeneralFlags :: Platform -> Way -> [GeneralFlag]
wayUnsetGeneralFlags _ (WayCustom {}) = []
wayUnsetGeneralFlags _ WayThreaded = []
wayUnsetGeneralFlags _ WayDebug    = []
wayUnsetGeneralFlags _ WayDyn      = [-- There's no point splitting objects
                                      -- when we're going to be dynamically
                                      -- linking. Plus it breaks compilation
                                      -- on OSX x86.
                                      Opt_SplitObjs,
                                      -- If splitobjs wasn't useful for this,
                                      -- assume sections aren't either.
                                      Opt_SplitSections]
wayUnsetGeneralFlags _ WayProf     = []
wayUnsetGeneralFlags _ WayEventLog = []

wayOptc :: Platform -> Way -> [String]
wayOptc _ (WayCustom {}) = []
wayOptc platform WayThreaded = case platformOS platform of
                               OSOpenBSD -> ["-pthread"]
                               OSNetBSD  -> ["-pthread"]
                               _         -> []
wayOptc _ WayDebug      = []
wayOptc _ WayDyn        = []
wayOptc _ WayProf       = ["-DPROFILING"]
wayOptc _ WayEventLog   = ["-DTRACING"]

wayOptl :: Platform -> Way -> [String]
wayOptl _ (WayCustom {}) = []
wayOptl platform WayThreaded =
        case platformOS platform of
        -- FreeBSD's default threading library is the KSE-based M:N libpthread,
        -- which GHC has some problems with.  It's currently not clear whether
        -- the problems are our fault or theirs, but it seems that using the
        -- alternative 1:1 threading library libthr works around it:
        OSFreeBSD  -> ["-lthr"]
        OSOpenBSD  -> ["-pthread"]
        OSNetBSD   -> ["-pthread"]
        _          -> []
wayOptl _ WayDebug      = []
wayOptl _ WayDyn        = []
wayOptl _ WayProf       = []
wayOptl _ WayEventLog   = []

wayOptP :: Platform -> Way -> [String]
wayOptP _ (WayCustom {}) = []
wayOptP _ WayThreaded = []
wayOptP _ WayDebug    = []
wayOptP _ WayDyn      = []
wayOptP _ WayProf     = ["-DPROFILING"]
wayOptP _ WayEventLog = ["-DTRACING"]

whenGeneratingDynamicToo :: MonadIO m => DynFlags -> m () -> m ()
whenGeneratingDynamicToo dflags f = ifGeneratingDynamicToo dflags f (return ())

ifGeneratingDynamicToo :: MonadIO m => DynFlags -> m a -> m a -> m a
ifGeneratingDynamicToo dflags f g = generateDynamicTooConditional dflags f g g

whenCannotGenerateDynamicToo :: MonadIO m => DynFlags -> m () -> m ()
whenCannotGenerateDynamicToo dflags f
    = ifCannotGenerateDynamicToo dflags f (return ())

ifCannotGenerateDynamicToo :: MonadIO m => DynFlags -> m a -> m a -> m a
ifCannotGenerateDynamicToo dflags f g
    = generateDynamicTooConditional dflags g f g

generateDynamicTooConditional :: MonadIO m
                              => DynFlags -> m a -> m a -> m a -> m a
generateDynamicTooConditional dflags canGen cannotGen notTryingToGen
    = if gopt Opt_BuildDynamicToo dflags
      then do let ref = canGenerateDynamicToo dflags
              b <- liftIO $ readIORef ref
              if b then canGen else cannotGen
      else notTryingToGen


-----------------------------------------------------------------------------

-- | Used by 'GHC.runGhc' to partially initialize a new 'DynFlags' value
initDynFlags :: DynFlags -> IO DynFlags
initDynFlags dflags = do
 let -- We can't build with dynamic-too on Windows, as labels before
     -- the fork point are different depending on whether we are
     -- building dynamically or not.
     platformCanGenerateDynamicToo
         = platformOS (targetPlatform dflags) /= OSMinGW32
 refCanGenerateDynamicToo <- newIORef platformCanGenerateDynamicToo
 refNextTempSuffix <- newIORef 0
 refFilesToClean <- newIORef []
 refDirsToClean <- newIORef Map.empty
 refFilesToNotIntermediateClean <- newIORef []
 refGeneratedDumps <- newIORef Set.empty
 refRtldInfo <- newIORef Nothing
 refRtccInfo <- newIORef Nothing
 wrapperNum <- newIORef emptyModuleEnv
 canUseUnicode <- do let enc = localeEncoding
                         str = "‘’"
                     (withCString enc str $ \cstr ->
                          do str' <- peekCString enc cstr
                             return (str == str'))
                         `catchIOError` \_ -> return False
 return dflags{
        canGenerateDynamicToo = refCanGenerateDynamicToo,
        nextTempSuffix = refNextTempSuffix,
        filesToClean   = refFilesToClean,
        dirsToClean    = refDirsToClean,
        filesToNotIntermediateClean = refFilesToNotIntermediateClean,
        generatedDumps = refGeneratedDumps,
        nextWrapperNum = wrapperNum,
        useUnicode    = canUseUnicode,
        rtldInfo      = refRtldInfo,
        rtccInfo      = refRtccInfo
        }

defaultWays :: Settings -> [Way]
defaultWays settings = if pc_DYNAMIC_BY_DEFAULT (sPlatformConstants settings)
                       then [WayDyn]
                       else []

interpWays :: [Way]
interpWays
  | dynamicGhc = [WayDyn]
  | rtsIsProfiled = [WayProf]
  | otherwise = []

interpreterProfiled :: DynFlags -> Bool
interpreterProfiled dflags
  | gopt Opt_ExternalInterpreter dflags = gopt Opt_SccProfilingOn dflags
  | otherwise = rtsIsProfiled

interpreterDynamic :: DynFlags -> Bool
interpreterDynamic dflags
  | gopt Opt_ExternalInterpreter dflags = WayDyn `elem` ways dflags
  | otherwise = dynamicGhc

--------------------------------------------------------------------------

type FatalMessager = String -> IO ()

type LogAction = DynFlags
              -> WarnReason
              -> Severity
              -> SrcSpan
              -> PprStyle
              -> MsgDoc
              -> IO ()

defaultFatalMessager :: FatalMessager
defaultFatalMessager = hPutStrLn stderr

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

newtype FlushOut = FlushOut (IO ())

defaultFlushOut :: FlushOut
defaultFlushOut = FlushOut $ hFlush stdout

newtype FlushErr = FlushErr (IO ())

defaultFlushErr :: FlushErr
defaultFlushErr = FlushErr $ hFlush stderr

{-
Note [Verbosity levels]
~~~~~~~~~~~~~~~~~~~~~~~
    0   |   print errors & warnings only
    1   |   minimal verbosity: print "compiling M ... done." for each module.
    2   |   equivalent to -dshow-passes
    3   |   equivalent to existing "ghc -v"
    4   |   "ghc -v -ddump-most"
    5   |   "ghc -v -ddump-all"
-}

data OnOff a = On a
             | Off a
  deriving (Eq, Show)

instance Outputable a => Outputable (OnOff a) where
  ppr (On x)  = text "On" <+> ppr x
  ppr (Off x) = text "Off" <+> ppr x

-- OnOffs accumulate in reverse order, so we use foldr in order to
-- process them in the right order
flattenExtensionFlags :: Maybe Language -> [OnOff LangExt.Extension] -> IntSet
flattenExtensionFlags ml = foldr f defaultExtensionFlags
    where f (On f)  flags = IntSet.insert (fromEnum f) flags
          f (Off f) flags = IntSet.delete (fromEnum f) flags
          defaultExtensionFlags = IntSet.fromList (map fromEnum (languageExtensions ml))

languageExtensions :: Maybe Language -> [LangExt.Extension]

languageExtensions Nothing
    -- Nothing => the default case
    = LangExt.NondecreasingIndentation -- This has been on by default for some time
    : delete LangExt.DatatypeContexts  -- The Haskell' committee decided to
                                       -- remove datatype contexts from the
                                       -- language:
   -- http://www.haskell.org/pipermail/haskell-prime/2011-January/003335.html
      (languageExtensions (Just Haskell2010))

   -- NB: MonoPatBinds is no longer the default

languageExtensions (Just Haskell98)
    = [LangExt.ImplicitPrelude,
       LangExt.MonomorphismRestriction,
       LangExt.NPlusKPatterns,
       LangExt.DatatypeContexts,
       LangExt.TraditionalRecordSyntax,
       LangExt.NondecreasingIndentation
           -- strictly speaking non-standard, but we always had this
           -- on implicitly before the option was added in 7.1, and
           -- turning it off breaks code, so we're keeping it on for
           -- backwards compatibility.  Cabal uses -XHaskell98 by
           -- default unless you specify another language.
      ]

languageExtensions (Just Haskell2010)
    = [LangExt.ImplicitPrelude,
       LangExt.MonomorphismRestriction,
       LangExt.DatatypeContexts,
       LangExt.TraditionalRecordSyntax,
       LangExt.EmptyDataDecls,
       LangExt.ForeignFunctionInterface,
       LangExt.PatternGuards,
       LangExt.DoAndIfThenElse,
       LangExt.RelaxedPolyRec]

-- | Test whether a 'DumpFlag' is set
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

-- | Set a 'DumpFlag'
dopt_set :: DynFlags -> DumpFlag -> DynFlags
dopt_set dfs f = dfs{ dumpFlags = IntSet.insert (fromEnum f) (dumpFlags dfs) }

-- | Unset a 'DumpFlag'
dopt_unset :: DynFlags -> DumpFlag -> DynFlags
dopt_unset dfs f = dfs{ dumpFlags = IntSet.delete (fromEnum f) (dumpFlags dfs) }

-- | Test whether a 'GeneralFlag' is set
gopt :: GeneralFlag -> DynFlags -> Bool
gopt f dflags  = fromEnum f `IntSet.member` generalFlags dflags

-- | Set a 'GeneralFlag'
gopt_set :: DynFlags -> GeneralFlag -> DynFlags
gopt_set dfs f = dfs{ generalFlags = IntSet.insert (fromEnum f) (generalFlags dfs) }

-- | Unset a 'GeneralFlag'
gopt_unset :: DynFlags -> GeneralFlag -> DynFlags
gopt_unset dfs f = dfs{ generalFlags = IntSet.delete (fromEnum f) (generalFlags dfs) }

-- | Test whether a 'WarningFlag' is set
wopt :: WarningFlag -> DynFlags -> Bool
wopt f dflags  = fromEnum f `IntSet.member` warningFlags dflags

-- | Set a 'WarningFlag'
wopt_set :: DynFlags -> WarningFlag -> DynFlags
wopt_set dfs f = dfs{ warningFlags = IntSet.insert (fromEnum f) (warningFlags dfs) }

-- | Unset a 'WarningFlag'
wopt_unset :: DynFlags -> WarningFlag -> DynFlags
wopt_unset dfs f = dfs{ warningFlags = IntSet.delete (fromEnum f) (warningFlags dfs) }

-- | Test whether a 'LangExt.Extension' is set
xopt :: LangExt.Extension -> DynFlags -> Bool
xopt f dflags = fromEnum f `IntSet.member` extensionFlags dflags

-- | Set a 'LangExt.Extension'
xopt_set :: DynFlags -> LangExt.Extension -> DynFlags
xopt_set dfs f
    = let onoffs = On f : extensions dfs
      in dfs { extensions = onoffs,
               extensionFlags = flattenExtensionFlags (language dfs) onoffs }

-- | Unset a 'LangExt.Extension'
xopt_unset :: DynFlags -> LangExt.Extension -> DynFlags
xopt_unset dfs f
    = let onoffs = Off f : extensions dfs
      in dfs { extensions = onoffs,
               extensionFlags = flattenExtensionFlags (language dfs) onoffs }

lang_set :: DynFlags -> Maybe Language -> DynFlags
lang_set dflags lang =
   dflags {
            language = lang,
            extensionFlags = flattenExtensionFlags lang (extensions dflags)
          }

-- | An internal helper to check whether to use unicode syntax for output.
--
-- Note: You should very likely be using 'Outputable.unicodeSyntax' instead
-- of this function.
useUnicodeSyntax :: DynFlags -> Bool
useUnicodeSyntax = gopt Opt_PrintUnicodeSyntax

-- | Some modules have dependencies on others through the DynFlags rather than textual imports
dynFlagDependencies :: DynFlags -> [ModuleName]
dynFlagDependencies = pluginModNames

-- | Is the -fpackage-trust mode on
packageTrustOn :: DynFlags -> Bool
packageTrustOn = gopt Opt_PackageTrust

-- | Is Safe Haskell on in some way (including inference mode)
safeHaskellOn :: DynFlags -> Bool
safeHaskellOn dflags = safeHaskell dflags /= Sf_None || safeInferOn dflags

-- | Is the Safe Haskell safe language in use
safeLanguageOn :: DynFlags -> Bool
safeLanguageOn dflags = safeHaskell dflags == Sf_Safe

-- | Is the Safe Haskell safe inference mode active
safeInferOn :: DynFlags -> Bool
safeInferOn = safeInfer

-- | Test if Safe Imports are on in some form
safeImportsOn :: DynFlags -> Bool
safeImportsOn dflags = safeHaskell dflags == Sf_Unsafe ||
                       safeHaskell dflags == Sf_Trustworthy ||
                       safeHaskell dflags == Sf_Safe

-- | Set a 'Safe Haskell' flag

-- | Are all direct imports required to be safe for this Safe Haskell mode?
-- Direct imports are when the code explicitly imports a module
safeDirectImpsReq :: DynFlags -> Bool
safeDirectImpsReq d = safeLanguageOn d

-- | Are all implicit imports required to be safe for this Safe Haskell mode?
-- Implicit imports are things in the prelude. e.g System.IO when print is used.
safeImplicitImpsReq :: DynFlags -> Bool
safeImplicitImpsReq d = safeLanguageOn d

-- | Combine two Safe Haskell modes correctly. Used for dealing with multiple flags.
-- This makes Safe Haskell very much a monoid but for now I prefer this as I don't
-- want to export this functionality from the module but do want to export the
-- type constructors.

-- | A list of unsafe flags under Safe Haskell. Tuple elements are:
--     * name of the flag
--     * function to get srcspan that enabled the flag
--     * function to test if the flag is on
--     * function to turn the flag off
unsafeFlags, unsafeFlagsForInfer
  :: [(String, DynFlags -> SrcSpan, DynFlags -> Bool, DynFlags -> DynFlags)]
unsafeFlags = [ ("-XGeneralizedNewtypeDeriving", newDerivOnLoc,
                    xopt LangExt.GeneralizedNewtypeDeriving,
                    flip xopt_unset LangExt.GeneralizedNewtypeDeriving)
              , ("-XTemplateHaskell", thOnLoc,
                    xopt LangExt.TemplateHaskell,
                    flip xopt_unset LangExt.TemplateHaskell)
              ]
unsafeFlagsForInfer = unsafeFlags


-- | Retrieve the options corresponding to a particular @opt_*@ field in the correct order
getOpts :: DynFlags             -- ^ 'DynFlags' to retrieve the options from
        -> (DynFlags -> [a])    -- ^ Relevant record accessor: one of the @opt_*@ accessors
        -> [a]                  -- ^ Correctly ordered extracted options
getOpts dflags opts = reverse (opts dflags)
        -- We add to the options from the front, so we need to reverse the list

-- | Gets the verbosity flag for the current verbosity level. This is fed to
-- other tools, so GHC-specific verbosity flags like @-ddump-most@ are not included
getVerbFlags :: DynFlags -> [String]
getVerbFlags dflags
  | verbosity dflags >= 4 = ["-v"]
  | otherwise             = []

setObjectDir, setHiDir, setStubDir, setDumpDir, setOutputDir,
         setDynObjectSuf, setDynHiSuf,
         setDylibInstallName,
         setObjectSuf, setHiSuf, setHcSuf, parseDynLibLoaderMode,
         setPgmP, addOptl, addOptc, addOptP,
         addCmdlineFramework, addHaddockOpts, addGhciScript,
         setInteractivePrint
   :: String -> DynFlags -> DynFlags
setOutputFile, setDynOutputFile, setOutputHi, setDumpPrefixForce
   :: Maybe String -> DynFlags -> DynFlags

setObjectDir  f d = d { objectDir  = Just f}
setHiDir      f d = d { hiDir      = Just f}
setStubDir    f d = d { stubDir    = Just f, includePaths = f : includePaths d }
  -- -stubdir D adds an implicit -I D, so that gcc can find the _stub.h file
  -- \#included from the .hc file when compiling via C (i.e. unregisterised
  -- builds).
setDumpDir    f d = d { dumpDir    = Just f}
setOutputDir  f = setObjectDir f . setHiDir f . setStubDir f . setDumpDir f
setDylibInstallName  f d = d { dylibInstallName = Just f}

setObjectSuf    f d = d { objectSuf    = f}
setDynObjectSuf f d = d { dynObjectSuf = f}
setHiSuf        f d = d { hiSuf        = f}
setDynHiSuf     f d = d { dynHiSuf     = f}
setHcSuf        f d = d { hcSuf        = f}

setOutputFile f d = d { outputFile = f}
setDynOutputFile f d = d { dynOutputFile = f}
setOutputHi   f d = d { outputHi   = f}


addPluginModuleName :: String -> DynFlags -> DynFlags
addPluginModuleName name d = d { pluginModNames = (mkModuleName name) : (pluginModNames d) }

addPluginModuleNameOption :: String -> DynFlags -> DynFlags
addPluginModuleNameOption optflag d = d { pluginModNameOpts = (mkModuleName m, option) : (pluginModNameOpts d) }
  where (m, rest) = break (== ':') optflag
        option = case rest of
          [] -> "" -- should probably signal an error
          (_:plug_opt) -> plug_opt -- ignore the ':' from break

addFrontendPluginOption :: String -> DynFlags -> DynFlags
addFrontendPluginOption s d = d { frontendPluginOpts = s : frontendPluginOpts d }

parseDynLibLoaderMode f d =
 case splitAt 8 f of
   ("deploy", "")       -> d { dynLibLoader = Deployable }
   ("sysdep", "")       -> d { dynLibLoader = SystemDependent }
   _                    -> throwGhcException (CmdLineError ("Unknown dynlib loader: " ++ f))

setDumpPrefixForce f d = d { dumpPrefixForce = f}

-- XXX HACK: Prelude> words "'does not' work" ===> ["'does","not'","work"]
-- Config.hs should really use Option.
setPgmP   f = let (pgm:args) = words f in alterSettings (\s -> s { sPgm_P   = (pgm, map Option args)})
addOptl   f = alterSettings (\s -> s { sOpt_l   = f : sOpt_l s})
addOptc   f = alterSettings (\s -> s { sOpt_c   = f : sOpt_c s})
addOptP   f = alterSettings (\s -> s { sOpt_P   = f : sOpt_P s})


setDepMakefile :: FilePath -> DynFlags -> DynFlags
setDepMakefile f d = d { depMakefile = f }

setDepIncludePkgDeps :: Bool -> DynFlags -> DynFlags
setDepIncludePkgDeps b d = d { depIncludePkgDeps = b }

addDepExcludeMod :: String -> DynFlags -> DynFlags
addDepExcludeMod m d
    = d { depExcludeMods = mkModuleName m : depExcludeMods d }

addDepSuffix :: FilePath -> DynFlags -> DynFlags
addDepSuffix s d = d { depSuffixes = s : depSuffixes d }

addCmdlineFramework f d = d { cmdlineFrameworks = f : cmdlineFrameworks d}

addHaddockOpts f d = d { haddockOptions = Just f}

addGhciScript f d = d { ghciScripts = f : ghciScripts d}

setInteractivePrint f d = d { interactivePrint = Just f}

-- -----------------------------------------------------------------------------
-- Command-line options

-- | When invoking external tools as part of the compilation pipeline, we
-- pass these a sequence of options on the command-line. Rather than
-- just using a list of Strings, we use a type that allows us to distinguish
-- between filepaths and 'other stuff'. The reason for this is that
-- this type gives us a handle on transforming filenames, and filenames only,
-- to whatever format they're expected to be on a particular platform.
data Option
 = FileOption -- an entry that _contains_ filename(s) / filepaths.
              String  -- a non-filepath prefix that shouldn't be
                      -- transformed (e.g., "/out=")
              String  -- the filepath/filename portion
 | Option     String
 deriving ( Eq )

showOpt :: Option -> String
showOpt (FileOption pre f) = pre ++ f
showOpt (Option s)  = s

-----------------------------------------------------------------------------
-- Setting the optimisation level

updOptLevel :: Int -> DynFlags -> DynFlags
-- ^ Sets the 'DynFlags' to be appropriate to the optimisation level
updOptLevel n dfs
  = dfs2{ optLevel = final_n }
  where
   final_n = max 0 (min 2 n)    -- Clamp to 0 <= n <= 2
   dfs1 = foldr (flip gopt_unset) dfs  remove_gopts
   dfs2 = foldr (flip gopt_set)   dfs1 extra_gopts

   extra_gopts  = [ f | (ns,f) <- optLevelFlags, final_n `elem` ns ]
   remove_gopts = [ f | (ns,f) <- optLevelFlags, final_n `notElem` ns ]

{- **********************************************************************
%*                                                                      *
                DynFlags parser
%*                                                                      *
%********************************************************************* -}

-- -----------------------------------------------------------------------------
-- Parsing the dynamic flags.



-- | Parses the dynamically set flags for GHC. This is the most general form of
-- the dynamic flag parser that the other methods simply wrap. It allows
-- saying which flags are valid flags and indicating if we are parsing
-- arguments from the command line or from a file pragma.
updateWays :: DynFlags -> DynFlags
updateWays dflags
    = let theWays = sort $ nub $ ways dflags
      in dflags {
             ways        = theWays,
             buildTag    = mkBuildTag (filter (not . wayRTSOnly) theWays),
             rtsBuildTag = mkBuildTag                            theWays
         }

-- | Check (and potentially disable) any extensions that aren't allowed
-- in safe mode.
--
-- The bool is to indicate if we are parsing command line flags (false means
-- file pragma). This allows us to generate better warnings.
safeFlagCheck :: Bool -> DynFlags -> (DynFlags, [Located String])
safeFlagCheck _ dflags | safeLanguageOn dflags = (dflagsUnset, warns)
  where
    -- Handle illegal flags under safe language.
    (dflagsUnset, warns) = foldl check_method (dflags, []) unsafeFlags

    check_method (df, warns) (str,loc,test,fix)
        | test df   = (fix df, warns ++ safeFailure (loc df) str)
        | otherwise = (df, warns)

    safeFailure loc str
       = [L loc $ str ++ " is not allowed in Safe Haskell; ignoring "
           ++ str]

safeFlagCheck cmdl dflags =
  case (safeInferOn dflags) of
    True | safeFlags -> (dflags', warn)
    True             -> (dflags' { safeInferred = False }, warn)
    False            -> (dflags', warn)

  where
    -- dynflags and warn for when -fpackage-trust by itself with no safe
    -- haskell flag
    (dflags', warn)
      | safeHaskell dflags == Sf_None && not cmdl && packageTrustOn dflags
      = (gopt_unset dflags Opt_PackageTrust, pkgWarnMsg)
      | otherwise = (dflags, [])

    pkgWarnMsg = [L (pkgTrustOnLoc dflags') $
                    "-fpackage-trust ignored;" ++
                    " must be specified with a Safe Haskell flag"]

    -- Have we inferred Unsafe? See Note [HscMain . Safe Haskell Inference]
    safeFlags = all (\(_,_,t,_) -> not $ t dflags) unsafeFlagsForInfer


{- **********************************************************************
%*                                                                      *
                DynFlags specifications
%*                                                                      *
%********************************************************************* -}

----------------Helpers to make flags and keep deprecation information----------

data Deprecation = Deprecated | NotDeprecated




-- | This is where we handle unrecognised warning flags. We only issue a warning
-- if -Wunrecognised-warning-flags is set. See Trac #11429 for context.

type TurnOnFlag = Bool   -- True  <=> we are turning the flag on
                         -- False <=> we are turning the flag off
turnOn  :: TurnOnFlag; turnOn  = True
turnOff :: TurnOnFlag; turnOff = False


deprecatedForExtension :: String -> TurnOnFlag -> String
deprecatedForExtension lang turn_on
    = "use -X" ++ flag ++
      " or pragma {-# LANGUAGE " ++ flag ++ " #-} instead"
    where
      flag | turn_on   = lang
           | otherwise = "No" ++ lang

useInstead :: String -> TurnOnFlag -> String
useInstead flag turn_on
  = "Use -f" ++ no ++ flag ++ " instead"
  where
    no = if turn_on then "" else "no-"




defaultFlags :: Settings -> [GeneralFlag]
defaultFlags settings
-- See Note [Updating flag description in the User's Guide]
  = [ Opt_AutoLinkPackages,
      Opt_EmbedManifest,
      Opt_FlatCache,
      Opt_GenManifest,
      Opt_GhciHistory,
      Opt_GhciSandbox,
      Opt_HelpfulErrors,
      Opt_KeepHiFiles,
      Opt_KeepOFiles,
      Opt_OmitYields,
      Opt_PrintBindContents,
      Opt_ProfCountEntries,
      Opt_RPath,
      Opt_SharedImplib,
      Opt_SimplPreInlining,
      Opt_VersionMacros
    ]

    ++ [f | (ns,f) <- optLevelFlags, 0 `elem` ns]
             -- The default -O0 options

    ++ default_PIC platform

    ++ concatMap (wayGeneralFlags platform) (defaultWays settings)

    where platform = sTargetPlatform settings

default_PIC :: Platform -> [GeneralFlag]
default_PIC platform =
  case (platformOS platform, platformArch platform) of
    (OSDarwin, ArchX86_64) -> [Opt_PIC]
    (OSOpenBSD, ArchX86_64) -> [Opt_PIC] -- Due to PIE support in
                                         -- OpenBSD since 5.3 release
                                         -- (1 May 2013) we need to
                                         -- always generate PIC. See
                                         -- #10597 for more
                                         -- information.
    _                      -> []

-- General flags that are switched on/off when other general flags are switched
-- on
impliedGFlags :: [(GeneralFlag, TurnOnFlag, GeneralFlag)]
impliedGFlags = [(Opt_DeferTypeErrors, turnOn, Opt_DeferTypedHoles)
                ,(Opt_Strictness, turnOn, Opt_WorkerWrapper)
                ]

-- General flags that are switched on/off when other general flags are switched
-- off
impliedOffGFlags :: [(GeneralFlag, TurnOnFlag, GeneralFlag)]
impliedOffGFlags = [(Opt_Strictness, turnOff, Opt_WorkerWrapper)]

impliedXFlags :: [(LangExt.Extension, TurnOnFlag, LangExt.Extension)]
impliedXFlags
-- See Note [Updating flag description in the User's Guide]
  = [ (LangExt.RankNTypes,                turnOn, LangExt.ExplicitForAll)
    , (LangExt.ScopedTypeVariables,       turnOn, LangExt.ExplicitForAll)
    , (LangExt.LiberalTypeSynonyms,       turnOn, LangExt.ExplicitForAll)
    , (LangExt.ExistentialQuantification, turnOn, LangExt.ExplicitForAll)
    , (LangExt.FlexibleInstances,         turnOn, LangExt.TypeSynonymInstances)
    , (LangExt.FunctionalDependencies,    turnOn, LangExt.MultiParamTypeClasses)
    , (LangExt.MultiParamTypeClasses,     turnOn, LangExt.ConstrainedClassMethods)  -- c.f. Trac #7854
    , (LangExt.TypeFamilyDependencies,    turnOn, LangExt.TypeFamilies)

    , (LangExt.RebindableSyntax, turnOff, LangExt.ImplicitPrelude)      -- NB: turn off!

    , (LangExt.GADTs,            turnOn, LangExt.GADTSyntax)
    , (LangExt.GADTs,            turnOn, LangExt.MonoLocalBinds)
    , (LangExt.TypeFamilies,     turnOn, LangExt.MonoLocalBinds)

    , (LangExt.TypeFamilies,     turnOn, LangExt.KindSignatures)  -- Type families use kind signatures
    , (LangExt.PolyKinds,        turnOn, LangExt.KindSignatures)  -- Ditto polymorphic kinds
    , (LangExt.TypeInType,       turnOn, LangExt.DataKinds)
    , (LangExt.TypeInType,       turnOn, LangExt.PolyKinds)
    , (LangExt.TypeInType,       turnOn, LangExt.KindSignatures)

    -- AutoDeriveTypeable is not very useful without DeriveDataTypeable
    , (LangExt.AutoDeriveTypeable, turnOn, LangExt.DeriveDataTypeable)

    -- We turn this on so that we can export associated type
    -- type synonyms in subordinates (e.g. MyClass(type AssocType))
    , (LangExt.TypeFamilies,     turnOn, LangExt.ExplicitNamespaces)
    , (LangExt.TypeOperators, turnOn, LangExt.ExplicitNamespaces)

    , (LangExt.ImpredicativeTypes,  turnOn, LangExt.RankNTypes)

        -- Record wild-cards implies field disambiguation
        -- Otherwise if you write (C {..}) you may well get
        -- stuff like " 'a' not in scope ", which is a bit silly
        -- if the compiler has just filled in field 'a' of constructor 'C'
    , (LangExt.RecordWildCards,     turnOn, LangExt.DisambiguateRecordFields)

    , (LangExt.ParallelArrays, turnOn, LangExt.ParallelListComp)

    , (LangExt.JavaScriptFFI, turnOn, LangExt.InterruptibleFFI)

    , (LangExt.DeriveTraversable, turnOn, LangExt.DeriveFunctor)
    , (LangExt.DeriveTraversable, turnOn, LangExt.DeriveFoldable)

    -- Duplicate record fields require field disambiguation
    , (LangExt.DuplicateRecordFields, turnOn, LangExt.DisambiguateRecordFields)

    , (LangExt.TemplateHaskell, turnOn, LangExt.TemplateHaskellQuotes)
    , (LangExt.Strict, turnOn, LangExt.StrictData)
  ]

-- Note [Documenting optimisation flags]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- If you change the list of flags enabled for particular optimisation levels
-- please remember to update the User's Guide. The relevant files are:
--
--  * utils/mkUserGuidePart/Options/
--  * docs/users_guide/using.rst
--
-- The first contains the Flag Refrence section, which breifly lists all
-- available flags. The second contains a detailed description of the
-- flags. Both places should contain information whether a flag is implied by
-- -O0, -O or -O2.

optLevelFlags :: [([Int], GeneralFlag)]
optLevelFlags -- see Note [Documenting optimisation flags]
  = [ ([0,1,2], Opt_DoLambdaEtaExpansion)
    , ([0,1,2], Opt_DoEtaReduction)       -- See Note [Eta-reduction in -O0]
    , ([0,1,2], Opt_DmdTxDictSel)
    , ([0,1,2], Opt_LlvmTBAA)
    , ([0,1,2], Opt_VectorisationAvoidance)
                -- This one is important for a tiresome reason:
                -- we want to make sure that the bindings for data
                -- constructors are eta-expanded.  This is probably
                -- a good thing anyway, but it seems fragile.

    , ([0],     Opt_IgnoreInterfacePragmas)
    , ([0],     Opt_OmitInterfacePragmas)

    , ([1,2],   Opt_CallArity)
    , ([1,2],   Opt_CaseMerge)
    , ([1,2],   Opt_CmmElimCommonBlocks)
    , ([1,2],   Opt_CmmSink)
    , ([1,2],   Opt_CSE)
    , ([1,2],   Opt_EnableRewriteRules)  -- Off for -O0; see Note [Scoping for Builtin rules]
                                         --              in PrelRules
    , ([1,2],   Opt_FloatIn)
    , ([1,2],   Opt_FullLaziness)
    , ([1,2],   Opt_IgnoreAsserts)
    , ([1,2],   Opt_Loopification)
    , ([1,2],   Opt_Specialise)
    , ([1,2],   Opt_CrossModuleSpecialise)
    , ([1,2],   Opt_Strictness)
    , ([1,2],   Opt_UnboxSmallStrictFields)
    , ([1,2],   Opt_CprAnal)
    , ([1,2],   Opt_WorkerWrapper)

    , ([2],     Opt_LiberateCase)
    , ([2],     Opt_SpecConstr)
--  , ([2],     Opt_RegsGraph)
--   RegsGraph suffers performance regression. See #7679
--  , ([2],     Opt_StaticArgumentTransformation)
--   Static Argument Transformation needs investigation. See #9374
    ]

{- Note [Eta-reduction in -O0]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Trac #11562 showed an example which tripped an ASSERT in CoreToStg; a
function was marked as MayHaveCafRefs when in fact it obviously
didn't.  Reason was:
 * Eta reduction wasn't happening in the simplifier, but it was
   happening in CorePrep, on
        $fBla = MkDict (/\a. K a)
 * Result: rhsIsStatic told TidyPgm that $fBla might have CAF refs
   but the eta-reduced version (MkDict K) obviously doesn't
Simple solution: just let the simplifier do eta-reduction even in -O0.
After all, CorePrep does it unconditionally!  Not a big deal, but
removes an assertion failure. -}


-- -----------------------------------------------------------------------------
-- Standard sets of warning options

-- Note [Documenting warning flags]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- If you change the list of warning enabled by default
-- please remember to update the User's Guide. The relevant file is:
--
--  * utils/mkUserGuidePart/
--  * docs/users_guide/using-warnings.rst


-- | Things you get with -Wcompat.
--
-- This is intended to group together warnings that will be enabled by default
-- at some point in the future, so that library authors eager to make their
-- code future compatible to fix issues before they even generate warnings.

glasgowExtsFlags :: [LangExt.Extension]
glasgowExtsFlags = [
             LangExt.ConstrainedClassMethods
           , LangExt.DeriveDataTypeable
           , LangExt.DeriveFoldable
           , LangExt.DeriveFunctor
           , LangExt.DeriveGeneric
           , LangExt.DeriveTraversable
           , LangExt.EmptyDataDecls
           , LangExt.ExistentialQuantification
           , LangExt.ExplicitNamespaces
           , LangExt.FlexibleContexts
           , LangExt.FlexibleInstances
           , LangExt.ForeignFunctionInterface
           , LangExt.FunctionalDependencies
           , LangExt.GeneralizedNewtypeDeriving
           , LangExt.ImplicitParams
           , LangExt.KindSignatures
           , LangExt.LiberalTypeSynonyms
           , LangExt.MagicHash
           , LangExt.MultiParamTypeClasses
           , LangExt.ParallelListComp
           , LangExt.PatternGuards
           , LangExt.PostfixOperators
           , LangExt.RankNTypes
           , LangExt.RecursiveDo
           , LangExt.ScopedTypeVariables
           , LangExt.StandaloneDeriving
           , LangExt.TypeOperators
           , LangExt.TypeSynonymInstances
           , LangExt.UnboxedTuples
           , LangExt.UnicodeSyntax
           , LangExt.UnliftedFFITypes ]

foreign import ccall unsafe "rts_isProfiled" rtsIsProfiledIO :: IO CInt

-- | Was the runtime system built with profiling enabled?
rtsIsProfiled :: Bool
rtsIsProfiled = unsafeDupablePerformIO rtsIsProfiledIO /= 0

#ifdef GHCI
-- Consult the RTS to find whether GHC itself has been built with
-- dynamic linking.  This can't be statically known at compile-time,
-- because we build both the static and dynamic versions together with
-- -dynamic-too.
foreign import ccall unsafe "rts_isDynamic" rtsIsDynamicIO :: IO CInt

dynamicGhc :: Bool
dynamicGhc = unsafeDupablePerformIO rtsIsDynamicIO /= 0
#else
dynamicGhc :: Bool
dynamicGhc = False
#endif


{- **********************************************************************
%*                                                                      *
                DynFlags constructors
%*                                                                      *
%********************************************************************* -}

--------------------------
alterSettings :: (Settings -> Settings) -> DynFlags -> DynFlags
alterSettings f dflags = dflags { settings = f (settings dflags) }

--------------------------

data PkgConfRef
  = GlobalPkgConf
  | UserPkgConf
  | PkgConfFile FilePath

setUnitId :: String -> DynFlags -> DynFlags
setUnitId p s =  s{ thisPackage = stringToUnitId p }

checkOptLevel :: Int -> DynFlags -> Either String DynFlags
checkOptLevel n dflags
   | hscTarget dflags == HscInterpreted && n > 0
     = Left "-O conflicts with --interactive; -O ignored."
   | otherwise
     = Right dflags


-----------------------------------------------------------------------------
-- Via-C compilation stuff

-- There are some options that we need to pass to gcc when compiling
-- Haskell code via C, but are only supported by recent versions of
-- gcc.  The configure script decides which of these options we need,
-- and puts them in the "settings" file in $topdir. The advantage of
-- having these in a separate file is that the file can be created at
-- install-time depending on the available gcc version, and even
-- re-generated later if gcc is upgraded.
--
-- The options below are not dependent on the version of gcc, only the
-- platform.

picCCOpts :: DynFlags -> [String]
picCCOpts dflags
    = case platformOS (targetPlatform dflags) of
      OSDarwin
          -- Apple prefers to do things the other way round.
          -- PIC is on by default.
          -- -mdynamic-no-pic:
          --     Turn off PIC code generation.
          -- -fno-common:
          --     Don't generate "common" symbols - these are unwanted
          --     in dynamic libraries.

       | gopt Opt_PIC dflags -> ["-fno-common", "-U__PIC__", "-D__PIC__"]
       | otherwise           -> ["-mdynamic-no-pic"]
      OSMinGW32 -- no -fPIC for Windows
       | gopt Opt_PIC dflags -> ["-U__PIC__", "-D__PIC__"]
       | otherwise           -> []
      _
      -- we need -fPIC for C files when we are compiling with -dynamic,
      -- otherwise things like stub.c files don't get compiled
      -- correctly.  They need to reference data in the Haskell
      -- objects, but can't without -fPIC.  See
      -- http://ghc.haskell.org/trac/ghc/wiki/Commentary/PositionIndependentCode
       | gopt Opt_PIC dflags || WayDyn `elem` ways dflags ->
          ["-fPIC", "-U__PIC__", "-D__PIC__"]
       | otherwise                             -> []

picPOpts :: DynFlags -> [String]
picPOpts dflags
 | gopt Opt_PIC dflags = ["-U__PIC__", "-D__PIC__"]
 | otherwise           = []

-- -----------------------------------------------------------------------------
-- Compiler Info

compilerInfo :: DynFlags -> [(String, String)]
compilerInfo dflags
    = -- We always make "Project name" be first to keep parsing in
      -- other languages simple, i.e. when looking for other fields,
      -- you don't have to worry whether there is a leading '[' or not
      ("Project name",                 cProjectName)
      -- Next come the settings, so anything else can be overridden
      -- in the settings file (as "lookup" uses the first match for the
      -- key)
    : rawSettings dflags
   ++ [("Project version",             projectVersion dflags),
       ("Project Git commit id",       cProjectGitCommitId),
       ("Booter version",              cBooterVersion),
       ("Stage",                       cStage),
       ("Build platform",              cBuildPlatformString),
       ("Host platform",               cHostPlatformString),
       ("Target platform",             cTargetPlatformString),
       ("Have interpreter",            cGhcWithInterpreter),
       ("Object splitting supported",  cSupportsSplitObjs),
       ("Have native code generator",  cGhcWithNativeCodeGen),
       ("Support SMP",                 cGhcWithSMP),
       ("Tables next to code",         cGhcEnableTablesNextToCode),
       ("RTS ways",                    cGhcRTSWays),
       ("RTS expects libdw",           showBool cGhcRtsWithLibdw),
       -- Whether or not we support @-dynamic-too@
       ("Support dynamic-too",         showBool $ not isWindows),
       -- Whether or not we support the @-j@ flag with @--make@.
       ("Support parallel --make",     "YES"),
       -- Whether or not we support "Foo from foo-0.1-XXX:Foo" syntax in
       -- installed package info.
       ("Support reexported-modules",  "YES"),
       -- Whether or not we support extended @-package foo (Foo)@ syntax.
       ("Support thinning and renaming package flags", "YES"),
       -- If true, we require that the 'id' field in installed package info
       -- match what is passed to the @-this-unit-id@ flag for modules
       -- built in it
       ("Requires unified installed package IDs", "YES"),
       -- Whether or not we support the @-this-package-key@ flag.  Prefer
       -- "Uses unit IDs" over it.
       ("Uses package keys",           "YES"),
       -- Whether or not we support the @-this-unit-id@ flag
       ("Uses unit IDs",               "YES"),
       -- Whether or not GHC compiles libraries as dynamic by default
       ("Dynamic by default",          showBool $ dYNAMIC_BY_DEFAULT dflags),
       -- Whether or not GHC was compiled using -dynamic
       ("GHC Dynamic",                 showBool dynamicGhc),
       -- Whether or not GHC was compiled using -prof
       ("GHC Profiled",                showBool rtsIsProfiled),
       ("Leading underscore",          cLeadingUnderscore),
       ("Debug on",                    show debugIsOn),
       ("LibDir",                      topDir dflags),
       -- The path of the global package database used by GHC
       ("Global Package DB",           systemPackageConfig dflags)
      ]
  where
    showBool True  = "YES"
    showBool False = "NO"
    isWindows = platformOS (targetPlatform dflags) == OSMinGW32

#include "GHCConstantsHaskellWrappers.hs"

bLOCK_SIZE_W :: DynFlags -> Int
bLOCK_SIZE_W dflags = bLOCK_SIZE dflags `quot` wORD_SIZE dflags

wORD_SIZE_IN_BITS :: DynFlags -> Int
wORD_SIZE_IN_BITS dflags = wORD_SIZE dflags * 8

tAG_MASK :: DynFlags -> Int
tAG_MASK dflags = (1 `shiftL` tAG_BITS dflags) - 1

mAX_PTR_TAG :: DynFlags -> Int
mAX_PTR_TAG = tAG_MASK

-- Might be worth caching these in targetPlatform?
tARGET_MIN_INT, tARGET_MAX_INT, tARGET_MAX_WORD :: DynFlags -> Integer
tARGET_MIN_INT dflags
    = case platformWordSize (targetPlatform dflags) of
      4 -> toInteger (minBound :: Int32)
      8 -> toInteger (minBound :: Int64)
      w -> panic ("tARGET_MIN_INT: Unknown platformWordSize: " ++ show w)
tARGET_MAX_INT dflags
    = case platformWordSize (targetPlatform dflags) of
      4 -> toInteger (maxBound :: Int32)
      8 -> toInteger (maxBound :: Int64)
      w -> panic ("tARGET_MAX_INT: Unknown platformWordSize: " ++ show w)
tARGET_MAX_WORD dflags
    = case platformWordSize (targetPlatform dflags) of
      4 -> toInteger (maxBound :: Word32)
      8 -> toInteger (maxBound :: Word64)
      w -> panic ("tARGET_MAX_WORD: Unknown platformWordSize: " ++ show w)


{- -----------------------------------------------------------------------------
Note [DynFlags consistency]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are a number of number of DynFlags configurations which either
do not make sense or lead to unimplemented or buggy codepaths in the
compiler. makeDynFlagsConsistent is responsible for verifying the validity
of a set of DynFlags, fixing any issues, and reporting them back to the
caller.

GHCi and -O
---------------

When using optimization, the compiler can introduce several things
(such as unboxed tuples) into the intermediate code, which GHCi later
chokes on since the bytecode interpreter can't handle this (and while
this is arguably a bug these aren't handled, there are no plans to fix
it.)

While the driver pipeline always checks for this particular erroneous
combination when parsing flags, we also need to check when we update
the flags; this is because API clients may parse flags but update the
DynFlags afterwords, before finally running code inside a session (see
T10052 and #10052).
-}

-- | Resolve any internal inconsistencies in a set of 'DynFlags'.
-- Returns the consistent 'DynFlags' as well as a list of warnings
-- to report to the user.
makeDynFlagsConsistent :: DynFlags -> (DynFlags, [Located String])
-- Whenever makeDynFlagsConsistent does anything, it starts over, to
-- ensure that a later change doesn't invalidate an earlier check.
-- Be careful not to introduce potential loops!
makeDynFlagsConsistent dflags
 -- Disable -dynamic-too on Windows (#8228, #7134, #5987)
 | os == OSMinGW32 && gopt Opt_BuildDynamicToo dflags
    = let dflags' = gopt_unset dflags Opt_BuildDynamicToo
          warn    = "-dynamic-too is not supported on Windows"
      in loop dflags' warn
 | hscTarget dflags == HscC &&
   not (platformUnregisterised (targetPlatform dflags))
    = if cGhcWithNativeCodeGen == "YES"
      then let dflags' = dflags { hscTarget = HscAsm }
               warn = "Compiler not unregisterised, so using native code generator rather than compiling via C"
           in loop dflags' warn
      else let dflags' = dflags { hscTarget = HscLlvm }
               warn = "Compiler not unregisterised, so using LLVM rather than compiling via C"
           in loop dflags' warn
 | gopt Opt_Hpc dflags && hscTarget dflags == HscInterpreted
    = let dflags' = gopt_unset dflags Opt_Hpc
          warn = "Hpc can't be used with byte-code interpreter. Ignoring -fhpc."
      in loop dflags' warn
 | hscTarget dflags == HscAsm &&
   platformUnregisterised (targetPlatform dflags)
    = loop (dflags { hscTarget = HscC })
           "Compiler unregisterised, so compiling via C"
 | hscTarget dflags == HscAsm &&
   cGhcWithNativeCodeGen /= "YES"
      = let dflags' = dflags { hscTarget = HscLlvm }
            warn = "No native code generator, so using LLVM"
        in loop dflags' warn
 | hscTarget dflags == HscLlvm &&
   not ((arch == ArchX86_64) && (os == OSLinux || os == OSDarwin || os == OSFreeBSD)) &&
   not ((isARM arch) && (os == OSLinux)) &&
   (gopt Opt_PIC dflags || WayDyn `elem` ways dflags)
    = if cGhcWithNativeCodeGen == "YES"
      then let dflags' = dflags { hscTarget = HscAsm }
               warn = "Using native code generator rather than LLVM, as LLVM is incompatible with -fPIC and -dynamic on this platform"
           in loop dflags' warn
      else throwGhcException $ CmdLineError "Can't use -fPIC or -dynamic on this platform"
 | os == OSDarwin &&
   arch == ArchX86_64 &&
   not (gopt Opt_PIC dflags)
    = loop (gopt_set dflags Opt_PIC)
           "Enabling -fPIC as it is always on for this platform"
 | Left err <- checkOptLevel (optLevel dflags) dflags
    = loop (updOptLevel 0 dflags) err

 | LinkInMemory <- ghcLink dflags
 , not (gopt Opt_ExternalInterpreter dflags)
 , rtsIsProfiled
 , isObjectTarget (hscTarget dflags)
 , WayProf `notElem` ways dflags
    = loop dflags{ways = WayProf : ways dflags}
         "Enabling -prof, because -fobject-code is enabled and GHCi is profiled"

 | otherwise = (dflags, [])
    where loc = mkGeneralSrcSpan (fsLit "when making flags consistent")
          loop updated_dflags warning
              = case makeDynFlagsConsistent updated_dflags of
                (dflags', ws) -> (dflags', L loc warning : ws)
          platform = targetPlatform dflags
          arch = platformArch platform
          os   = platformOS   platform

-- -----------------------------------------------------------------------------
-- SSE and AVX

-- TODO: Instead of using a separate predicate (i.e. isSse2Enabled) to
-- check if SSE is enabled, we might have x86-64 imply the -msse2
-- flag.

data SseVersion = SSE1
                | SSE2
                | SSE3
                | SSE4
                | SSE42
                deriving (Eq, Ord)

isSseEnabled :: DynFlags -> Bool
isSseEnabled dflags = case platformArch (targetPlatform dflags) of
    ArchX86_64 -> True
    ArchX86    -> sseVersion dflags >= Just SSE1
    _          -> False

isSse2Enabled :: DynFlags -> Bool
isSse2Enabled dflags = case platformArch (targetPlatform dflags) of
    ArchX86_64 -> -- SSE2 is fixed on for x86_64.  It would be
                  -- possible to make it optional, but we'd need to
                  -- fix at least the foreign call code where the
                  -- calling convention specifies the use of xmm regs,
                  -- and possibly other places.
                  True
    ArchX86    -> sseVersion dflags >= Just SSE2
    _          -> False

isSse4_2Enabled :: DynFlags -> Bool
isSse4_2Enabled dflags = sseVersion dflags >= Just SSE42

isAvxEnabled :: DynFlags -> Bool
isAvxEnabled dflags = avx dflags || avx2 dflags || avx512f dflags

isAvx2Enabled :: DynFlags -> Bool
isAvx2Enabled dflags = avx2 dflags || avx512f dflags

isAvx512cdEnabled :: DynFlags -> Bool
isAvx512cdEnabled dflags = avx512cd dflags

isAvx512erEnabled :: DynFlags -> Bool
isAvx512erEnabled dflags = avx512er dflags

isAvx512fEnabled :: DynFlags -> Bool
isAvx512fEnabled dflags = avx512f dflags

isAvx512pfEnabled :: DynFlags -> Bool
isAvx512pfEnabled dflags = avx512pf dflags

-- -----------------------------------------------------------------------------
-- Linker/compiler information

-- LinkerInfo contains any extra options needed by the system linker.
data LinkerInfo
  = GnuLD    [Option]
  | GnuGold  [Option]
  | DarwinLD [Option]
  | SolarisLD [Option]
  | AixLD    [Option]
  | UnknownLD
  deriving Eq

-- CompilerInfo tells us which C compiler we're using
data CompilerInfo
   = GCC
   | Clang
   | AppleClang
   | AppleClang51
   | UnknownCC
   deriving Eq

-- -----------------------------------------------------------------------------
-- RTS hooks


defaultDynFlag :: DynFlags
defaultDynFlag = DynFlags {}
