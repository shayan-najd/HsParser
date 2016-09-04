{-
(c) The AQUA Project, Glasgow University, 1994-1998

\section[ErrsUtils]{Utilities for error reporting}
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}

module U.ErrUtils (
        -- * Basic types
        Validity(..), andValid, allValid, isValid, getInvalids,
        Severity(..),

        -- * Messages
        MsgDoc, ErrMsg, ErrDoc, errDoc, WarnMsg,
        Messages, ErrorMessages, WarningMessages,
        unionMessages,
        errMsgSpan, errMsgContext,
        errorsFound, isEmptyMessages,

        -- ** Formatting
        pprMessageBag, pprErrMsgBagWithLoc,
        pprLocErrMsg, printBagOfErrors,

        -- ** Construction
        emptyMessages, mkLocMessage, mkLocMessageAnn, makeIntoWarning,
        mkErrMsg, mkPlainErrMsg, mkErrDoc, mkLongErrMsg, mkWarnMsg,
        mkPlainWarnMsg,
        warnIsErrorMsg, mkLongWarnMsg,

        -- * Utilities
        doIfSet, doIfSet_dyn,

        -- * Issuing messages during compilation
        putMsg, printInfoForUser, printOutputForUser,
        logInfo, logOutput,
        errorMsg, warningMsg,
        fatalErrorMsg, fatalErrorMsg', fatalErrorMsg'',
        compilationProgressMsg,
        showPass, withTiming,
        debugTraceMsg,
        prettyPrintGhcErrors,
    ) where

#include "HsVersions.h"

import U.Bag
import U.Exception
import U.Outputable
import U.Panic
import SrcLoc
import U.DynFlags

import Data.List
import Data.Maybe       ( fromMaybe )
import Data.Ord
import Control.Monad.IO.Class
import GHC.Conc         ( getAllocationCounter )
import System.CPUTime

-------------------------
type MsgDoc  = SDoc

-------------------------
data Validity
  = IsValid            -- ^ Everything is fine
  | NotValid MsgDoc    -- ^ A problem, and some indication of why

isValid :: Validity -> Bool
isValid IsValid       = True
isValid (NotValid {}) = False

andValid :: Validity -> Validity -> Validity
andValid IsValid v = v
andValid v _       = v

-- | If they aren't all valid, return the first
allValid :: [Validity] -> Validity
allValid []       = IsValid
allValid (v : vs) = v `andValid` allValid vs

getInvalids :: [Validity] -> [MsgDoc]
getInvalids vs = [d | NotValid d <- vs]

-- -----------------------------------------------------------------------------
-- Basic error messages: just render a message with a source location.

type Messages        = (WarningMessages, ErrorMessages)
type WarningMessages = Bag WarnMsg
type ErrorMessages   = Bag ErrMsg

unionMessages :: Messages -> Messages -> Messages
unionMessages (warns1, errs1) (warns2, errs2) =
  (warns1 `unionBags` warns2, errs1 `unionBags` errs2)

data ErrMsg = ErrMsg {
        errMsgSpan        :: SrcSpan,
        errMsgContext     :: PrintUnqualified,
        errMsgDoc         :: ErrDoc,
        -- | This has the same text as errDocImportant . errMsgDoc.
        errMsgShortString :: String,
        errMsgSeverity    :: Severity,
        errMsgReason      :: WarnReason
        }
        -- The SrcSpan is used for sorting errors into line-number order

-- | Categorise error msgs by their importance.  This is so each section can
-- be rendered visually distinct.  See Note [Error report] for where these come
-- from.
data ErrDoc = ErrDoc {
        -- | Primary error msg.
        errDocImportant :: [MsgDoc],
        -- | Context e.g. \"In the second argument of ...\".
        _errDocContext :: [MsgDoc],
        -- | Supplementary information, e.g. \"Relevant bindings include ...\".
        _errDocSupplementary :: [MsgDoc]
        }

errDoc :: [MsgDoc] -> [MsgDoc] -> [MsgDoc] -> ErrDoc
errDoc = ErrDoc

type WarnMsg = ErrMsg

data Severity
  = SevOutput
  | SevFatal
  | SevInteractive

  | SevDump
    -- ^ Log messagse intended for compiler developers
    -- No file/line/column stuff

  | SevInfo
    -- ^ Log messages intended for end users.
    -- No file/line/column stuff.

  | SevWarning
  | SevError
    -- ^ SevWarning and SevError are used for warnings and errors
    --   o The message has a file/line/column heading,
    --     plus "warning:" or "error:",
    --     added by mkLocMessags
    --   o Output is intended for end users


instance Show ErrMsg where
    show em = errMsgShortString em

pprMessageBag :: Bag MsgDoc -> SDoc
pprMessageBag msgs = vcat (punctuate blankLine (bagToList msgs))

mkLocMessage :: Severity -> SrcSpan -> MsgDoc -> MsgDoc
mkLocMessage = mkLocMessageAnn Nothing

mkLocMessageAnn :: Maybe String -> Severity -> SrcSpan -> MsgDoc -> MsgDoc
  -- Always print the location, even if it is unhelpful.  Error messages
  -- are supposed to be in a standard format, and one without a location
  -- would look strange.  Better to say explicitly "<no location info>".
mkLocMessageAnn ann severity locn msg
    = sdocWithDynFlags $ \dflags ->
      let locn' = if gopt Opt_ErrorSpans dflags
                  then ppr locn
                  else ppr (srcSpanStart locn)
      in hang (locn' <> colon <+> sev_info <> opt_ann) 4 msg
  where
    -- Add prefixes, like    Foo.hs:34: warning:
    --                           <the warning message>
    sev_info = case severity of
                 SevWarning -> text "warning:"
                 SevError -> text "error:"
                 SevFatal -> text "fatal:"
                 _ -> empty

    -- Add optional information
    opt_ann = text $ maybe "" (\i -> " ["++i++"]") ann

makeIntoWarning :: WarnReason -> ErrMsg -> ErrMsg
makeIntoWarning reason err = err
    { errMsgSeverity = SevWarning
    , errMsgReason = reason }

-- -----------------------------------------------------------------------------
-- Collecting up messages for later ordering and printing.

mk_err_msg :: DynFlags -> Severity -> SrcSpan -> PrintUnqualified -> ErrDoc -> ErrMsg
mk_err_msg dflags sev locn print_unqual doc
 = ErrMsg { errMsgSpan = locn
          , errMsgContext = print_unqual
          , errMsgDoc = doc
          , errMsgShortString = showSDoc dflags (vcat (errDocImportant doc))
          , errMsgSeverity = sev
          , errMsgReason = NoReason }

mkErrDoc :: DynFlags -> SrcSpan -> PrintUnqualified -> ErrDoc -> ErrMsg
mkErrDoc dflags = mk_err_msg dflags SevError

mkLongErrMsg, mkLongWarnMsg   :: DynFlags -> SrcSpan -> PrintUnqualified -> MsgDoc -> MsgDoc -> ErrMsg
-- ^ A long (multi-line) error message
mkErrMsg, mkWarnMsg           :: DynFlags -> SrcSpan -> PrintUnqualified -> MsgDoc            -> ErrMsg
-- ^ A short (one-line) error message
mkPlainErrMsg, mkPlainWarnMsg :: DynFlags -> SrcSpan ->                     MsgDoc            -> ErrMsg
-- ^ Variant that doesn't care about qualified/unqualified names

mkLongErrMsg   dflags locn unqual msg extra = mk_err_msg dflags SevError   locn unqual        (ErrDoc [msg] [] [extra])
mkErrMsg       dflags locn unqual msg       = mk_err_msg dflags SevError   locn unqual        (ErrDoc [msg] [] [])
mkPlainErrMsg  dflags locn        msg       = mk_err_msg dflags SevError   locn alwaysQualify (ErrDoc [msg] [] [])
mkLongWarnMsg  dflags locn unqual msg extra = mk_err_msg dflags SevWarning locn unqual        (ErrDoc [msg] [] [extra])
mkWarnMsg      dflags locn unqual msg       = mk_err_msg dflags SevWarning locn unqual        (ErrDoc [msg] [] [])
mkPlainWarnMsg dflags locn        msg       = mk_err_msg dflags SevWarning locn alwaysQualify (ErrDoc [msg] [] [])

----------------
emptyMessages :: Messages
emptyMessages = (emptyBag, emptyBag)

isEmptyMessages :: Messages -> Bool
isEmptyMessages (warns, errs) = isEmptyBag warns && isEmptyBag errs

warnIsErrorMsg :: DynFlags -> ErrMsg
warnIsErrorMsg dflags
    = mkPlainErrMsg dflags noSrcSpan (text "\nFailing due to -Werror.")

errorsFound :: DynFlags -> Messages -> Bool
errorsFound _dflags (_warns, errs) = not (isEmptyBag errs)

printBagOfErrors :: DynFlags -> Bag ErrMsg -> IO ()
printBagOfErrors dflags bag_of_errors
  = sequence_ [ let style = mkErrStyle dflags unqual
                in log_action dflags dflags reason sev s style (formatErrDoc dflags doc)
              | ErrMsg { errMsgSpan      = s,
                         errMsgDoc       = doc,
                         errMsgSeverity  = sev,
                         errMsgReason    = reason,
                         errMsgContext   = unqual } <- sortMsgBag (Just dflags)
                                                                  bag_of_errors ]

formatErrDoc :: DynFlags -> ErrDoc -> SDoc
formatErrDoc dflags (ErrDoc important context supplementary)
  = case msgs of
        [msg] -> vcat msg
        _ -> vcat $ map starred msgs
    where
    msgs = filter (not . null) $ map (filter (not . U.Outputable.isEmpty dflags))
        [important, context, supplementary]
    starred = (bullet<+>) . vcat
    bullet = text $ if U.DynFlags.useUnicode dflags then "•" else "*"

pprErrMsgBagWithLoc :: Bag ErrMsg -> [SDoc]
pprErrMsgBagWithLoc bag = [ pprLocErrMsg item | item <- sortMsgBag Nothing bag ]

pprLocErrMsg :: ErrMsg -> SDoc
pprLocErrMsg (ErrMsg { errMsgSpan      = s
                     , errMsgDoc       = doc
                     , errMsgSeverity  = sev
                     , errMsgContext   = unqual })
  = sdocWithDynFlags $ \dflags ->
    withPprStyle (mkErrStyle dflags unqual) $
    mkLocMessage sev s (formatErrDoc dflags doc)

sortMsgBag :: Maybe DynFlags -> Bag ErrMsg -> [ErrMsg]
sortMsgBag dflags = sortBy (maybeFlip $ comparing errMsgSpan) . bagToList
  where maybeFlip :: (a -> a -> b) -> (a -> a -> b)
        maybeFlip
          | fromMaybe False (fmap reverseErrors dflags) = flip
          | otherwise                                   = id


doIfSet :: Bool -> IO () -> IO ()
doIfSet flag action | flag      = action
                    | otherwise = return ()

doIfSet_dyn :: DynFlags -> GeneralFlag -> IO () -> IO()
doIfSet_dyn dflags flag action | gopt flag dflags = action
                               | otherwise        = return ()

-- -----------------------------------------------------------------------------
-- Outputting messages from the compiler

-- We want all messages to go through one place, so that we can
-- redirect them if necessary.  For example, when GHC is used as a
-- library we might want to catch all messages that GHC tries to
-- output and do something else with them.

ifVerbose :: DynFlags -> Int -> IO () -> IO ()
ifVerbose dflags val act
  | verbosity dflags >= val = act
  | otherwise               = return ()

errorMsg :: DynFlags -> MsgDoc -> IO ()
errorMsg dflags msg
   = log_action dflags dflags NoReason SevError noSrcSpan (defaultErrStyle dflags) msg

warningMsg :: DynFlags -> MsgDoc -> IO ()
warningMsg dflags msg
   = log_action dflags dflags NoReason SevWarning noSrcSpan (defaultErrStyle dflags) msg

fatalErrorMsg :: DynFlags -> MsgDoc -> IO ()
fatalErrorMsg dflags msg = fatalErrorMsg' (log_action dflags) dflags msg

fatalErrorMsg' :: LogAction -> DynFlags -> MsgDoc -> IO ()
fatalErrorMsg' la dflags msg =
    la dflags NoReason SevFatal noSrcSpan (defaultErrStyle dflags) msg

fatalErrorMsg'' :: FatalMessager -> String -> IO ()
fatalErrorMsg'' fm msg = fm msg

compilationProgressMsg :: DynFlags -> String -> IO ()
compilationProgressMsg dflags msg
  = ifVerbose dflags 1 $
    logOutput dflags defaultUserStyle (text msg)

showPass :: DynFlags -> String -> IO ()
showPass dflags what
  = ifVerbose dflags 2 $
    logInfo dflags defaultUserStyle (text "***" <+> text what <> colon)

-- | Time a compilation phase.
--
-- When timings are enabled (e.g. with the @-v2@ flag), the allocations
-- and CPU time used by the phase will be reported to stderr. Consider
-- a typical usage: @withTiming getDynFlags (text "simplify") force pass@.
-- When timings are enabled the following costs are included in the
-- produced accounting,
--
--  - The cost of executing @pass@ to a result @r@ in WHNF
--  - The cost of evaluating @force r@ to WHNF (e.g. @()@)
--
-- The choice of the @force@ function depends upon the amount of forcing
-- desired; the goal here is to ensure that the cost of evaluating the result
-- is, to the greatest extent possible, included in the accounting provided by
-- 'withTiming'. Often the pass already sufficiently forces its result during
-- construction; in this case @const ()@ is a reasonable choice.
-- In other cases, it is necessary to evaluate the result to normal form, in
-- which case something like @Control.DeepSeq.rnf@ is appropriate.
--
-- To avoid adversely affecting compiler performance when timings are not
-- requested, the result is only forced when timings are enabled.
withTiming :: MonadIO m
           => m DynFlags  -- ^ A means of getting a 'DynFlags' (often
                          -- 'getDynFlags' will work here)
           -> SDoc        -- ^ The name of the phase
           -> (a -> ())   -- ^ A function to force the result
                          -- (often either @const ()@ or 'rnf')
           -> m a         -- ^ The body of the phase to be timed
           -> m a
withTiming getDFlags what force_result action
  = do dflags <- getDFlags
       if verbosity dflags >= 2
          then do liftIO $ logInfo dflags defaultUserStyle
                         $ text "***" <+> what <> colon
                  alloc0 <- liftIO getAllocationCounter
                  start <- liftIO getCPUTime
                  !r <- action
                  () <- pure $ force_result r
                  end <- liftIO getCPUTime
                  alloc1 <- liftIO getAllocationCounter
                  -- recall that allocation counter counts down
                  let alloc = alloc0 - alloc1
                  liftIO $ logInfo dflags defaultUserStyle
                      (text "!!!" <+> what <> colon <+> text "finished in"
                       <+> doublePrec 2 (realToFrac (end - start) * 1e-9)
                       <+> text "milliseconds"
                       <> comma
                       <+> text "allocated"
                       <+> doublePrec 3 (realToFrac alloc / 1024 / 1024)
                       <+> text "megabytes")
                  pure r
           else action

debugTraceMsg :: DynFlags -> Int -> MsgDoc -> IO ()
debugTraceMsg dflags val msg = ifVerbose dflags val $
                               logInfo dflags defaultDumpStyle msg

putMsg :: DynFlags -> MsgDoc -> IO ()
putMsg dflags msg = logInfo dflags defaultUserStyle msg

printInfoForUser :: DynFlags -> PrintUnqualified -> MsgDoc -> IO ()
printInfoForUser dflags print_unqual msg
  = logInfo dflags (mkUserStyle print_unqual AllTheWay) msg

printOutputForUser :: DynFlags -> PrintUnqualified -> MsgDoc -> IO ()
printOutputForUser dflags print_unqual msg
  = logOutput dflags (mkUserStyle print_unqual AllTheWay) msg

logInfo :: DynFlags -> PprStyle -> MsgDoc -> IO ()
logInfo dflags sty msg
  = log_action dflags dflags NoReason SevInfo noSrcSpan sty msg

logOutput :: DynFlags -> PprStyle -> MsgDoc -> IO ()
-- ^ Like 'logInfo' but with 'SevOutput' rather then 'SevInfo'
logOutput dflags sty msg
  = log_action dflags dflags NoReason SevOutput noSrcSpan sty msg

prettyPrintGhcErrors :: ExceptionMonad m => DynFlags -> m a -> m a
prettyPrintGhcErrors dflags
    = ghandle $ \e -> case e of
                      PprPanic str doc ->
                          pprDebugAndThen dflags panic (text str) doc
                      PprSorry str doc ->
                          pprDebugAndThen dflags sorry (text str) doc
                      PprProgramError str doc ->
                          pprDebugAndThen dflags pgmError (text str) doc
                      _ ->
                          liftIO $ throwIO e
