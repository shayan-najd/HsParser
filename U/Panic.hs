{-
(c) The University of Glasgow 2006
(c) The GRASP Project, Glasgow University, 1992-2000

Defines basic functions for printing error messages.

It's hard to put these functions anywhere else without causing
some unnecessary loops in the module dependency graph.
-}
{-# LANGUAGE CPP, ScopedTypeVariables #-}
#define TargetPlatform_NAME ""
module U.Panic (panic,trace,pgmError,
                sorry,assertPanic,GhcException(..)) where

import {-# SOURCE #-} U.Outputable (SDoc, showSDocUnsafe)

import U.Exception

import Debug.Trace        ( trace )
import System.IO.Unsafe
import System.Environment
import GHC.Stack

cProjectVersion       :: String
cProjectVersion       = "8.1.20160617"

-- | GHC's own exception type
--   error messages all take the form:
--
--  @
--      <location>: <error>
--  @
--
--   If the location is on the command line, or in GHC itself, then
--   <location>="ghc".  All of the error types below correspond to
--   a <location> of "ghc", except for ProgramError (where the string is
--  assumed to contain a location already, so we don't print one).

data GhcException
  -- | Some other fatal signal (SIGHUP,SIGTERM)
  = Signal Int

  -- | Prints the short usage msg after the error
  | UsageError   String

  -- | A problem with the command line arguments, but don't print usage.
  | CmdLineError String

  -- | The 'impossible' happened.
  | Panic        String
  | PprPanic     String SDoc

  -- | The user tickled something that's known not to work yet,
  --   but we're not counting it as a bug.
  | Sorry        String
  | PprSorry     String SDoc

  -- | An installation problem.
  | InstallationError String

  -- | An error in the user's code, probably.
  | ProgramError    String
  | PprProgramError String SDoc

instance Exception GhcException

instance Show GhcException where
  showsPrec _ e@(ProgramError _) = showGhcException e
  showsPrec _ e@(CmdLineError _) = showString "<command line>: " . showGhcException e
  showsPrec _ e = showString progName . showString ": " . showGhcException e


-- | The name of this GHC.
progName :: String
progName = unsafePerformIO (getProgName)
{-# NOINLINE progName #-}


-- | Short usage information to display when we are given the wrong cmd line arguments.
short_usage :: String
short_usage = "Usage: For basic information, try the `--help' option."

-- | Append a description of the given exception to this string.
--
-- Note that this uses 'DynFlags.unsafeGlobalDynFlags', which may have some
-- uninitialized fields if invoked before 'GHC.initGhcMonad' has been called.
-- If the error message to be printed includes a pretty-printer document
-- which forces one of these fields this call may bottom.
showGhcException :: GhcException -> ShowS
showGhcException exception
 = case exception of
        UsageError str
         -> showString str . showChar '\n' . showString short_usage

        CmdLineError str        -> showString str
        PprProgramError str  sdoc  ->
            showString str . showString "\n\n" .
            showString (showSDocUnsafe sdoc)
        ProgramError str        -> showString str
        InstallationError str   -> showString str
        Signal n                -> showString "signal: " . shows n

        PprPanic  s sdoc ->
            panicMsg $ showString s . showString "\n\n"
                     . showString (showSDocUnsafe sdoc)
        Panic s -> panicMsg (showString s)

        PprSorry  s sdoc ->
            sorryMsg $ showString s . showString "\n\n"
                     . showString (showSDocUnsafe sdoc)
        Sorry s -> sorryMsg (showString s)
  where
    sorryMsg :: ShowS -> ShowS
    sorryMsg s =
        showString "sorry! (unimplemented feature or known bug)\n"
      . showString ("  (GHC version " ++ cProjectVersion ++ " for " ++ TargetPlatform_NAME ++ "):\n\t")
      . s . showString "\n"

    panicMsg :: ShowS -> ShowS
    panicMsg s =
        showString "panic! (the 'impossible' happened)\n"
      . showString ("  (GHC version " ++ cProjectVersion ++ " for " ++ TargetPlatform_NAME ++ "):\n\t")
      . s . showString "\n\n"
      . showString "Please report this as a GHC bug:  http://www.haskell.org/ghc/reportabug\n"


throwGhcException :: GhcException -> a
throwGhcException = U.Exception.throw

-- | Panics and asserts.
panic, sorry, pgmError :: String -> a
panic    x = unsafeDupablePerformIO $ do
   stack <- ccsToStrings =<< getCurrentCCS x
   if null stack
      then throwGhcException (Panic x)
      else throwGhcException (Panic (x ++ '\n' : renderStack stack))

sorry    x = throwGhcException (Sorry x)
pgmError x = throwGhcException (ProgramError x)


-- | Throw an failed assertion exception for a given filename and line number.
assertPanic :: String -> Int -> a
assertPanic file line =
  U.Exception.throw (U.Exception.AssertionFailed
           ("ASSERT failed! file " ++ file ++ ", line " ++ show line))
