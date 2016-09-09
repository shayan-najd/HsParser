{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[Foreign]{Foreign calls}
-}

{-# LANGUAGE DeriveDataTypeable #-}

module Language.Haskell.Syntax.ForeignCall  ( CExportSpec(..)
                                            , CLabelString(..)
                                            , CType(..)
                                            , Header(..)
                                            , CCallTarget(..)
                                            , CCallSpec(..)
                                            , ForeignCall(..)
                                            , CCallConv(..)
                                            , Safety(..)) where

import Language.Haskell.Utility.FastString
import Language.Haskell.Syntax.Module
import Language.Haskell.Syntax.BasicTypes ( SourceText )

import Data.Data

{-
************************************************************************
*                                                                      *
\subsubsection{Data types}
*                                                                      *
************************************************************************
-}

newtype ForeignCall = CCall CCallSpec
  deriving Eq
  {-! derive: Binary !-}

data Safety
  = PlaySafe            -- Might invoke Haskell GC, or do a call back, or
                        -- switch threads, etc.  So make sure things are
                        -- tidy before the call. Additionally, in the threaded
                        -- RTS we arrange for the external call to be executed
                        -- by a separate OS thread, i.e., _concurrently_ to the
                        -- execution of other Haskell threads.

  | PlayInterruptible   -- Like PlaySafe, but additionally
                        -- the worker thread running this foreign call may
                        -- be unceremoniously killed, so it must be scheduled
                        -- on an unbound thread.

  | PlayRisky           -- None of the above can happen; the call will return
                        -- without interacting with the runtime system at all
  deriving ( Eq, Show, Data )
        -- Show used just for Show Lex.Token, I think
  {-! derive: Binary !-}

{-
************************************************************************
*                                                                      *
\subsubsection{Calling C}
*                                                                      *
************************************************************************
-}

data CExportSpec
  = CExportStatic               -- foreign export ccall foo :: ty
        SourceText              -- of the CLabelString.
                                -- See note [Pragma source text] in BasicTypes
        CLabelString            -- C Name of exported function
        CCallConv
  deriving Data
  {-! derive: Binary !-}

data CCallSpec
  =  CCallSpec  CCallTarget     -- What to call
                CCallConv       -- Calling convention to use.
                Safety
  deriving( Eq )
  {-! derive: Binary !-}

-- The call target:

-- | How to call a particular function in C-land.
data CCallTarget
  -- An "unboxed" ccall# to named function in a particular package.
  = StaticTarget
        SourceText                -- of the CLabelString.
                                  -- See note [Pragma source text] in BasicTypes
        CLabelString                    -- C-land name of label.

        (Maybe UnitId)              -- What package the function is in.
                                        -- If Nothing, then it's taken to be in the current package.
                                        -- Note: This information is only used for PrimCalls on Windows.
                                        --       See CLabel.labelDynamic and CoreToStg.coreToStgApp
                                        --       for the difference in representation between PrimCalls
                                        --       and ForeignCalls. If the CCallTarget is representing
                                        --       a regular ForeignCall then it's safe to set this to Nothing.

  -- The first argument of the import is the name of a function pointer (an Addr#).
  --    Used when importing a label as "foreign import ccall "dynamic" ..."
        Bool                            -- True => really a function
                                        -- False => a value; only
                                        -- allowed in CAPI imports
  | DynamicTarget

  deriving( Eq, Data )
  {-! derive: Binary !-}
{-
Stuff to do with calling convention:

ccall:          Caller allocates parameters, *and* deallocates them.

stdcall:        Caller allocates parameters, callee deallocates.
                Function name has @N after it, where N is number of arg bytes
                e.g.  _Foo@8. This convention is x86 (win32) specific.

See: http://www.programmersheaven.com/2/Calling-conventions
-}

-- any changes here should be replicated in  the CallConv type in template haskell
data CCallConv = CCallConv | CApiConv | StdCallConv | PrimCallConv | JavaScriptCallConv
  deriving (Eq, Data)
  {-! derive: Binary !-}

type CLabelString = FastString          -- A C label, completely unencoded

-- The filename for a C header file
-- Note [Pragma source text] in BasicTypes
data Header = Header SourceText FastString
    deriving (Eq, Data)


data CType = CType SourceText -- Note [Pragma source text] in BasicTypes
                   (Maybe Header) -- header to include for this type
                   (SourceText,FastString) -- the type itself
    deriving (Eq, Data)
