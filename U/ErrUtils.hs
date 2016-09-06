{-
(c) The AQUA Project, Glasgow University, 1994-1998

\section[ErrsUtils]{Utilities for error reporting}
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}

module U.ErrUtils (MsgDoc,
                   Messages,
                   ErrMsg,
                   mkWarnMsg,
                   makeIntoWarning,
                   emptyMessages
                   ) where
#include "HsVersions.h"

import U.Bag
import U.Outputable
import SrcLoc
import U.DynFlags


-------------------------
type MsgDoc  = SDoc

-- -----------------------------------------------------------------------------
-- Basic error messages: just render a message with a source location.

type Messages        = (WarningMessages, ErrorMessages)
type WarningMessages = Bag WarnMsg
type ErrorMessages   = Bag ErrMsg

data ErrMsg = ErrMsg {
        errMsgSpan        :: SrcSpan,
        errMsgContext     :: PrintUnqualified,
        errMsgDoc         :: ErrDoc,
        -- | This has the same text as errDocImportant . errMsgDoc.
        errMsgShortString :: String,
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

type WarnMsg = ErrMsg


instance Show ErrMsg where
    show em = errMsgShortString em



makeIntoWarning :: WarnReason -> ErrMsg -> ErrMsg
makeIntoWarning reason err = err
    { errMsgReason = reason }

-- -----------------------------------------------------------------------------
-- Collecting up messages for later ordering and printing.

mk_err_msg :: DynFlags -> SrcSpan -> PrintUnqualified -> ErrDoc -> ErrMsg
mk_err_msg dflags locn print_unqual doc
 = ErrMsg { errMsgSpan = locn
          , errMsgContext = print_unqual
          , errMsgDoc = doc
          , errMsgShortString = showSDoc dflags (vcat (errDocImportant doc))
          , errMsgReason = NoReason }

-- ^ A long (multi-line) error message
mkWarnMsg :: DynFlags -> SrcSpan -> PrintUnqualified -> MsgDoc -> ErrMsg
mkWarnMsg      dflags locn unqual msg
  = mk_err_msg dflags locn unqual        (ErrDoc [msg] [] [])

----------------
emptyMessages :: Messages
emptyMessages = (emptyBag, emptyBag)
