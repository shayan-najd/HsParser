
-- | A description of the platform we're compiling for.
--
module Platform (
        Platform(platformWordSize,platformOS) ,OS(..))

where

-- | Contains enough information for the native code generator to emit
--      code for this platform.
data Platform
        = Platform {
              platformOS                       :: OS,
              platformWordSize                 :: {-# UNPACK #-} !Int
          }
        deriving (Read, Show, Eq)

-- | Operating systems that the native code generator knows about.
--      Having OSUnknown should produce a sensible default, but no promises.
data OS
        = OSUnknown
        | OSLinux
        | OSDarwin
        | OSiOS
        | OSSolaris2
        | OSMinGW32
        | OSFreeBSD
        | OSDragonFly
        | OSOpenBSD
        | OSNetBSD
        | OSKFreeBSD
        | OSHaiku
        | OSQNXNTO
        | OSAndroid
        | OSAIX
        deriving (Read, Show, Eq)
