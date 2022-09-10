{-# LANGUAGE CApiFFI #-}

module System.Linux (
      chdir
    , chroot
    ) where

import Data.Int (Int32)
import Foreign.C.Error (throwErrnoIfMinus1, throwErrnoPathIfMinus1)
import Foreign.C.String (CString, withCString)

import Foreign.Utils (throwIfNonZero)

foreign import capi "unistd.h chdir"
    c_chdir :: CString -> IO #{type int}

chdir :: FilePath -> IO ()
chdir path = throwIfNonZero "chdir" $ throwErrnoPathIfMinus1 "chdir" path $ withCString path c_chdir

foreign import capi "unistd.h chroot"
    c_chroot :: CString -> IO #{type int}

chroot :: FilePath -> IO ()
chroot path = throwIfNonZero "chroot" $ throwErrnoPathIfMinus1 "chroot" path $ withCString path c_chroot
