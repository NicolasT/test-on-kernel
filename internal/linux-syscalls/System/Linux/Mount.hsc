{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}

module System.Linux.Mount (
      MountFlag(..)
    , mount
    , mount'
    ) where

import Data.Bits ((.|.))
import Data.Functor ((<&>))
import Data.Int (Int32)
import Data.Word (Word64)
import Foreign.C.Error (throwErrnoIfMinus1)
import Foreign.C.String (CString, withCString)
import Foreign.Marshal.Utils (maybeWith, with)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable)

import Foreign.Utils (throwIfNonZero)

#include <sys/mount.h>

data MountFlag = MS_NOSUID
               | MS_NOEXEC
               | MS_RELATIME
    deriving (Show, Eq)

mount :: Storable a => String -> FilePath -> String -> [MountFlag] -> Maybe a -> IO ()
mount source target filesystemtype mountflags data_ =
    throwIfNonZero "mount" $ throwErrnoIfMinus1 "mount" $
        withCString source $ \source' ->
        withCString target $ \target' ->
        withCString filesystemtype $ \filesystemtype' ->
        maybeWith with data_ $ \data_' ->
            c_mount source' target' filesystemtype' mountflags' data_'
  where
    mountflags' = foldr (.|.) 0 $ mountflags <&> \case
        MS_NOSUID -> #{const MS_NOSUID}
        MS_NOEXEC -> #{const MS_NOEXEC}
        MS_RELATIME -> #{const MS_RELATIME}

mount' :: String -> FilePath -> String -> [MountFlag] -> IO ()
mount' source target filesystemtype mountflags = mount source target filesystemtype mountflags (Nothing :: Maybe ())

foreign import capi unsafe "sys/mount.h mount"
    c_mount :: CString -> CString -> CString -> #{type unsigned long} -> Ptr a -> IO #{type int}
