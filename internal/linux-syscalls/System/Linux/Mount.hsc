{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}

module System.Linux.Mount (
      MountFlag(..)
    , mount
    , mount'
    , umount
    ) where

import Data.Bits ((.|.))
import Data.Functor ((<&>))
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Data.Word (Word64)
import Foreign.C.Error (throwErrnoPathIfMinus1)
import Foreign.C.String (CString, withCString)
import Foreign.Ptr (Ptr, nullPtr)

import Foreign.Utils (throwIfNonZero)

#include <sys/mount.h>

data MountFlag = MS_NOSUID
               | MS_NOEXEC
               | MS_RELATIME
               | MS_MOVE
               | MS_NODEV
    deriving (Show, Eq)

mount :: String -> FilePath -> String -> [MountFlag] -> Maybe (Ptr a) -> IO ()
mount source target filesystemtype mountflags data_ =
    throwIfNonZero "mount" $ throwErrnoPathIfMinus1 "mount" target $
        withCString source $ \source' ->
        withCString target $ \target' ->
        withCString filesystemtype $ \filesystemtype' -> do
            let data_' = fromMaybe nullPtr data_
            c_mount source' target' filesystemtype' mountflags' data_'
  where
    mountflags' = foldr (.|.) 0 $ mountflags <&> \case
        MS_NOSUID -> #{const MS_NOSUID}
        MS_NOEXEC -> #{const MS_NOEXEC}
        MS_RELATIME -> #{const MS_RELATIME}
        MS_MOVE -> #{const MS_MOVE}
        MS_NODEV -> #{const MS_NODEV}

mount' :: String -> FilePath -> String -> [MountFlag] -> IO ()
mount' source target filesystemtype mountflags = mount source target filesystemtype mountflags (Nothing :: Maybe (Ptr Void))

foreign import capi unsafe "sys/mount.h mount"
    c_mount :: CString -> CString -> CString -> #{type unsigned long} -> Ptr a -> IO #{type int}


umount :: FilePath -> IO ()
umount target = throwIfNonZero "umount" $ throwErrnoPathIfMinus1 "mount" target $ withCString target c_umount

foreign import capi unsafe "sys/mount.h umount"
    c_umount :: CString -> IO #{type int}
