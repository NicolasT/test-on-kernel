{-# LANGUAGE CApiFFI #-}

module System.Linux.Reboot (
      RebootCommand(..)
    , reboot
    ) where

import Control.Monad (unless)
import Data.Int (Int32)
import Foreign.C.Error (throwErrnoIfMinus1)

#include <unistd.h>
#include <sys/reboot.h>

data RebootCommand = Autoboot
    deriving (Show, Eq)

foreign import capi unsafe "sys/reboot.h reboot"
  c_reboot :: #{type int} -> IO #{type int}

reboot :: RebootCommand -> IO a
reboot cmd = do
    rc <- throwErrnoIfMinus1 "reboot" $ c_reboot $ case cmd of
        Autoboot -> #{const RB_AUTOBOOT}

    unless (rc == 0) $
        fail $ "reboot returned. Furthermore, it returned " ++ show rc ++ "."

    fail "reboot returned"
