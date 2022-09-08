{-# LANGUAGE CApiFFI #-}

module Utils where

import Data.Int
import Data.Word

foreign import capi "sys/io.h ioperm"
    c_ioperm :: #{type unsigned long} -> #{type unsigned long} -> #{type int} -> IO #{type int}

foreign import capi "sys/io.h outl"
    c_outl :: #{type unsigned int} -> #{type unsigned short} -> IO ()
