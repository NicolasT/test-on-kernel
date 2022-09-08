module Main where

import Control.Exception.Base (SomeException, finally, onException, throwIO, try)
import Control.Monad (unless)
import System.IO (IOMode(..), hFlush, hPutStrLn, stderr, withFile)
import System.Linux.Reboot (RebootCommand(..), reboot)
import System.Posix.Process (getProcessID)

import Data.Int
import Data.Word
import Foreign.C.String
import Foreign.Ptr
import System.Directory

main :: IO ()
main = do
    pid <- getProcessID
    unless (pid == 1) $
        fail "Not running as PID 1"

    finallyHalt $ do
        putStrLn "Mounting stuff"
        mount "devtmpfs" "/dev" "devtmpfs" 0 nullPtr
        listDirectory "/dev" >>= print
        putStrLn "Hello, Haskell!"
        withFile "/dev/vport0p1" WriteMode $ \fd -> do
            hPutStrLn fd "Hello!"
            hFlush fd
            putStrLn "Wrote the stuff"
        reboot Autoboot

finallyHalt :: IO a -> IO b
finallyHalt act = finally act' $ do
    hPutStrLn stderr "Halting system"
    reboot HaltSystem
    fail "reboot returned"
  where
    act' = do
        t <- try act
        case t of
            Left e -> print (e :: SomeException) >> throwIO e
            Right _ -> return ()
        return undefined


foreign import ccall unsafe "sys/mount.h mount"
    c_mount :: CString -> CString -> CString -> Word64 -> Ptr a -> IO Int32

mount s t f m d = withCString s $ \s' -> withCString t $ \t' -> withCString f $ \f' -> c_mount s' t' f' m d
