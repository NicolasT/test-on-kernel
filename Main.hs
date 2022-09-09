{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Colog (LoggerT(..), cmap, fmtMessage, logError, logInfo, logTextStdout, usingLoggerT)
import Control.Exception.Base (SomeException, throwIO)
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch(..), MonadMask, MonadThrow(..), finally, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import System.IO (IOMode(..), hFlush, hPutStrLn, stderr, withFile)
import System.Linux.Mount (MountFlag(..), mount')
import System.Linux.Reboot (RebootCommand(..), reboot)
import System.Posix.Directory (createDirectory)
import System.Posix.Env (setEnv)
import System.Posix.Process (getProcessID)

loggerAction = cmap fmtMessage logTextStdout

main :: IO ()
main = usingLoggerT loggerAction $ do
    pid <- liftIO getProcessID
    unless (pid == 1) $
        logError "Not running as PID 1"

    finallyHalt $ do
        logInfo "Mounting stuff"
        liftIO $ do
            createDirectory "/proc" 0o755
            mount' "proc" "/proc" "proc" [MS_NOSUID, MS_NOEXEC, MS_RELATIME]
            setEnv "LC_ALL" "C.UTF-8" True
            putStrLn "Hello, Haskell!"
            reboot Autoboot

finallyHalt :: (MonadIO m, MonadMask m, MonadFail m) => m a -> m b
finallyHalt act = finally act' $ do
    -- hPutStrLn stderr "Halting system"
    liftIO $ reboot HaltSystem
    fail "reboot returned"
  where
    act' = do
        t <- try act
        case t of
            Left e -> fail (show (e :: SomeException)) -- print (e :: SomeException) >> throwIO e
            Right _ -> return ()
        return undefined


deriving instance MonadFail m => MonadFail (LoggerT msg m)
deriving instance MonadThrow m => MonadThrow (LoggerT msg m)
deriving instance MonadCatch m => MonadCatch (LoggerT msg m)
deriving instance MonadMask m => MonadMask (LoggerT msg m)
