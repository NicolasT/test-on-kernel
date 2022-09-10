{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getProgName)
import Data.Maybe
import Options.Applicative
import System.Posix.Files
import System.Directory (copyFile, listDirectory)
import System.Linux
import Foreign.C.String (withCString)

import Colog (LoggerT(..), cmap, fmtMessage, logError, logInfo, logTextStdout, usingLoggerT)
import Colog.Instances ()
import Control.Exception.Base (SomeException, throwIO)
import Control.Monad (unless)
import Control.Monad.Catch (MonadCatch(..), MonadMask, MonadThrow(..), finally, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import System.IO (IOMode(..), hFlush, hPrint, hPutStrLn, stderr, withFile)
import System.Linux.Mount (MountFlag(..), mount, mount', umount)
import System.Linux.Reboot (RebootCommand(..), reboot)
import System.Posix.Directory (createDirectory)
import System.Posix.Env (getEnvironment, setEnv)
import System.Posix.Process (executeFile, getProcessID)

import qualified GHC.IO.Encoding.Iconv

loggerAction = cmap fmtMessage logTextStdout

main :: IO ()
main = do
    progName <- getProgName
    putStrLn $ "progname: " ++ progName
    case progName of
        "init" -> actAsInit
        _ -> actAsNonInit

data InitStage = Stage0 | Stage1 | Stage2
    deriving (Show, Eq)

data InitArgs = InitArgs { initStage :: InitStage }
    deriving (Show, Eq)

stage1 :: Parser InitStage
stage1 = flag' Stage1 ( long "stage1"
                      )

stage2 :: Parser InitStage
stage2 = flag' Stage2 ( long "stage2"
                      )

initArgsParser :: Parser InitArgs
initArgsParser =  InitArgs
              <$> (fromMaybe Stage0 `fmap` optional (stage1 <|> stage2))

actAsInit :: IO ()
actAsInit = do
    args <- execParser opts
    case initStage args of
        Stage0 -> initStage0
        Stage1 -> initStage1
        Stage2 -> initStage2
  where
    opts = info (initArgsParser <**> helper) (fullDesc)

respawn :: [String] -> Maybe [(String, String)] -> IO a
respawn args env = do
    selfExe <- readSymbolicLink "/proc/self/exe"
    executeFile selfExe False args env

initStage0 = do
    -- Set up /proc
    createDirectory "/proc" 0o755
    mount' "proc" "/proc" "proc" [MS_NOSUID, MS_NOEXEC, MS_RELATIME]
    -- Ensure we can use Unicode on a UTF-8-enabled console
    respawn ["--stage1"] $ Just [
          ("LC_CTYPE", "C.UTF-8")
        ]

initStage1 = do
    umount "/proc"

    let sysroot = "/sysroot"
        a </> b = if head b == '/' then a ++ b else a ++ "/" ++ b

    createDirectory sysroot 0o755
    mount' "sysroot" sysroot "tmpfs" [MS_RELATIME]

    createDirectory (sysroot </> "/usr") 0o755
    createDirectory (sysroot </> "/usr/sbin") 0o755
    createSymbolicLink "/usr/sbin" (sysroot </> "/sbin")
    createDirectory (sysroot </> "/usr/lib64") 0o755
    createSymbolicLink "/usr/lib64" (sysroot </> "/lib64")
    createDirectory (sysroot </> "/usr/lib") 0o755
    createSymbolicLink "/usr/lib" (sysroot </> "/lib")
    createDirectory (sysroot </> "/usr/lib/locale") 0o755
    createDirectory (sysroot </> "/usr/lib/locale/C.utf8") 0o755

    mapM_ (\f -> print f >> copyFile f (sysroot </> f)) [
          "/usr/sbin/init"
        , "/usr/lib64/ld-linux-x86-64.so.2"
        , "/usr/lib64/libc.so.6"
        , "/usr/lib64/libffi.so.8"
        , "/usr/lib64/libgmp.so.10"
        , "/usr/lib64/libm.so.6"
        , "/usr/lib/locale/C.utf8/LC_CTYPE"
        ]



    chdir "/sysroot"
    mount' "." "/" "" [MS_MOVE]
    chroot "."
    chdir "/"

    createDirectory "/proc" 0o755
    mount' "proc" "/proc" "proc" [MS_NOSUID, MS_NOEXEC, MS_RELATIME]
    createDirectory "/dev" 0o755
    mount' "devtmpfs" "/dev" "devtmpfs" [MS_NOSUID]
    createSymbolicLink "/proc/self/fd" "/dev/fd"
    createDirectory "/sys" 0o755
    mount' "sysfs" "/sys" "sysfs" [MS_NOSUID, MS_NODEV, MS_NOEXEC, MS_RELATIME]
    createDirectory "/run" 0o755
    withCString "mode=755" $ \data_ ->
        mount "tmpfs" "/run" "tmpfs" [MS_NOSUID, MS_NODEV] (Just data_)
    createDirectory "/var" 0o755
    createSymbolicLink "/run" "/var/run"

    putStrLn "Here's a ❤️"

    readFile "/proc/mounts" >>= putStrLn
    listDirectory "/dev" >>= print
    listDirectory "/usr/sbin" >>= print

    respawn ["--stage2"] Nothing

data TestResult = TestResult Int
    deriving (Show, Eq)

initStage2 = do
    let testResult = TestResult 0
    withFile "/dev/vport0p1" WriteMode $ \fd -> do
        hPrint fd testResult
        hFlush fd
    reboot Autoboot

actAsNonInit :: IO ()
actAsNonInit = fail "Not implemented 🤷"

main' :: IO ()
main' = usingLoggerT loggerAction $ do
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
