module Main where

import Control.Monad (when)
import System.Linux.Reboot (RebootCommand(..), reboot)
import System.Posix.Process (getProcessID)

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    pid <- getProcessID
    when (pid == 1) $ do
        putStrLn "Running as PID 1, rebooting system"
        reboot Autoboot
