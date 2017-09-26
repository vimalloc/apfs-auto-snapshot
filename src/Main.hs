module Main where

import Snapshot

import Control.Monad (when)
import Data.Monoid ((<>))
import System.Exit (ExitCode (ExitSuccess), exitWith)
import System.Process (readProcessWithExitCode)

printAndExit :: String -> ExitCode -> IO ()
printAndExit errMsg exitCode = do
    putStrLn errMsg
    exitWith exitCode

main :: IO ()
main = do
    let tmutil = "/usr/bin/tmutil"
    let args   = ["listlocalsnapshotdates"]
    let stdin  = ""
    (code, stdout, stderr) <- readProcessWithExitCode tmutil args stdin
    when (code /= ExitSuccess)
      (printAndExit ("Error running tmutil: " <> stderr) code)
    let snapshotDates = parseSnapshotDates stdout
    putStrLn $ show snapshotDates
