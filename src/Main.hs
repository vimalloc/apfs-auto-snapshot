module Main where

import Snapshot

import Control.Monad (when)
import Data.Monoid ((<>))
import System.Exit
import System.Process

printAndExit :: String -> ExitCode -> IO ()
printAndExit s c = do
    putStrLn s
    exitWith c


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
