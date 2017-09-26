module Main where

import Data.Monoid ((<>))
import System.Exit (ExitCode (ExitSuccess, ExitFailure), exitWith)

import Snapshot

printAndExit :: String -> IO ()
printAndExit errMsg = do
    putStrLn errMsg
    exitWith $ ExitFailure 1

main :: IO ()
main = do
    snapshots <- listSnapshots
    case snapshots of
        Right s -> putStrLn $ show s
        Left s  -> printAndExit $ "Error: " <> s
{-
main = do
    snapshot <- createSnapshot
    case snapshot of
        Right s -> putStrLn $ "Created new snapshot: " <> show s
        Left s  -> printAndExit $ "Error: " <> s
-}
