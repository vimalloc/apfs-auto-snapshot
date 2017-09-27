module Main where

import Snapshot

import Control.Monad.Except
import Data.Either.Utils (forceEither) -- provided by MissingH
import Data.Semigroup ((<>))
import Options.Applicative
import System.Exit (ExitCode (ExitSuccess, ExitFailure), exitWith)


-- TODO use submcommands, so you can have other commands for explictly
--      pruning, listing, purging, etc.
cliParser :: Parser SnapshotType
cliParser = SnapshotType
    <$> switch
        ( long "yearly"
       <> help "This snapshot is a yearly snapshot")
    <*> switch
        ( long "monthly"
       <> help "This snapshot is a monthly snapshot")
    <*> switch
        ( long "weekly"
       <> help "This snapshot is a weekly snapshot")
    <*> switch
        ( long "daily"
       <> help "This snapshot is a daily snapshot")
    <*> switch
        ( long "hourly"
       <> help "This snapshot is a hourly snapshot")
    <*> switch
        ( long "quater_hourly"
       <> help "This snapshot is a quater_hourly snapshot")

-- TODO make this an ExceptT monad as well, and make main do runExceptT?
handleCli :: SnapshotType -> IO ()
handleCli (SnapshotType False False False False False False) =
    printAndExit "Specify at least one snapshot type (see --help)"
handleCli types = do
    snapshot <- runExceptT createSnapshot
    case snapshot of
        Left er -> printAndExit er
        Right s -> storeSnapshot s types
    -- TODO remove any snapshots from the database that don't exist
    --      in time machine any more (deleted outside of this program)
    -- TODO remove any snapshots from the database AND time machine that are
    --      now outside of the floating window limit


printAndExit :: String -> IO ()
printAndExit errMsg = do
    putStrLn $ "Error: " ++ errMsg
    exitWith $ ExitFailure 1

main :: IO ()
main = handleCli =<< execParser opts
  where
    opts = info (cliParser <**> helper)
      ( fullDesc
     <> progDesc "Create a new snapshot in the given timelines"
     <> header "apfs-auto-snapshot - Automaticaaly craete and delete APFS snapshots")
