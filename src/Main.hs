{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import SnapshotDatabase
import TMSnapshot

import Control.Monad.Except
import Database.SQLite.Simple
import Data.Either.Utils (forceEither) -- provided by MissingH
import Data.Semigroup ((<>))
import Options.Applicative
import System.Exit (ExitCode (ExitSuccess, ExitFailure), exitWith)

data RequestedSnapshots = RequestedSnapshots
    { yearly        :: Bool
    , monthly       :: Bool
    , weekly        :: Bool
    , daily         :: Bool
    , hourly        :: Bool
    , quater_hourly :: Bool
    } deriving (Show)


reqToTypes :: RequestedSnapshots -> [SnapshotTimeline]
reqToTypes req
    | yearly req        = Yearly : (reqToTypes req { yearly = False })
    | monthly req       = Monthly : (reqToTypes req { monthly = False })
    | weekly req        = Weekly : (reqToTypes req { weekly = False })
    | daily req         = Daily : (reqToTypes req { daily = False })
    | hourly req        = Hourly : (reqToTypes req { hourly = False })
    | quater_hourly req = QuaterHourly : (reqToTypes req { quater_hourly = False })
    | otherwise         = []

-- TODO use submcommands, so you can have other commands for explictly
--      pruning, listing, purging, etc.
cliParser :: Parser RequestedSnapshots
cliParser = RequestedSnapshots
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
        ( long "quater-hourly"
       <> help "This snapshot is a quater_hourly snapshot")

handleCli :: (MonadError String m, MonadIO m) => [SnapshotTimeline] -> m ()
handleCli []    = throwError "Specify at least one snapshot type (see --help)"
handleCli timelines = do
    -- Open the database connection with foreign key support
    conn <- liftIO $ open "apfs-auto-snapshot.db"
    liftIO $ execute_ conn "PRAGMA foreign_keys = ON"

    -- Remove any snapshots from the database that don't exist in time machine
    -- any more (deleted outside of this program).
    -- TODO should maybe add logging if this finds a missing snapshot
    expectedSnaps <- liftIO $ getStoredSnapshots conn
    tmSnaps       <- listTMSnapshots
    let tmSnapIds = map tmId tmSnaps
    let missingSnaps = filter (\s -> (snapshotName s) `notElem` tmSnapIds) expectedSnaps
    liftIO $ mapM_ (deleteStoredSnapshot conn) missingSnaps

    -- Create our new snapshot
    tmSnap     <- createTMSnapshot
    storedSnap <- liftIO $ storeSnapshot conn tmSnap
    liftIO $ storeTimelines conn storedSnap timelines

    -- TODO remove any snapshots from the database AND time machine that are
    --      now outside of the floating window limit

printAndExit :: String -> IO ()
printAndExit errMsg = do
    putStrLn $ "Error: " ++ errMsg
    exitWith $ ExitFailure 1

main :: IO ()
main = do
    let opts = info (cliParser <**> helper)
          ( fullDesc
         <> progDesc "Create a new snapshot in the given timelines"
         <> header "apfs-auto-snapshot - Automaticaaly craete and delete APFS snapshots")
    req <- execParser opts
    let snapshotTypes = reqToTypes req
    result <- runExceptT $ handleCli snapshotTypes
    case result of
        Right _  -> return ()
        Left err -> printAndExit err
