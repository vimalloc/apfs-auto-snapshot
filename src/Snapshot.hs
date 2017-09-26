{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Snapshot where

import Control.Monad (when)
import Database.SQLite.Simple
import Data.List.Split (splitOn)
import Data.Monoid ((<>))
import System.Exit (ExitCode (ExitSuccess, ExitFailure))
import System.Process (readProcessWithExitCode)
import Text.Printf (printf)
import Text.Regex.PCRE.Heavy (scan, re)


-- TODO make sure this is properly ordered by date
data SnapshotDate = SnapshotDate
    { year   :: !Int
    , month  :: !Int
    , day    :: !Int
    , hour   :: !Int
    , minute :: !Int
    , second :: !Int
    } deriving (Eq, Ord)

instance Show SnapshotDate where
    show d = printf "%04d-%02d-%02d-%02d%02d%02d" (year d) (month d) (day d)
                                                  (hour d) (minute d) (second d)

-- What categories the current snapshot will fall under
data SnapshotType = SnapshotType
    { yearly        :: Bool
    , monthly       :: Bool
    , weekly        :: Bool
    , daily         :: Bool
    , hourly        :: Bool
    , quater_hourly :: Bool
    } deriving (Show)


parseSnapshotDates :: [String] -> Either String [SnapshotDate]
parseSnapshotDates dates = sequence $ parseDate <$> dates

parseDate :: String -> Either String SnapshotDate
parseDate s = case scan dateRegex s of
                  (_, y:mo:d:h:mi:s:[]):_ -> Right $ SnapshotDate (read y)
                                                                  (read mo)
                                                                  (read d)
                                                                  (read h)
                                                                  (read mi)
                                                                  (read s)
                  _ -> Left $ s <> " is not in the format YYYY-MM-DD-HHMMSS."
  where
    dateRegex = [re|(\d{4})-(\d{2})-(\d{2})-(\d{2})(\d{2})(\d{2})|]

-- TODO have basically teh same code for checking exit status. Can we generalize it?
listSnapshots :: IO (Either String [SnapshotDate])
listSnapshots = do
    let tmutil = "/usr/bin/tmutil"
    let args   = ["listlocalsnapshotdates"]
    let stdin  = ""
    (code, stdout, stderr) <- readProcessWithExitCode tmutil args stdin
    case code of
        ExitFailure _ -> return $ Left ("tmutil exited with code " <> show code
                                        <> ": " <> stderr)
        ExitSuccess   -> do
            -- Strip header and trailing newline
            let dates = tail . init $ splitOn "\n" stdout
            return $ parseSnapshotDates dates

createSnapshot :: IO (Either String SnapshotDate)
createSnapshot = do
    let tmutil = "/usr/bin/tmutil"
    let args   = ["localsnapshot"]
    let stdin  = ""
    (code, stdout, stderr) <- readProcessWithExitCode tmutil args stdin
    case code of
        ExitFailure _ -> return $ Left ("tmutil exited with code " <> show code
                                        <> ": " <> stderr)
        ExitSuccess   -> return $ parseDate stdout


data IdField = IdField
    { primaryKey :: Int
    } deriving (Show)

instance FromRow IdField where
  fromRow = IdField <$> field


-- TODO switch to opaleye or something that gives us better type support
storeSnapshot :: SnapshotDate -> SnapshotType -> IO ()
storeSnapshot s flags = do
    -- Open db with foreign key support
    conn <- open "apfs-auto-snapshot.db"
    execute_ conn "PRAGMA foreign_keys = ON"

    -- Insert the snapshot and get the id of newly inserted row
    execute conn "INSERT INTO snapshots (name) VALUES (?)"
                 (Only (show s :: String))
    res <- query_ conn "SELECT last_insert_rowid()" :: IO [IdField]
    let snapshot_id = primaryKey $ head res

    -- Insert the types of snapshots this is for in the timelines. Can be
    -- multiple timelines at the same time.
    when (yearly flags)
        (execute conn
        "INSERT INTO timelines (snapshot_id, snapshot_type) VALUES (?, 'yearly')"
        (Only (show snapshot_id :: String)))
    when (monthly flags)
        (execute conn
        "INSERT INTO timelines (snapshot_id, snapshot_type) VALUES (?, 'monthly')"
        (Only (show snapshot_id :: String)))
    when (weekly flags)
        (execute conn
        "INSERT INTO timelines (snapshot_id, snapshot_type) VALUES (?, 'weekly')"
        (Only (show snapshot_id :: String)))
    when (daily flags)
        (execute conn
        "INSERT INTO timelines (snapshot_id, snapshot_type) VALUES (?, 'daily')"
        (Only (show snapshot_id :: String)))
    when (hourly flags)
        (execute conn
        "INSERT INTO timelines (snapshot_id, snapshot_type) VALUES (?, 'hourly')"
        (Only (show snapshot_id :: String)))
    when (quater_hourly flags)
        (execute conn
        "INSERT INTO timelines (snapshot_id, snapshot_type) VALUES (?, 'quater-hourly')"
        (Only (show snapshot_id :: String)))

    -- Close the connection
    close conn
