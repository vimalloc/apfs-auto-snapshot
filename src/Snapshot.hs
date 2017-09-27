{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Snapshot where

import Control.Monad (when)
import Control.Monad.Except
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Data.List.Split (splitOn)
import Data.Monoid ((<>))
import System.Exit (ExitCode (ExitSuccess, ExitFailure))
import System.Process (readProcessWithExitCode)
import Text.Printf (printf)
import Text.Regex.PCRE.Heavy (scan, re)


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

data SnapshotType = Yearly
                  | Monthly
                  | Weekly
                  | Daily
                  | Hourly
                  | QuaterHourly

instance ToField SnapshotType where
    toField Yearly       = toField ("yearly" :: String)
    toField Monthly      = toField ("monthly" :: String)
    toField Weekly       = toField ("weekly" :: String)
    toField Daily        = toField ("daily" :: String)
    toField Hourly       = toField ("hourly" :: String)
    toField QuaterHourly = toField ("quater_hourly" :: String)


parseSnapshotDates :: (MonadError String m) => [String] -> m [SnapshotDate]
parseSnapshotDates dates = sequence $ parseDate <$> dates

parseDate :: (MonadError String m) => String -> m SnapshotDate
parseDate s = case scan dateRegex s of
                  (_, y:mo:d:h:mi:s:[]):_ -> return $ SnapshotDate (read y)
                                                                   (read mo)
                                                                   (read d)
                                                                   (read h)
                                                                   (read mi)
                                                                   (read s)
                  _ -> throwError $ s <> " is not in the format YYYY-MM-DD-HHMMSS."
  where
    dateRegex = [re|(\d{4})-(\d{2})-(\d{2})-(\d{2})(\d{2})(\d{2})|]

runSubprocess :: (MonadError String m, MonadIO m) => String -> [String] -> String -> m String
runSubprocess binary args stdin = do
    (code, stdout, stderr) <- liftIO $ readProcessWithExitCode binary args stdin
    case code of
        ExitSuccess   -> return stdout
        ExitFailure _ -> throwError $ concat [ binary
                                             , " exited with code "
                                             , show code
                                             , ": "
                                             , stderr ]

listSnapshots :: (MonadError String m, MonadIO m) => m [SnapshotDate]
listSnapshots = do
    -- Expected output should have a header, list of dates, and trailing
    -- newline. Strip out the header and trailing newline, and work on
    -- just the dates.
    stdout <- runSubprocess "/usr/bin/tmutil"  ["listlocalsnapshotdates"] ""
    let split = splitOn "\n" stdout
    case split of
      _:_:_ -> parseSnapshotDates . tail . init $ split
      _     -> throwError $ "Unexpected output from tmutil: " <> stdout

createSnapshot :: (MonadError String m, MonadIO m) => m SnapshotDate
createSnapshot = runSubprocess "/usr/bin/tmutil" ["localsnapshot"] "" >>= parseDate

data IdField = IdField
    { primaryKey :: Int
    } deriving (Show)

instance FromRow IdField where
  fromRow = IdField <$> field


-- TODO switch to opaleye or something that gives us better type support
--storeSnapshot :: (MonadError String m, MonadIO m) -> SnapshotDate -> SnapshotType -> m ()
storeSnapshot :: SnapshotDate -> [SnapshotType] -> IO ()
storeSnapshot s types = do
    -- Open db with foreign key support
    conn <- open "apfs-auto-snapshot.db"
    execute_ conn "PRAGMA foreign_keys = ON"

    -- Insert the snapshot and get the id of newly inserted row
    execute conn "INSERT INTO snapshots (name) VALUES (?)"
                 (Only (show s :: String))
    res <- query_ conn "SELECT last_insert_rowid()" :: IO [IdField]
    let snapshotId = primaryKey $ head res

    -- Insert what type of snapshot this is
    mapM_ (storeSnapshotType conn snapshotId) types

    -- Close the connection
    close conn
  where
    storeSnapshotType :: Connection -> Int -> SnapshotType -> IO ()
    storeSnapshotType conn snapshotId snapshotType = execute conn query args
      where
        query = "INSERT INTO timelines (snapshot_id, snapshot_type) VALUES (?, ?)"
        args  = (snapshotId, snapshotType)
