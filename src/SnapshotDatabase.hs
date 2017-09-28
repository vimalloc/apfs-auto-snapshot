{-# LANGUAGE OverloadedStrings #-}

module SnapshotDatabase where

import TMSnapshot

import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.Internal (Field(..))  -- TODO not great...
import Database.SQLite.Simple.Ok
import Database.SQLite.Simple.ToField
import Data.Text (unpack)

-- TODO move from String to Text everywhere
-- TODO make all imports explicit

data SnapshotTimeline = Yearly
                      | Monthly
                      | Weekly
                      | Daily
                      | Hourly
                      | QuaterHourly

instance ToField SnapshotTimeline where
    toField Yearly       = toField ("yearly" :: String)
    toField Monthly      = toField ("monthly" :: String)
    toField Weekly       = toField ("weekly" :: String)
    toField Daily        = toField ("daily" :: String)
    toField Hourly       = toField ("hourly" :: String)
    toField QuaterHourly = toField ("quater_hourly" :: String)

data StoredSnapshot = StoredSnapshot
    { snapshotId :: Int
    , snapshotName :: String
    }

instance FromRow StoredSnapshot where
    fromRow = StoredSnapshot <$> field <*> field

data StoredTimeline = StoredTimeline
    { timelineId :: Int
    , snapshotId' :: Int
    , snapshotType :: String
    }

instance FromRow StoredTimeline where
    fromRow = StoredTimeline <$> field <*> field <*> field

data LastInsertedRowId = LastInsertedRowId
    { lastInsertedRowId :: Int
    }

instance FromRow LastInsertedRowId where
    fromRow = LastInsertedRowId <$> field


getStoredSnapshots :: Connection -> IO [StoredSnapshot]
getStoredSnapshots conn = query_ conn "SELECT * FROM snapshots"

storeSnapshot :: Connection -> TMSnapshot -> IO StoredSnapshot
storeSnapshot conn tmSnap = do
    execute conn "INSERT INTO snapshots (name) VALUES (?)" (Only (tmId tmSnap))
    lastRowIdRes <- query_ conn "SELECT last_insert_rowid()"
    let snapId = lastInsertedRowId $ head lastRowIdRes
    snapRes <- query conn "SELECT * FROM snapshots WHERE id = (?)" (Only snapId)
    return $ head snapRes

storeTimelines :: Connection -> StoredSnapshot -> [SnapshotTimeline] -> IO ()
storeTimelines conn snap timelines = mapM_ (storeTimeline conn snap) timelines
  where
    storeTimeline :: Connection -> StoredSnapshot -> SnapshotTimeline -> IO ()
    storeTimeline conn snap timeline = execute conn query args
      where
        query = "INSERT INTO timelines (snapshot_id, snapshot_type) VALUES (?, ?)"
        args  = (snapshotId snap, timeline)

deleteStoredSnapshot :: Connection -> StoredSnapshot -> IO ()
deleteStoredSnapshot conn snap = execute conn query args
  where
      query = "DELETE FROM snapshots WHERE id = (?)"
      args = (Only (snapshotId snap))

deleteTimelines :: Connection -> SnapshotTimeline -> Int -> IO ()
deleteTimelines conn timeline numToKeep = do
    let timelineQuery = "SELECT * FROM timelines WHERE snapshot_type = (?)\
                        \ ORDER BY snapshot_id DESC"
    timelines <- query conn timelineQuery (Only timeline)
    let toRemove = drop numToKeep timelines
    mapM_ (deleteTimeline conn) toRemove
  where
    deleteTimeline :: Connection -> StoredTimeline -> IO ()
    deleteTimeline conn tl = execute conn query args
      where
        query = "DELETE FROM timelines WHERE id = (?)"
        args = (Only (timelineId tl))

snapshotsWithoutTimelines :: Connection -> IO [StoredSnapshot]
snapshotsWithoutTimelines conn = query_ conn query
  where
      query = "SELECT snapshots.* FROM snapshots LEFT JOIN timelines \
              \ON snapshots.id = timelines.snapshot_id \
              \WHERE timelines.snapshot_id IS NULL"
