{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Snapshot where

import Control.Monad (when)
import Control.Monad.Except
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.Internal (Field(..))  -- TODO not great...
import Database.SQLite.Simple.Ok
import Database.SQLite.Simple.ToField
import Data.List.Split (splitOn)
import Data.Text (unpack)
import Data.Monoid ((<>))
import System.Exit (ExitCode (ExitSuccess, ExitFailure))
import System.Process (readProcessWithExitCode)
import Text.Printf (printf)
import Text.Regex.PCRE.Heavy (scan, re)

-- TODO move from String to Text everywhere
-- TODO make all imports explicit
-- TODO pass sqlite connection in everywhere that needs it instead of opening
--      a new connection in each function


-- TODO change to SnapshotName
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

instance ToField SnapshotDate where
    toField = toField . show

instance FromField SnapshotDate where
    fromField f@(Field (SQLText txt) _) =
        case (parseDate $ unpack txt) of
            Right name -> Ok name
            Left err   -> returnError ConversionFailed f err
    fromField f = returnError ConversionFailed f "need a text"

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

data IdField = IdField
    { primaryKey :: Int
    }

instance FromRow IdField where
    fromRow = IdField <$> field

data SnapshotField = SnapshotField
    { snapshotId :: Int
    , snapshotName :: SnapshotDate
    }

instance FromRow SnapshotField where
    fromRow = SnapshotField <$> field <*> field


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

getStoredSnapshots :: IO [SnapshotDate]
getStoredSnapshots = do
    conn <- open "apfs-auto-snapshot.db"
    execute_ conn "PRAGMA foreign_keys = ON"
    res <- query_ conn "SELECT * FROM snapshots" :: IO [SnapshotField]
    close conn
    return $ map (snapshotName) res

deleteStoredSnapshot :: SnapshotDate -> IO ()
deleteStoredSnapshot s = do
    conn <- open "apfs-auto-snapshot.db"
    execute_ conn "PRAGMA foreign_keys = ON"
    execute conn "DELETE FROM snapshots WHERE name = (?)" (Only s)
    close conn

storeSnapshot :: SnapshotDate -> [SnapshotType] -> IO ()
storeSnapshot s types = do
    conn <- open "apfs-auto-snapshot.db"
    execute_ conn "PRAGMA foreign_keys = ON"
    execute conn "INSERT INTO snapshots (name) VALUES (?)" (Only s)
    res <- query_ conn "SELECT last_insert_rowid()"
    let snapshotId = primaryKey $ head res
    mapM_ (storeSnapshotType conn snapshotId) types
    close conn
  where
    storeSnapshotType :: Connection -> Int -> SnapshotType -> IO ()
    storeSnapshotType conn snapshotId snapshotType = execute conn query args
      where
        query = "INSERT INTO timelines (snapshot_id, snapshot_type) VALUES (?, ?)"
        args  = (snapshotId, snapshotType)
