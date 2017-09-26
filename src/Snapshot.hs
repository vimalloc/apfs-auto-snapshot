{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}

module Snapshot where

import Control.Monad (when)
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

data SnapshotLimit = Yearly
                   | Monthly
                   | Weekly
                   | Daily
                   | Hourly
                   | QuaterHourly

data Snapshot = Snapshot
    { date  :: SnapshotDate
    , limit :: SnapshotLimit
    }


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


-- TODO need to save current snapshot in persistant store somewhere, so we can
--      keep track of what snapshots this made (and will eventually delete as
--      the time window shifts), and which snapshots were user or system made
--      (and thus shouldn't be deleted by us). In that store, we need to
--      include what limits this snapshot applies to (a single snapshot could
--      be part of  hourly, daily, weekly, etc). If rotating this snapshot
--      should be rotated out of the hourly limits, but is still there for the
--      daily limits, we cannot remove it yet.
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
