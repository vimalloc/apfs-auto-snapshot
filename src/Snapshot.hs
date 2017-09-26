{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}

module Snapshot where

import Control.Monad (when)
import Data.List.Split (splitOn)
import Data.Monoid ((<>))
import Text.Regex.PCRE.Heavy (scan, re)
import System.Exit (ExitCode (ExitSuccess), exitWith)
import System.Process (readProcessWithExitCode)



-- TODO make sure this is properly ordered by date
data SnapshotDate = SnapshotDate
    { year   :: !Int
    , month  :: !Int
    , day    :: !Int
    , hour   :: !Int
    , minute :: !Int
    , second :: !Int
    } deriving (Eq, Show, Ord)

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


-- TODO should probably use either here
parseSnapshotDates :: String -> Maybe [SnapshotDate]
parseSnapshotDates dateStr = sequence $ parseDate <$> dates
  where
    -- Need to strip the header in stdout and the trailing newline
    dates = tail . init $ splitOn "\n" dateStr

    parseDate :: String -> Maybe SnapshotDate
    parseDate s = case scan dateRegex s of
                      (_, y:mo:d:h:mi:s:[]):_ -> Just $ SnapshotDate (read y)
                                                                     (read mo)
                                                                     (read d)
                                                                     (read h)
                                                                     (read mi)
                                                                     (read s)
                      _                       -> Nothing
      where
        dateRegex = [re|^(\d{4})-(\d{2})-(\d{2})-(\d{2})(\d{2})(\d{2})$|]

printAndExit :: String -> ExitCode -> IO ()
printAndExit errMsg exitCode = do
    putStrLn errMsg
    exitWith exitCode

-- TODO should propbably use either here too
listSnapshots :: IO (Maybe [SnapshotDate])
listSnapshots = do
    let tmutil = "/usr/bin/tmutil"
    let args   = ["listlocalsnapshotdates"]
    let stdin  = ""
    (code, stdout, stderr) <- readProcessWithExitCode tmutil args stdin
    when (code /= ExitSuccess)
      (printAndExit ("Error running tmutil: " <> stderr) code)
    return $ parseSnapshotDates stdout

-- TODO need to save current snapshot in persistant store somewhere, so we can
--      keep track of what snapshots this made (and will eventually delete as
--      the time window shifts), and which snapshots were user or system made
--      (and thus shouldn't be deleted by us). In that store, we need to
--      include what limits this snapshot applies to (a single snapshot could
--      be part of  hourly, daily, weekly, etc). If rotating this snapshot
--      should be rotated out of the hourly limits, but is still there for the
--      daily limits, we cannot remove it yet.
-- TODO also throw this in the Either monad, raise errors, etc
createSnapshot :: IO ()
createSnapshot = do
    let tmutil = "/usr/bin/tmutil"
    let args   = ["localsnapshot"]
    let stdin  = ""
    (code, stdout, stderr) <- readProcessWithExitCode tmutil args stdin
    when (code /= ExitSuccess)
      (printAndExit ("Error running tmutil: " <> stderr) code)
