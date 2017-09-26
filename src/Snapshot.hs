{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}

module Snapshot where

import Data.List.Split (splitOn)
import Text.Regex.PCRE.Heavy (scan, re)


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
                      _ -> Nothing
      where
        dateRegex = [re|^(\d{4})-(\d{2})-(\d{2})-(\d{2})(\d{2})(\d{2})$|]
