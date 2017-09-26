{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}

module Snapshot where

import Text.Regex.PCRE.Heavy (scan, re)
import Text.Read (readMaybe)


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
parseSnapshotDates :: [String] -> Maybe [SnapshotDate]
parseSnapshotDates dates = sequence $ parseDate <$> dates
  where
    parseDate :: String -> Maybe SnapshotDate
    parseDate s = case scan dateRegex s of
                      (_, y:mo:d:h:mi:s:[]):_ -> SnapshotDate <$> readMaybe y
                                                              <*> readMaybe mo
                                                              <*> readMaybe d
                                                              <*> readMaybe h
                                                              <*> readMaybe mi
                                                              <*> readMaybe s
                      _ -> Nothing
      where
        dateRegex = [re|^(\d{4})-(\d{2})-(\d{2})-(\d{2})(\d{2})(\d{2})$|]
