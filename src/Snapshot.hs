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

-- What categories the current snapshot will fall under
-- TODO make this a better name
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

storeSnapshot :: SnapshotDate -> SnapshotType -> IO ()
storeSnapshot s flags = do
    -- TODO Initially store the snapshot in the database
    -- TODO Actually associate this snapshot with the given flags
    when (yearly flags)
        (putStrLn "Yearly")
    when (monthly flags)
        (putStrLn "Monthly")
    when (weekly flags)
        (putStrLn "Weekly")
    when (daily flags)
        (putStrLn "Daily")
    when (hourly flags)
        (putStrLn "Hourly")
    when (quater_hourly flags)
        (putStrLn "Quater Hourly")
