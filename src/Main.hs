module Main where

import qualified Data.ConfigFile as CP
import           Data.Either.Utils (forceEither) -- provided by MissingH
import           Data.Monoid ((<>))
import           System.Exit (ExitCode (ExitSuccess, ExitFailure), exitWith)

data SnapshotLimitConfig = SnapshotLimitConfig
    { yearly       :: !Int
    , monthly      :: !Int
    , weekly       :: !Int
    , daily        :: !Int
    , hourly       :: !Int
    , quaterHourly :: !Int
    } deriving (Show)


printAndExit :: String -> IO ()
printAndExit errMsg = do
    putStrLn errMsg
    exitWith $ ExitFailure 1

-- TODO get rid of forceEither here
getConfigParser :: String -> IO CP.ConfigParser
getConfigParser filepath = forceEither <$> CP.readfile CP.emptyCP filepath

-- TODO get rid of forceEither here as well
getTimelineConfig :: CP.ConfigParser -> Either CP.CPError SnapshotLimitConfig
getTimelineConfig cp = do
    keep_yearly        <- CP.get cp "TIMELINE" "keep_yearly"
    keep_monthly       <- CP.get cp "TIMELINE" "keep_monthly"
    keep_weekly        <- CP.get cp "TIMELINE" "keep_weekly"
    keep_daily         <- CP.get cp "TIMELINE" "keep_daily"
    keep_hourly        <- CP.get cp "TIMELINE" "keep_hourly"
    keep_quater_hourly <- CP.get cp "TIMELINE" "keep_quater_hourly"
    return $  SnapshotLimitConfig keep_yearly keep_monthly keep_weekly
                                  keep_daily keep_hourly keep_quater_hourly

main :: IO ()
main = do
    cp <- getConfigParser "apfs-auto-snapshot.cfg"
    let snapLimitConfig = forceEither $ getTimelineConfig cp
    putStrLn $ show snapLimitConfig

{-
    -- Parse command line options
    snapshots <- listSnapshots
    case snapshots of
        Right s -> putStrLn $ show s
        Left s  -> printAndExit $ "Error: " <> s
    snapshot <- createSnapshot
    case snapshot of
        Right s -> putStrLn $ "Created new snapshot: " <> show s
        Left s  -> printAndExit $ "Error: " <> s
-}

