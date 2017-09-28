module ConfigParse where

import qualified Data.ConfigFile as CP
import           Data.Either.Utils (forceEither) -- provided by MissingH

data SnapshotLimitConfig = SnapshotLimitConfig
    { yearlyLimit       :: !Int
    , monthlyLimit      :: !Int
    , weeklyLimit       :: !Int
    , dailyLimit        :: !Int
    , hourlyLimit       :: !Int
    , quaterHourlyLimit :: !Int
    } deriving (Show)

-- TODO get rid of forceEither here
getConfigParser :: String -> IO CP.ConfigParser
getConfigParser filepath = forceEither <$> CP.readfile CP.emptyCP filepath

getTimelineConfig :: CP.ConfigParser -> Either CP.CPError SnapshotLimitConfig
getTimelineConfig cp = do
    keep_yearly        <- CP.get cp "TIMELINE" "keep_yearly"
    keep_monthly       <- CP.get cp "TIMELINE" "keep_monthly"
    keep_weekly        <- CP.get cp "TIMELINE" "keep_weekly"
    keep_daily         <- CP.get cp "TIMELINE" "keep_daily"
    keep_hourly        <- CP.get cp "TIMELINE" "keep_hourly"
    keep_quater_hourly <- CP.get cp "TIMELINE" "keep_quater_hourly"
    return $ SnapshotLimitConfig keep_yearly keep_monthly keep_weekly
                                 keep_daily keep_hourly keep_quater_hourly

