{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}

module TMSnapshot where

import Control.Monad.Except (MonadError, MonadIO, throwError, liftIO)
import Data.List.Split (splitOn)
import System.Exit (ExitCode (ExitSuccess, ExitFailure))
import System.Process (readProcessWithExitCode)
import Text.Regex.PCRE.Heavy (scan, re)

-- Unique id (date) of snapshot as it is stored in time machine. As it
-- currnelty stands, this can be derived from the data, but seperating
-- them here helps make things more clear, and also easier for us if
-- apple changes the id for these in the future (or allows us to
-- label them ourselves)
type TMSnapshotId = String

-- Datetime that the time machine snapshot was created
data TMSnapshotDate = TMSnapshotDate
    { year   :: !Int
    , month  :: !Int
    , day    :: !Int
    , hour   :: !Int
    , minute :: !Int
    , second :: !Int
    } deriving (Eq, Ord)

-- Snapshot data we will be working with in our functions
data TMSnapshot = TMSnapshot
    { date :: TMSnapshotDate
    , tmId :: TMSnapshotId
    }


parseTMSnapshotDates :: (MonadError String m) => [String] -> m [TMSnapshot]
parseTMSnapshotDates dates = sequence $ parseTMDate <$> dates

parseTMDate :: (MonadError String m) => String -> m TMSnapshot
parseTMDate dateStr =
    case scan dateRegex dateStr of
        (tmid, y:mo:d:h:mi:s:[]):[] -> do
            let tmDate     = TMSnapshotDate (read y) (read mo) (read d)
                                            (read h) (read mi) (read s)
            let tmUniqueId = tmid
            return $ TMSnapshot tmDate tmUniqueId
        _                           -> throwError badFormat
  where
    dateRegex = [re|(\d{4})-(\d{2})-(\d{2})-(\d{2})(\d{2})(\d{2})|]
    badFormat = concat [ "tmutil output "
                       , dateStr
                       , " is not in the format YYYY-MM-DD-HHMMSS." ]

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

listTMSnapshots :: (MonadError String m, MonadIO m) => m [TMSnapshot]
listTMSnapshots = do
    -- Expected output should have a header, list of dates, and trailing
    -- newline. Strip out the header and trailing newline, and work on
    -- just the dates.
    stdout <- runSubprocess "/usr/bin/tmutil"  ["listlocalsnapshotdates"] ""
    let split = splitOn "\n" stdout
    case split of
      _:_:_ -> parseTMSnapshotDates . tail . init $ split
      _     -> throwError $ concat [ "Unexpected output from tmutil: "
                                   , stdout
                                   , ". Expected header, dates, and newline" ]

createTMSnapshot :: (MonadError String m, MonadIO m) => m TMSnapshot
createTMSnapshot = runSubprocess "/usr/bin/tmutil" ["localsnapshot"] "" >>= parseTMDate

deleteTMSnapshot :: (MonadError String m, MonadIO m) => TMSnapshot -> m ()
deleteTMSnapshot s = runSubprocess "/usr/bin/tmutil" args "" >>= \_ -> return ()
  where
    args = ["deletelocalsnapshots", tmId s]
