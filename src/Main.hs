{-# LANGUAGE FlexibleContexts #-}
module Main where

import Snapshot

import Control.Monad.Except
import Data.Either.Utils (forceEither) -- provided by MissingH
import Data.Semigroup ((<>))
import Options.Applicative
import System.Exit (ExitCode (ExitSuccess, ExitFailure), exitWith)

data RequestedSnapshots = RequestedSnapshots
    { yearly        :: Bool
    , monthly       :: Bool
    , weekly        :: Bool
    , daily         :: Bool
    , hourly        :: Bool
    , quater_hourly :: Bool
    } deriving (Show)


reqToTypes :: RequestedSnapshots -> [SnapshotType]
reqToTypes req
    | yearly req        = Yearly : (reqToTypes req { yearly = False })
    | monthly req       = Monthly : (reqToTypes req { monthly = False })
    | weekly req        = Weekly : (reqToTypes req { weekly = False })
    | daily req         = Daily : (reqToTypes req { daily = False })
    | hourly req        = Hourly : (reqToTypes req { hourly = False })
    | quater_hourly req = QuaterHourly : (reqToTypes req { quater_hourly = False })
    | otherwise         = []

-- TODO use submcommands, so you can have other commands for explictly
--      pruning, listing, purging, etc.
cliParser :: Parser RequestedSnapshots
cliParser = RequestedSnapshots
    <$> switch
        ( long "yearly"
       <> help "This snapshot is a yearly snapshot")
    <*> switch
        ( long "monthly"
       <> help "This snapshot is a monthly snapshot")
    <*> switch
        ( long "weekly"
       <> help "This snapshot is a weekly snapshot")
    <*> switch
        ( long "daily"
       <> help "This snapshot is a daily snapshot")
    <*> switch
        ( long "hourly"
       <> help "This snapshot is a hourly snapshot")
    <*> switch
        ( long "quater-hourly"
       <> help "This snapshot is a quater_hourly snapshot")

handleCli :: (MonadError String m, MonadIO m) => [SnapshotType] -> m ()
handleCli []    = throwError "Specify at least one snapshot type (see --help)"
handleCli types = do
    snapshot <- createSnapshot
    liftIO $ storeSnapshot snapshot types
    -- TODO remove any snapshots from the database that don't exist
    --      in time machine any more (deleted outside of this program)
    -- TODO remove any snapshots from the database AND time machine that are
    --      now outside of the floating window limit

printAndExit :: String -> IO ()
printAndExit errMsg = do
    putStrLn $ "Error: " ++ errMsg
    exitWith $ ExitFailure 1

main :: IO ()
main = do
    let opts = info (cliParser <**> helper)
          ( fullDesc
         <> progDesc "Create a new snapshot in the given timelines"
         <> header "apfs-auto-snapshot - Automaticaaly craete and delete APFS snapshots")
    req <- execParser opts
    let snapshotTypes = reqToTypes req
    result <- runExceptT $ handleCli snapshotTypes
    case result of
        Right _  -> return ()
        Left err -> printAndExit err
