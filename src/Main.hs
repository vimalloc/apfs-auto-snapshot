module Main where

import           Data.Either.Utils (forceEither) -- provided by MissingH
import           Data.Semigroup ((<>))
import           Options.Applicative
import           System.Exit (ExitCode (ExitSuccess, ExitFailure), exitWith)

-- What categories the current snapshot will fall under
data CliFlags = CliFlags
    { yearly        :: Bool
    , monthly       :: Bool
    , weekly        :: Bool
    , daily         :: Bool
    , hourly        :: Bool
    , quater_hourly :: Bool
    } deriving (Show)


cliParser :: Parser CliFlags
cliParser = CliFlags
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
        ( long "quater_hourly"
       <> help "This snapshot is a quater_hourly snapshot")

greet :: CliFlags -> IO ()
greet (CliFlags False False False False False False) =
    printAndExit "Specify at least one snapshot type (see --help)"
greet _ = return ()

printAndExit :: String -> IO ()
printAndExit errMsg = do
    putStrLn $ "Error: " ++ errMsg
    exitWith $ ExitFailure 1

main :: IO ()
main = greet =<< execParser opts
  where
    opts = info (cliParser <**> helper)
      ( fullDesc
     <> progDesc "Create a new snapshot in the given timelines"
     <> header "apfs-auto-snapshot - Automaticaaly craete and delete APFS snapshots")

{-
main :: IO ()
main = do
    cp <- getConfigParser "apfs-auto-snapshot.cfg"
    let snapLimitConfig = forceEither $ getTimelineConfig cp
    putStrLn $ show snapLimitConfig

    -- Parse command line options
    snapshots <- listSnapshots
    case snapshots of
        Right s -> putStrLn $ show s
        Left s  -> printAndExit s
    snapshot <- createSnapshot
    case snapshot of
        Right s -> putStrLn $ "Created new snapshot: " ++ show s
        Left s  -> printAndExit s
-}
