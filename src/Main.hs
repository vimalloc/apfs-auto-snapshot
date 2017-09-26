module Main where

import Snapshot

 -- TODO we need to be an either instead of a maybe here
main :: IO ()
main = do
    snapshots <- listSnapshots
    case snapshots of
        Just s  -> putStrLn $ show s
        Nothing -> putStrLn "Error parsing snapshot dates"
--main = createSnapshot
