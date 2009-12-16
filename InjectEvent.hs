module Main where

import IO
import qualified System.Environment as SysEnv
import qualified System.Directory as SysDir
import qualified System.Exit as Exit
import qualified System.Locale as Locale
import qualified SpimCommon as Spim
import qualified MIMEDir as MD
import qualified Data.Map as Map
import qualified Data.Time.Format as TimeFormat
import Data.Time.LocalTime (LocalTime)

eventTypes = Map.fromList [("TIME", processTimeUpdate), ("GEO", processGeoUpdate)]

main :: IO()
main = do repoDir:eventType:params <- SysEnv.getArgs
          isRepo <- Spim.isSpimRepo repoDir
          if not isRepo 
              then do putStr ("Error: '" ++ repoDir ++ "' is not a spim repository")
                      Exit.exitWith (Exit.ExitFailure Spim.badRepoEC) 
              else do oldDir <- SysDir.getCurrentDirectory
                      SysDir.setCurrentDirectory repoDir
                      case Map.lookup eventType eventTypes of
                        Just f -> do f params
                        Nothing -> 
                            do putStr ("Event type '" ++ eventType ++ "' is not supported")
                      do SysDir.setCurrentDirectory oldDir

processTimeUpdate :: [String] -> IO ()
processTimeUpdate (localTs:[]) = do
  let newTime = case TimeFormat.parseTime Locale.defaultTimeLocale "%s" localTs of
                  Just t -> t
                  Nothing -> error "Failed to parse time"
  alarmIdx:[] <- Spim.loadIndicesByKinds ["VALARM"]
  let triggeredObjects = findTriggered newTime alarmIdx
  returnObjects triggeredObjects

returnObjects :: [String] -> IO ()
returnObjects = putStr . show   -- show triggeredObjects here somehow

findTriggered :: LocalTime -> MD.MIMEDir -> [String]
findTriggered time alarmIdx = error "not yet"

processGeoUpdate :: [String] -> IO ()
processGeoUpdate =  error "not yet"