module Main where

import IO
import qualified System.Environment as SysEnv
import qualified System.Directory as SysDir
import qualified System.Exit as Exit
import qualified System.Locale as Locale
import qualified SpimCommon as Spim
import qualified MIMEDir as MD
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Time.Format as TimeFormat
import qualified Data.Time.LocalTime as Time
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Calendar as Cal

import Data.Time.LocalTime (LocalTime)
import Data.Ratio ((%))

--default (Int)

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


{- 
  time in index must fall in this bounds comparing to time submitted from command line
     for corresponding object to be triggered
-}
secDiffToTrigger = (-6, 6) 

processTimeUpdate :: [String] -> IO ()
processTimeUpdate (localTs:[]) = do
  let newTime = case TimeFormat.parseTime Locale.defaultTimeLocale MD.dateFormat localTs of
                  Just t -> t
                  Nothing -> error "Failed to parse time"
  alarmIdx:[] <- Spim.loadIndicesByKinds ["VALARM"]
  curTZ <- Time.getCurrentTimeZone
  let triggeredObjects = findTriggered newTime secDiffToTrigger curTZ alarmIdx
  returnObjects triggeredObjects

returnObjects :: [String] -> IO ()
returnObjects = putStr . show

findTriggered :: LocalTime -> (Int, Int) -> Time.TimeZone -> MD.MIMEDir -> [String]
findTriggered now (lowBound, highBound) tz 
    = MD.filterValuesWProps (\ pN props -> isTimeWithinBounds (incTime now lowBound) 
                             (incTime now highBound) tz pN)

incTime :: LocalTime -> Int -> LocalTime
incTime base diff 
    = let resFrac = ((Time.timeOfDayToDayFraction . Time.localTimeOfDay) base) 
                    + ((toInteger diff) % secInDay) where secInDay = 24*60*60
          d = Time.localDay base
      in
        if resFrac < 1 then
            Time.LocalTime d (Time.dayFractionToTimeOfDay resFrac)
        else
            Time.LocalTime (Cal.addDays 1 d) (Time.dayFractionToTimeOfDay (resFrac - 1))

isTimeWithinBounds :: LocalTime -> LocalTime -> Time.TimeZone -> MD.PropName -> Bool
isTimeWithinBounds lowTime highTime tz trgTime
    = let alarmTime = getLocalFromUTC trgTime tz in
      (alarmTime > lowTime) && (alarmTime < highTime)

getLocalFromUTC :: MD.PropName -> Time.TimeZone -> LocalTime
getLocalFromUTC strTimeUTC tz 
    = case TimeFormat.parseTime Locale.defaultTimeLocale MD.dateFormat strTimeUTC of
        Just utcTime -> Time.utcToLocalTime tz utcTime
        Nothing -> error ("time string '" ++ strTimeUTC ++ "' is not a valid iCal time")

processGeoUpdate :: [String] -> IO ()
processGeoUpdate =  error "not yet"