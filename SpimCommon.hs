module SpimCommon where

import IO
import qualified System.Directory as SysDir
import qualified System.Cmd as Cmd
import qualified SpimIndex as SI
import qualified Maybe as Mb
import qualified MIMEDir as MD
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Date.Time.LocalTime as Time
import qualified Date.Time.Clock as Clock
import qualified Data.Time.Format as TimeFormat
import Data.Map ((!))

default (Int)


indexedFields = Map.fromList [("VCARD", ["EMAIL", "TEL"]), ("VCALENDAR", ["CATEGORIES"])]
indexUpdateInfo = Map.fromList [("VCARD" 
                                , Map.fromList [("EMAIL", simpleUpdate)
                                               , ("TEL", simpleUpdate)])
                               ,("VALARM", Map.singleton "ALARM" alarmUpdate)]

lookupIdxUpdFunc :: String -> String 
                 -> (SI.SpimIndex -> MD.MIMEDir -> Maybe MD.MIMEDir -> SI.SpimIndex)
lookupIdxUpdFunc fieldName dirKind = 
    case Map.lookup dirKind indexUpdateInfo of
      Nothing -> error "MIME-DIR type '" ++ dirKind ++ "' is not supported!"
      Just map -> case Map.lookup fieldName map of
                   Nothing -> noUpdate
                   Just func -> func
-- returns index unchanged
noUpdate :: SI.SpimIndex -> MD.MIMEDir -> MD.MIMEDir -> SI.SpimIndex
noUpdate index _ _ = index

{- 1. get field name from index
   2. get value of the field from mime-dir
   3. update index with value (from step 2) and mime-dir uid
-}
simpleUpdate :: SI.SpimIndex -> MD.MIMEDir -> MD.MIMEDir -> SI.SpimIndex
simpleUpdate idx dir _ = 
    case MD.getAllValues (SI.getIndexField idx) (MD.contents dir) of
      Nothing -> idx
      Just values -> SI.addValueToIndex values (MD.getSpimUID dir) idx

{-  Parses the VALARM object and updates index with ALL the occurences of the alarm either in 
      UTC (UTC and time-zone definitions) or local time (acc. to RFC 5545). UTC times are suffixed with 'Z'
-}
alarmUpdate :: SI.SpimIndex -> MD.MIMEDir -> MD.MIMEDir -> SI.SpimIndex
alarmUpdate idx dir parent = 
    let repDur = (MD.getFirstParamsAndValue "REPEAT" dir, MD.getFirstParamsAndValue "DURATION" dir) in 
    case MD.getFirstParamsAndValue "TRIGGER" dir of
      Nothing -> error "VALARM doesn't contain TRIGGER field"
      Just ([params], val) -> 
          case params of 
            [] -> SI.addValueToIndex (calcTimesRelToStart val repDur parent) (MD.getSpimUID parent) idx
            [param] -> case Map.lookup "VALUE" param of
                        Just "DATE-TIME" -> SI.addValueToIndex (calcTimesAbs val repDur) (MD.getSpimUID parent) idx
                        _ -> case Map.lookup "RELATED" param of
                              Just "END" -> SI.addValueToIndex 
                                             (calcTimesRelToEnd val repDur parent) (MD.getSpimUID parent) idx
                              _ - SI.addValueToIndex 
                                    (calcTimesRelToStart val repDur parent) (MD.getSpimUID parent) idx

calcTimesAbs :: String -> (String, String) -> [String]
calcTimesAbs val (repCount, duration) = 
    map utc2string (createTimeList (read repCount) (duration2DiffTime duration) (string2utc val))

calcTimesRelToStart :: String -> (String, String) -> MD.MIMEDir -> [String]
calcTimesRelToStart val (repCount, duration) parent = nothing

calcTimesRelToEnd :: String -> (String, String) -> MD.MIMEDir -> [String]
calcTimesRelToEnd val (repCount, duration) parent = nothing

createTimeList :: Int -> Int -> LocalTime -> [UTCTime] 
createTimeList rep diff time -> [time : createTimeList (rep - 1) (incTime time diff)]

utc2string :: UTCTime -> String
utc2string time = (TimeFormat.formatTime Locale.defaultTimeLocale MD.dateFormat time) ++ "Z"

string2utc :: String -> UTCTime
string2utc time = case TimeFormat.parseTime Locale.defaultTimeLocale MD.dateFormat time of
                    Just t -> t
                    Nothing -> error "Failed to parse time"


-- FIXME: this one is copied from InjectEvent.hs. Should be externalized 
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

-- toUTC and convertTZ2UTC are posibly unneeded
toUTC ::  MD.MIMEDir -> (Parameters, PropValue) -> String
toUTC  parent (tzParams, time) | "Z" `isSuffixOf` time = time -- UTC already
                               -- convert this one to UTC
                               | Map.member "TZID" tzParams = convertTZ2UTC parent (tzParams!"TZID") time
                               -- keep the local time (acc. to RFC-5545-3.3.5#1)
                               | otherwise = time 
     
convertTZ2UTC :: MD.MIMEDir -> ParamValue -> PropValue -> String
convertTZ2UTC parent tzName time = convertTime ((getTZDefinitions parent)!tzName) time 
    where
      convertTime tz time = nothing
      getTZDefinitions md = nothing

badRepoEC =  1
badObjectEC = 2


addToRepo :: [MD.MIMEDir] -> IO ()
addToRepo piObjects = do indices <- loadIndicesByKinds (concat (map MD.nestedKinds piObjects))
                         let updIndices = updateIndices indices piObjects 
                         saveMimeDirs piObjects
                         saveIndices updIndices
                         commit (createCommitComment piObjects)

nothing = error "not yet implemented"

loadLink :: String -> IO MD.MIMEDir
loadLink linkType = do
  content <- readFile ("links/" ++ linkType ++ ".link") 
                `catch` \e -> do return ("BEGIN:LINK\r\nTYPE:" ++ linkType ++ "\r\nEND:LINK\r\n")
  return (MD.mimeDirFromString content)

saveLink :: MD.MIMEDir -> IO ()
saveLink link = let fname = "links/" ++ (Mb.fromJust (MD.getFirstValue "TYPE" link)) ++ ".link"
                    content = MD.mimeDirToString link
                in do writeFile fname content

createCommitComment :: [MD.MIMEDir] -> String
createCommitComment _ = "comment!"

commit :: String -> IO()
commit comment = do Cmd.system ("git commit -a -m " ++ comment)
                    return ()

updateIndices :: [SI.SpimIndex] -> [MD.MIMEDir] -> [SI.SpimIndex]
updateIndices indices mimeDirs = 
    let indexMap = Map.fromList (map (\idx -> (SI.getIndexField idx, idx)) indices) in
    Map.elems (updateIdxsWDirs indexMap mimeDirs) 

updateIndices' :: [SI.SpimIndex] -> [MD.MIMEDir] -> [SI.SpimIndex]
updateIndices' [] _ = []
updateIndices' indices [] = indices
updateIndices' (idx:indices) dirs = (updateIndex idx dirs) : (updateIndices' indices dirs) 

updateIndex :: SI.SpimIndex -> [MD.MIMEDir] -> SI.SpimIndex
updateIndex idx [] = idx
updateIndex idx (dir:dirs) = 
    let newIdx = updateIndex1 idx dir in
    updateIndex newIdx dirs

updateIndex1 :: SI.SpimIndex -> MD.MIMEDir -> SI.SpimIndex
updateIndex1 idx dir = 
    let subdirs = MD.getSubDirs dir 
        indexField = SI.getIndexField idx
        afterFirstUpdate = (lookupIdxUpdFunc indexField (MD.kind dir)) idx dir dir 
    in
      foldl (\ i d -> (lookupIdxUpdFunc indexField (MD.kind d)) i d dir) 
            afterFirstUpdate subdirs

updateIdxsWDirs :: Map.Map String SI.SpimIndex -> [MD.MIMEDir] -> Map.Map String SI.SpimIndex
updateIdxsWDirs indexMap [] = indexMap
updateIdxsWDirs indexMap (dir:dirs) = updateIdxsWDirs (updateProps indexMap dir) dirs

updateProps :: Map.Map String SI.SpimIndex -> MD.MIMEDir -> Map.Map String SI.SpimIndex
updateProps indexMap dir = Map.mapWithKey (indexInsert dir) indexMap
                         
indexInsert :: MD.MIMEDir -> String -> SI.SpimIndex -> SI.SpimIndex
indexInsert dir field index = case MD.getAllValues field dir of
                                Nothing -> index
                                Just newValues -> SI.addValueToIndex 
                                                             newValues (MD.getSpimUID dir) index
                                  
saveMimeDirs :: [MD.MIMEDir] -> IO ()
saveMimeDirs [] = do return ()
saveMimeDirs (dir:dirs) = do 
  saveMimeDir dir
  saveMimeDirs dirs

saveMimeDir :: MD.MIMEDir -> IO ()
saveMimeDir dir = do 
  let fname = MD.getSpimUID dir
  writeFile fname (MD.mimeDirToString dir)
  Cmd.system ("git add " ++ fname)
  return ()

saveIndices :: [SI.SpimIndex] -> IO ()
saveIndices [] = do return ()
saveIndices (idx:idxs) = do
  SI.saveIndex idx
  saveIndices idxs


loadIndicesByKinds :: [String] -> IO [SI.SpimIndex]
loadIndicesByKinds kinds = do 
  loadIndicesByName $ Set.elems 
                        (Set.fold 
                                (\k res -> case Map.lookup k indexedFields of
                                             Just l -> res `Set.union` (Set.fromList l)
                                             Nothing -> res
                                ) Set.empty (Set.fromList kinds))


loadIndicesByName :: [String] -> IO [SI.SpimIndex]
loadIndicesByName [] = do return []
loadIndicesByName (fld:fields) = do
  head <- SI.loadIndex fld
  tail <- loadIndicesByName fields
  return (head : tail)

-- TBI
isSpimRepo :: FilePath -> IO Bool
isSpimRepo _ = do return True

readFiles :: [FilePath] -> IO [String]
readFiles [] = do return []
readFiles (fp:fps) = do 
  t <- readFile fp
  ts <- readFiles fps
  return (t:ts)

