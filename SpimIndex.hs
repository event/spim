--  Copyright 2010 Leonid Movshovich <event.riga@gmail.com>

-- This file is part of SPIM.

-- SPIM is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- SPIM is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.

-- You should have received a copy of the GNU Affero General Public License
-- along with SPIM.  If not, see <http://www.gnu.org/licenses/>.



{-
  index files are special kind of MIME-Dir for storing k:v pairs
  where k is value of field and v is list of pim objects uids holding value 
  k in indexed field 
structure:
BEGIN:INDEX
FIELD:<name of indexed field>
V1:UID11, UID12, ...
V2:UID21, UID22, ...
...
Vn:UIDn1, UIDn2, ...
END:INDEX
-}
module SpimIndex where

import qualified MIMEDir as MD
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Maybe as Mb
import qualified Data.Time.LocalTime as Time
import qualified System.Cmd as Cmd

type SpimIndex = MD.MIMEDir
indexFieldProp = "FIELD"

timeZoneId = "LOCAL_TIMEZONE"
loadIndexHook = Map.fromList [("ALARM", putCurrentTZ)]
saveIndexHook = Map.fromList [("ALARM", removeCurrentTZ)]

putCurrentTZ :: SpimIndex -> IO SpimIndex
putCurrentTZ index = do
  tz <- Time.getCurrentTimeZone
  return (MD.MIMEDir (MD.kind index) (Map.insert timeZoneId (Left [(Map.empty, show tz)]) (MD.contents index)))

removeCurrentTZ :: SpimIndex -> IO SpimIndex
removeCurrentTZ index = do return (MD.MIMEDir (MD.kind index) (Map.delete timeZoneId (MD.contents index)))

getIndexLoadHook :: String -> (SpimIndex -> IO SpimIndex)
getIndexLoadHook name = case Map.lookup name loadIndexHook of
                          Nothing -> (\ si -> do return si)
                          Just f -> f

getIndexSaveHook :: String -> (SpimIndex -> IO SpimIndex)
getIndexSaveHook name = case Map.lookup name saveIndexHook of
                          Nothing -> (\ si -> do return si)
                          Just f -> f


loadIndex :: String -> IO SpimIndex
loadIndex fld = do 
  content <- readFile ("indices/" ++ fld ++ ".idx") 
                `catch` \e -> do return ("BEGIN:INDEX\r\nFIELD:" ++ fld ++ "\r\nEND:INDEX\r\n")
  ((getIndexLoadHook fld) (Mb.fromJust $ toSpimIndex content))

saveIndex :: SpimIndex -> IO ()
saveIndex idx = do let idxName = (getIndexField idx)
                       fname = "indices/" ++ idxName ++ ".idx"
                   toWrite <- (getIndexSaveHook idxName) idx
                   writeFile fname (MD.mimeDirToString toWrite)
                   Cmd.system ("git add " ++ fname) 
                   return ()


getIndexField :: SpimIndex -> MD.PropValue
getIndexField idx = Mb.fromJust (MD.getFirstValue indexFieldProp idx)

addValueToIndex :: [MD.PropValue] -> String -> SpimIndex -> SpimIndex
addValueToIndex [] uid idx = idx
addValueToIndex value uid idx = let values = foldr ((++) . MD.propValueToList) [] value in
                                addValuesToIndex values uid idx

--TODO;Perf: first agregate all values in list to one long comma-separated string
addValuesToIndex :: [String] -> String -> SpimIndex -> SpimIndex
addValuesToIndex [] _ idx = idx
addValuesToIndex (v:vs) uid idx = addValuesToIndex vs uid (MD.appendValue v uid idx)

toSpimIndex :: String -> Maybe SpimIndex
toSpimIndex s = let index = MD.mimeDirFromString s in
                if checkIndex index then
                    Just index
                else
                    Nothing

checkIndex :: SpimIndex -> Bool
checkIndex = MD.isMIMEDirValid indexCLCheck


indexCLCheck :: MD.PropName -> MD.PropValue -> MD.Parameters -> Bool
indexCLCheck _ _ = Map.null

