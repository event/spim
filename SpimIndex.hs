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

type SpimIndex = MD.MIMEDir
indexFieldProp = "FIELD"

getIndexField :: SpimIndex -> MD.PropValue
getIndexField idx = Mb.fromJust (MD.getFirstValue indexFieldProp idx)

addValueToIndex :: [MD.PropValue] -> String -> SpimIndex -> SpimIndex
addValueToIndex [] uid idx = idx
addValueToIndex value uid idx = let values = foldr ((++) . MD.propValueToList) [] value in
                                addValuesToIndex values uid idx

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

