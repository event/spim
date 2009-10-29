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
import Data.Map((!))

type SpimIndex = MD.MIMEDir
indexFieldProp = "FIELD"

getIndexField :: SpimIndex -> String
getIndexField idx = snd $ head (idx ! indexFieldProp)

addValueToIndex :: SpimIndex -> [MD.PropValue] -> String -> SpimIndex
addValueToIndex idx [] uid = idx
addValueToIndex idx value uid = let values = foldr ((++) . MD.propValueToList) [] value in
                                 addValuesToIndex idx values uid

addValuesToIndex :: SpimIndex -> [String] -> String -> SpimIndex
addValuesToIndex idx [] uid = idx
addValuesToIndex idx (v:vs) uid = case Map.lookup v idx of
                                    Nothing -> Map.insert v [(Map.empty, uid)] idx
                                    Just [(_, uids)] -> Map.insert v 
                                                        [(Map.empty, uids ++ "," ++ uid)] idx


toSpimIndex :: String -> Maybe SpimIndex
toSpimIndex s = let index = MD.mimeDirFromString s in
            if checkIndex index then
                Just index
            else
                Nothing

checkIndex :: SpimIndex -> Bool
checkIndex = MD.isMIMEDirValid indexCLCheck


indexCLCheck :: MD.PropName -> MD.PropValue -> MD.Parameters -> Bool
indexCLCheck _ _ params = Map.null params

