module SpimIndex where

import qualified MIMEDir as MD
import qualified Data.Map as Map
import qualified Data.List as List

type SpimIndex = MD.MIMEDir

toSpimIndex :: String -> Maybe SpimIndex
toSpimIndex s = let index = MD.mimeDirFromString s in
            if checkIndex index then
                Just index
            else
                Nothing

checkIndex :: SpimIndex -> Bool
checkIndex = MD.isMIMEDirValid indexCLCheck


indexCLCheck :: MD.PropName -> MD.PropValue -> MD.Parameters -> Bool
indexCLCheck name _ params = List.isPrefixOf "Entry" name && Map.null params

