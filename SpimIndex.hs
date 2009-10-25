module SpimIndex where

import qualified MIMEDir as MD
import qualified Data.Map as Map

type SpimIndex = MD.MIMEDir

toSpimIndex :: String -> Maybe SpimIndex
toSpimIndex s = let index = MD.mimeDirFromString s in
            if checkIndex index then
                Just index
            else
                Nothing

checkIndex :: SpimIndex -> Bool
checkIndex = isMIMEDirValid indexCLCheck


indexCLCheck :: PropName -> PropValue -> Parameters -> Bool
indexCLCheck name _ params = isPrefixOf "Entry" PropName && Map.null params

