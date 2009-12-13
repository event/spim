module ICal where

import qualified MIMEDir as MD
import qualified Data.Map as Map
import Data.Map ((!))
import Data.List as List


type ICal = MD.MIMEDir

toICal :: String -> Maybe ICal
toICal s = let vc = MD.mimeDirFromString s in
           if checkICal vc then
               Just vc
           else
               Nothing

checkICal :: ICal -> Bool
checkICal ic = MD.kind ic == "VCALENDAR" && MD.isMIMEDirValid icalCLCheck ic

icalCLCheck :: MD.PropName -> MD.PropValue -> MD.Parameters -> Bool
icalCLCheck name value params | Map.member name stdProps 
                                   = (fst (stdProps!name)) value && (snd (stdProps!name)) params
                               | otherwise = checkUnsupp name

checkUnsupp :: String -> Bool
checkUnsupp = List.isPrefixOf "X-"
                                                                                                
notAllowed :: a -> Bool
notAllowed x = False
               
anyAllowed :: a -> Bool
anyAllowed x = True

stdProps :: Map.Map MD.PropName (MD.PropValue -> Bool, MD.Parameters -> Bool)
stdProps = Map.fromList [("PRODID", (anyAllowed, notAllowed))
                        , ("VERSION", (anyAllowed, notAllowed))
                        , ("CALCSCALE", (anyAllowed, anyAllowed))
                        , ("METHOD", (anyAllowed, anyAllowed))
                        ]
