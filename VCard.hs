module VCard where

import qualified MIMEDir as MD
import qualified Data.Map as Map
import Data.Map ((!))
import Data.List as List


type VCard = MD.MIMEDir

toVCard :: String -> Maybe VCard
toVCard s = let vc = MD.mimeDirFromString s in
            if checkVCard vc then
                Just vc
            else
                Nothing

checkVCard :: VCard -> Bool
checkVCard vc = MD.kind vc == "VCARD" && MD.isMIMEDirValid vcardCLCheck vc

vcardCLCheck :: MD.PropName -> MD.PropValue -> MD.Parameters -> Bool
vcardCLCheck name value params | Map.member name stdProps 
                                   = (fst (stdProps!name)) value && (snd (stdProps!name)) params
                               | otherwise = checkUnsupp name

checkUnsupp :: String -> Bool
checkUnsupp = List.isPrefixOf "X-"
                                                                                                
notAllowed :: a -> Bool
notAllowed x = False
               
anyAllowed :: a -> Bool
anyAllowed x = True

stdProps :: Map.Map MD.PropName (MD.PropValue -> Bool, MD.Parameters -> Bool)
stdProps = Map.fromList [("NAME", (anyAllowed, notAllowed))
                        , ("PROFILE", ((=="VCARD"), notAllowed))
                        , ("SOURCE", (anyAllowed, anyAllowed))
                        , ("FN", (anyAllowed, anyAllowed))
                        , ("N" ,(anyAllowed, anyAllowed))
                        , ("NICKNAME", (anyAllowed, anyAllowed))
                        , ("PHOTO" ,(anyAllowed, anyAllowed))
                        , ("BDAY", (anyAllowed, anyAllowed))
                        , ("ADR" ,(anyAllowed, anyAllowed))
                        , ("LABEL", (anyAllowed, anyAllowed))
                        , ("TEL" ,(anyAllowed, anyAllowed))
                        , ("EMAIL", (anyAllowed, anyAllowed))
                        , ("MAILER" ,(anyAllowed, anyAllowed))
                        , ("TZ", (anyAllowed, anyAllowed))
                        , ("GEO" ,(anyAllowed, anyAllowed))
                        , ("TITLE", (anyAllowed, anyAllowed))
                        , ("ROLE" ,(anyAllowed, anyAllowed))
                        , ("LOGO", (anyAllowed, anyAllowed))
                        , ("AGENT" ,(anyAllowed, anyAllowed))
                        , ("ORG", (anyAllowed, anyAllowed))
                        , ("CATEGORIES" ,(anyAllowed, anyAllowed))
                        , ("NOTE", (anyAllowed, anyAllowed))
                        , ("PRODID" ,(anyAllowed, anyAllowed))
                        , ("REV", (anyAllowed, anyAllowed))
                        , ("SORT-STRING" ,(anyAllowed, anyAllowed))
                        , ("SOUND", (anyAllowed, anyAllowed))
                        , ("UID" ,(anyAllowed, anyAllowed))
                        , ("URL", (anyAllowed, anyAllowed))
                        , ("VERSION" ,((=="3.0"), anyAllowed))
                        , ("CLASS" ,(anyAllowed, anyAllowed))
                        , ("KEY", (anyAllowed, anyAllowed))
                        ]
