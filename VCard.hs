module VCard where

import qualified VCommon as VC
import qualified Data.Map as Map

date VCard = VC.VCommon

checkVCard :: VCard -> Bool
checkVCard = isVCommonValid vcardCLCheck

checkRemoveFirstLast :: [ContentLine] -> Maybe [ContentLine]
checkRemoveFirstLast cl:cls = let res = init cls
                                  lastCl = last cls
                                  goodBegin = (name cl) == "BEGIN" 
                                              && (value cl) == "VCARD" 
                                              && Map.null (properties cl)
                                  goodEnd = (name cl) == "END" 
                                            && (value cl) == "VCARD" 
                                            && Map.null (properties cl)
                              in
                                if goodBegin && goodEnd then 
                                    Just res
                                else
                                    Nothing


vcardCLCheck :: PropName -> PropValue -> Parameters -> Bool
vcardCLCheck name value params | Map.member name stdProps 
                                   = (fst stdProps!name) value && (snd stdProps!name) params
                               | otherwise = checkUnsupp name

checkUnsupp :: String -> Bool
checkUnsupp = isPrefixOf "X-"
 
notAllowed :: a -> Bool
notAllowed x = False

anyAllowed :: a -> Bool
anyAllowed x = True

stdProps :: Map.Map VC.PropName (VC.PropValue -> Bool, VC.Parameters -> Bool)
stdProps = Map.fromList [("NAME", (anyAllowed, notAllowed))
                        , ("BEGIN", (=="VCARD", notAllowed))
                        , ("END", (=="VCARD", notAllowed))
                        , ("PROFILE", (=="VCARD", notAllowed))
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
