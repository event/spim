module VCommon where

import qualified Data.Map as Map
import qualified Data.Maybe as Mb
import qualified Data.List as List

type PropName = String
type ParamName = String
type ParamValue = String
type PropValue = String
type Parameters = Map.Map ParamName ParamValue

type VCommon = Map.Map PropName [(Parameters, PropValue)]

data ContentLine = ContentLine {name :: PropName,
                                parameters :: Parameters,
                                value :: PropValue}

splitList :: (Eq a) => a -> [a] -> [ [a] ]
splitList _ [] = [] 
splitList x xs = let t = span (/=x) xs 
                 in
                   fst t : case snd t of -- the first elem of `snd t' is x by definition of span
                             _ : [] -> [ [ ] ]
                             _ : rest -> splitList x rest
                             [] -> []
                                               
readContentLine :: String -> ContentLine
readContentLine str = let prefixValue = splitList ':' str
                          (prefix, v) = if length prefixValue == 2 then
                                            (prefixValue!!0, prefixValue!!1)
                                        else
                                            error ("Bad content line '" ++ str 
                                                   ++ "': no semicolon found")
                          (n, params) = span (/=';') prefix
                          p = readParams params 
                      in ContentLine n p v

readParams :: String -> Parameters
readParams (';' : paramStr) = Map.fromList $ map readParam (splitList ';' paramStr)
readParams [] = Map.empty

readParam :: String -> (ParamName, ParamValue)
readParam s = let (paramName, '=' : valuesStr) = span (/='=') s in
              (paramName, valuesStr)
         
showParams :: Parameters -> String
showParams = Map.foldWithKey (\k v res -> res ++ ";" ++ k ++ "=" ++ v) ""
               
takeBeforeCRLF :: String -> String
takeBeforeCRLF "" = ""
takeBeforeCRLF ('\r' : '\n' : _) = ""
takeBeforeCRLF (hchar : strtail) = hchar : takeBeforeCRLF strtail 

readContentLines :: String -> [ContentLine] 
readContentLines "" = []
readContentLines str = case reads str :: [(ContentLine, String)] of 
                         [(cl, rest)] -> cl : readContentLines rest
                         _ -> error ("Failed to parse line '" ++ str ++ "'")

foldVCommon' :: String -> Int -> String
foldVCommon' "" _ = ""
foldVCommon' ('\r' : '\n' : rest) _ = "\r\n" ++ foldVCommon' rest 0
foldVCommon' (h : t) n | n < 75 = h : (foldVCommon' t (n+1))
                       | otherwise = "\r\n " ++ (h : foldVCommon' t 0) 

foldVCommon :: String -> String
foldVCommon str = foldVCommon' str 0 

unfoldVCommon :: String -> String
unfoldVCommon "" = ""
unfoldVCommon str | List.isPrefixOf "\r\n\t" str || List.isPrefixOf "\r\n " str 
                      = unfoldVCommon (drop 3 str)
                  | otherwise = (head str) : unfoldVCommon (tail str)

insertCl2VCommon :: VCommon -> ContentLine -> VCommon
insertCl2VCommon vc cl = let clName = name cl in
                         case Map.lookup clName vc of 
                             Just v -> Map.insert clName 
                                       ((parameters cl, value cl):v) vc
                             Nothing -> Map.insert clName
                                       [(parameters cl, value cl)] vc

contentLines2VCommon :: [ContentLine] -> VCommon
contentLines2VCommon = foldl (insertCl2VCommon) Map.empty 

vCommon2ContentLines :: VCommon -> [ContentLine]
vCommon2ContentLines = Map.foldWithKey (\k v res -> 
                       (foldr (\(prop, val) cls_per_name 
                               -> (ContentLine k prop val) : cls_per_name) [] v) ++ res) []

instance Read ContentLine where
    readsPrec _ str = let cl = takeBeforeCRLF str in
                      [(readContentLine cl, drop ((length cl) + 2) str)]

instance Eq ContentLine where
    cl1 == cl2 = (name cl1 == name cl2) && (parameters cl1 == parameters cl2) && (value cl1 == value cl2)

instance Show ContentLine where 
    showsPrec _ cl = \s -> name cl ++ (showParams . parameters) cl ++ ":" ++ value cl ++ "\r\n" ++ s

vCommonFromString :: String -> VCommon
vCommonFromString str = contentLines2VCommon $ readContentLines $ unfoldVCommon str

vCommonToString :: VCommon -> String
vCommonToString vc = (foldVCommon . (shows $ vCommon2ContentLines vc)) ""

isVCommonValid :: (PropName -> PropValue -> Parameters -> Bool) -> VCommon -> Bool
isVCommonValid checkFunc vc = and (map (checkCL checkFunc) cls) where 
    cls = vCommon2ContentLines vc
   

checkCL :: (PropName -> PropValue -> Parameters -> Bool) -> ContentLine -> Bool
checkCL checkFunc cl = checkFunc (name cl) (value cl) (parameters cl)
