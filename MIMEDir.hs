module MIMEDir where
-- MIMEDir is basically a haskell view of text/directory content-type
import qualified Data.Map as Map
import qualified Data.Maybe as Mb
import qualified Data.List as List
import Data.Map ((!))

type PropName = String
type ParamName = String
type ParamValue = String
type PropValue = String
type Parameters = Map.Map ParamName ParamValue

type MIMEDir = Map.Map PropName [(Parameters, PropValue)]

data ContentLine = ContentLine {name :: PropName
                                , parameters :: Parameters
                                , value :: PropValue}

spimUIDProp = "X-SpimUID"

getSpimUID :: MIMEDir -> PropValue
getSpimUID dir = snd $ head (dir!spimUIDProp)

addWParams :: PropName -> Parameters ->  PropValue -> MIMEDir -> MIMEDir
addWParams name params value dir = case Map.lookup name dir of
                              Just oldVal -> Map.insert name ((params, value):oldVal) dir
                              Nothing -> Map.insert name [(params, value)] dir

add :: PropName -> PropValue -> MIMEDir -> MIMEDir
add name value = addWParams name Map.empty value 

propValueToList :: PropValue -> [String]
propValueToList = splitList ','

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
readParam s = case span (/='=') s of
                (paramName, '=' : valuesStr) -> (paramName, valuesStr)
                (paramName, []) -> (paramName, [])
         
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

foldMIMEDir' :: String -> Int -> String
foldMIMEDir' "" _ = ""
foldMIMEDir' ('\r' : '\n' : rest) _ = "\r\n" ++ foldMIMEDir' rest 0
foldMIMEDir' (h : t) n | n < 75 = h : (foldMIMEDir' t (n+1))
                       | otherwise = "\r\n " ++ (h : foldMIMEDir' t 0) 

foldMIMEDir :: String -> String
foldMIMEDir str = foldMIMEDir' str 0 

unfoldMIMEDir :: String -> String
unfoldMIMEDir "" = ""
unfoldMIMEDir str | List.isPrefixOf "\r\n\t" str || List.isPrefixOf "\r\n " str 
                      = unfoldMIMEDir (drop 3 str)
                  | otherwise = (head str) : unfoldMIMEDir (tail str)

insertCl2MIMEDir :: MIMEDir -> ContentLine -> MIMEDir
insertCl2MIMEDir dir cl = let clName = name cl in
                         case Map.lookup clName dir of 
                             Just v -> Map.insert clName 
                                       ((parameters cl, value cl):v) dir
                             Nothing -> Map.insert clName
                                       [(parameters cl, value cl)] dir

contentLines2MIMEDir :: [ContentLine] -> MIMEDir
contentLines2MIMEDir = foldl (insertCl2MIMEDir) Map.empty 

mimeDir2ContentLines :: MIMEDir -> [ContentLine]
mimeDir2ContentLines = Map.foldWithKey (\k v res -> 
                       (foldr (\(prop, val) cls_per_name 
                               -> (ContentLine k prop val) : cls_per_name) [] v) ++ res) []

instance Read ContentLine where
    readsPrec _ str = let cl = takeBeforeCRLF str in
                      [(readContentLine cl, drop ((length cl) + 2) str)]

instance Eq ContentLine where
    cl1 == cl2 = (name cl1 == name cl2) && (parameters cl1 == parameters cl2) && (value cl1 == value cl2)

instance Show ContentLine where 
    showsPrec _ cl = \s -> name cl ++ (showParams . parameters) cl ++ ":" ++ value cl ++ "\r\n" ++ s

mimeDirFromString :: String -> MIMEDir
mimeDirFromString str = contentLines2MIMEDir $ readContentLines $ unfoldMIMEDir str

mimeDirToString :: MIMEDir -> String
mimeDirToString dir = foldMIMEDir (foldr shows "" (mimeDir2ContentLines dir))

isMIMEDirValid :: (PropName -> PropValue -> Parameters -> Bool) -> MIMEDir -> Bool
isMIMEDirValid checkFunc dir = and (map (checkCL checkFunc) cls) where 
    cls = mimeDir2ContentLines dir
   

checkCL :: (PropName -> PropValue -> Parameters -> Bool) -> ContentLine -> Bool
checkCL checkFunc cl = checkFunc (name cl) (value cl) (parameters cl)
