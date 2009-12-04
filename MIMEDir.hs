module MIMEDir where
-- MIMEDir is basically a haskell view of text/directory content-type
import qualified Data.Map as Map
import qualified Data.Maybe as Mb
import qualified Data.List as List

type PropName = String
type ParamName = String
type ParamValue = String
type PropValue = String
type Parameters = Map.Map ParamName ParamValue

type MIMEDirContents = Map.Map PropName (Either [(Parameters, PropValue)] MIMEDir)

data MIMEDir = MIMEDir {kind :: String
                       , contents :: MIMEDirContents}

data ContentLine = ContentLine {name :: PropName
                                , parameters :: Parameters
                                , value :: PropValue}

spimUIDProp = "X-SpimUID"

getSpimUID :: MIMEDir -> PropValue
getSpimUID = Mb.fromJust . getFirstValue spimUIDProp

addWParams :: PropName -> Parameters ->  PropValue -> MIMEDir -> MIMEDir
addWParams name params value dir = 
    let oldContent = contents dir
        newContent = addContentWParams name params value oldContent
    in MIMEDir (kind dir) newContent

addContentWParams :: PropName -> Parameters ->  PropValue -> MIMEDirContents -> MIMEDirContents
addContentWParams name params value content = 
    case Map.lookup name content of
      Just (Left oldVal) -> Map.insert name 
                                    (Left ((params, value):oldVal)) content
      Nothing -> Map.insert name (Left [(params, value)]) content

add :: PropName -> PropValue -> MIMEDir -> MIMEDir
add name value = addWParams name Map.empty value 

appendValue :: PropName -> PropValue -> MIMEDir -> MIMEDir
appendValue name val dir = case getFirstParamsAndValue name dir of
                             Nothing -> add name val dir
                             Just (p, oldVal) -> MIMEDir (kind dir) 
                                                 (Map.insert name 
                                                    (Left [(p, oldVal ++ "," ++ val)]) 
                                                    (contents dir))


getAllValues :: PropName -> MIMEDir -> Maybe [PropValue]
getAllValues name dir = case Map.lookup name (contents dir) of
                          Nothing -> Nothing
                          Just (Left val) -> Just (map snd val)
                          Just (Right _) -> Nothing

getAllParamsAndValues :: PropName -> MIMEDir -> Maybe [(Parameters, PropValue)]
getAllParamsAndValues name dir = case Map.lookup name (contents dir) of
                                   Nothing -> Nothing
                                   Just (Left val) -> Just val
                                   Just (Right _) -> Nothing

getFirst :: (a -> b -> Maybe [c]) -> a -> b -> Maybe c
getFirst f x y = case f x y of
                   Nothing -> Nothing
                   Just l -> Just (head l)

getFirstValue :: PropName -> MIMEDir -> Maybe PropValue
getFirstValue = getFirst getAllValues 

getFirstParamsAndValue :: PropName -> MIMEDir -> Maybe (Parameters, PropValue)
getFirstParamsAndValue = getFirst getAllParamsAndValues
                                    

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

insertCls2MIMEDir :: [ContentLine] -> MIMEDirContents -> MIMEDirContents
insertCls2MIMEDir [] dir = dir
insertCls2MIMEDir lines dir | name (head lines) == "BEGIN" 
                                = let (nested, rest) = splitBeginEnd lines
                                      nestedDir = contentLines2MIMEDir nested 
                                      newDir = Map.insert (getNestedDirKey dir) 
                                                    (Right nestedDir) dir 
                                  in insertCls2MIMEDir rest newDir
                            | otherwise 
                                = let cl:cls = lines in
                                  insertCls2MIMEDir 
                                      cls (addContentWParams 
                                           (name cl) (parameters cl) (value cl) dir)

contentLines2MIMEDir :: [ContentLine] -> MIMEDir
contentLines2MIMEDir ((ContentLine "BEGIN" _ kind):cls) =
    let content = init cls in
    MIMEDir kind (contentLines2MIMEDirContent content)
contentLines2MIMEDir _ = error "MIME-DIR must start with BEGIN:<TYPE>!"

splitBeginEnd :: [ContentLine] -> ([ContentLine], [ContentLine])
splitBeginEnd = error "Not yet!"

getNestedDirKey :: MIMEDirContents -> PropName
getNestedDirKey = error "Not yet!"

contentLines2MIMEDirContent :: [ContentLine] -> MIMEDirContents
contentLines2MIMEDirContent cls = insertCls2MIMEDir cls Map.empty

mimeDir2ContentLines :: MIMEDir -> [ContentLine]
mimeDir2ContentLines dir = 
    let k = kind dir in 
    [ContentLine "BEGIN" Map.empty k] 
     ++ (mimeDirContent2Cls (contents dir))
     ++ [ContentLine "END" Map.empty k] 

mimeDirContent2Cls :: MIMEDirContents -> [ContentLine]
mimeDirContent2Cls = 
    Map.foldWithKey (\k v res -> 
                         let cls = case v of
                                     Left propAndVal -> dirContentEntry2Cls k propAndVal
                                     Right nestedDir -> mimeDir2ContentLines nestedDir
                         in cls ++ res) []

dirContentEntry2Cls :: PropName -> [(Parameters, PropValue)] -> [ContentLine]
dirContentEntry2Cls name = 
    foldr (\(prop, val) cls_per_name 
               -> (ContentLine name prop val) : cls_per_name) []

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
