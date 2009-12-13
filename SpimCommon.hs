module SpimCommon where

import IO
import qualified System.Directory as SysDir
import qualified System.Cmd as Cmd
import qualified SpimIndex as SI
import qualified Maybe as Mb
import qualified MIMEDir as MD
import qualified Data.Map as Map
import qualified Data.Set as Set

default (Int)


indexedFields = Map.fromList [("VCARD", ["EMAIL", "TEL"]), ("VCALENDAR", ["CATEGORIES"])]
badRepoEC =  1
badObjectEC = 2


addToRepo :: [MD.MIMEDir] -> IO ()
addToRepo piObjects = do indices <- loadIndicesByKinds (map MD.kind piObjects) 
                         let updIndices = updateIndices indices piObjects 
                         saveMimeDirs piObjects
                         saveIndices updIndices
                         commit (createCommitComment piObjects)

nothing = error "not yet implemented"

loadLink :: String -> IO MD.MIMEDir
loadLink linkType = do
  content <- readFile ("links/" ++ linkType ++ ".link") 
                `catch` \e -> do return ("BEGIN:LINK\r\nTYPE:" ++ linkType ++ "\r\nEND:LINK\r\n")
  return (MD.mimeDirFromString content)

saveLink :: MD.MIMEDir -> IO ()
saveLink link = let fname = "links/" ++ (Mb.fromJust (MD.getFirstValue "TYPE" link)) ++ ".link"
                    content = MD.mimeDirToString link
                in do writeFile fname content

createCommitComment :: [MD.MIMEDir] -> String
createCommitComment _ = "comment!"

commit :: String -> IO()
commit comment = do Cmd.system ("git commit -a -m " ++ comment)
                    return ()

updateIndices :: [SI.SpimIndex] -> [MD.MIMEDir] -> [SI.SpimIndex]
updateIndices indices mimeDirs = 
    let indexMap = Map.fromList (map (\idx -> (SI.getIndexField idx, idx)) indices) in
    Map.elems (updateIdxsWDirs indexMap mimeDirs) 

updateIdxsWDirs :: Map.Map String SI.SpimIndex -> [MD.MIMEDir] -> Map.Map String SI.SpimIndex
updateIdxsWDirs indexMap [] = indexMap
updateIdxsWDirs indexMap (dir:dirs) = updateIdxsWDirs (updateProps indexMap dir) dirs

updateProps :: Map.Map String SI.SpimIndex -> MD.MIMEDir -> Map.Map String SI.SpimIndex
updateProps indexMap dir = Map.mapWithKey (indexInsert dir) indexMap
                         
indexInsert :: MD.MIMEDir -> String -> SI.SpimIndex -> SI.SpimIndex
indexInsert dir field index = case MD.getAllValues field dir of
                                Nothing -> index
                                Just newValues -> SI.addValueToIndex 
                                                             newValues (MD.getSpimUID dir) index
                                  
saveMimeDirs :: [MD.MIMEDir] -> IO ()
saveMimeDirs [] = do return ()
saveMimeDirs (dir:dirs) = do 
  saveMimeDir dir
  saveMimeDirs dirs

saveMimeDir :: MD.MIMEDir -> IO ()
saveMimeDir dir = do 
  let fname = MD.getSpimUID dir
  writeFile fname (MD.mimeDirToString dir)
  Cmd.system ("git add " ++ fname)
  return ()

saveIndices :: [SI.SpimIndex] -> IO ()
saveIndices [] = do return ()
saveIndices (idx:idxs) = do
  saveIndex idx
  saveIndices idxs

saveIndex :: SI.SpimIndex -> IO ()
saveIndex idx = do let fname = "indices/" ++ (SI.getIndexField idx) ++ ".idx"
                   writeFile fname (MD.mimeDirToString idx)
                   Cmd.system ("git add " ++ fname) 
                   return ()

loadIndicesByKinds :: [String] -> IO [SI.SpimIndex]
loadIndicesByKinds kinds = do 
  loadIndicesByName $ Set.elems 
                        (Set.fold 
                                (\k res -> case Map.lookup k indexedFields of
                                             Just l -> res `Set.union` (Set.fromList l)
                                             Nothing -> res
                                ) Set.empty (Set.fromList kinds))


loadIndicesByName :: [String] -> IO [SI.SpimIndex]
loadIndicesByName [] = do return []
loadIndicesByName (fld:fields) = do
  head <- loadIndex fld
  tail <- loadIndicesByName fields
  return (head : tail)

loadIndex :: String -> IO SI.SpimIndex
loadIndex fld = do 
  content <- readFile ("indices/" ++ fld ++ ".idx") 
                `catch` \e -> do return ("BEGIN:INDEX\r\nFIELD:" ++ fld ++ "\r\nEND:INDEX\r\n")
  return (Mb.fromJust $ SI.toSpimIndex content)

-- TBI
isSpimRepo :: FilePath -> IO Bool
isSpimRepo _ = do return True

readFiles :: [FilePath] -> IO [String]
readFiles [] = do return []
readFiles (fp:fps) = do 
  t <- readFile fp
  ts <- readFiles fps
  return (t:ts)

