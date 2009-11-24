module SpimCommon where

import IO
import qualified System.Directory as SysDir
import qualified System.Cmd as Cmd
import qualified SpimIndex as SI
import qualified Maybe as Mb
import qualified MIMEDir as MD
import qualified Data.Map as Map
import Data.Map ( (!) )

default (Int)


indexedFields = ["EMAIL", "TEL"]
badRepoEC =  1
badObjectEC = 2


addToRepo :: [MD.MIMEDir] -> IO ()
addToRepo piObjects = do indices <- loadIndices 
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
saveLink link = let fname = "links/" ++ (snd $ head $ link!"TYPE") ++ ".link"
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
indexInsert dir field index = case Map.lookup field dir of
                                Nothing -> index
                                Just v -> let newValues = snd (unzip $ v) in
                                          SI.addValueToIndex index newValues (MD.getSpimUID dir)
                                  
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

loadIndices :: IO [SI.SpimIndex]
loadIndices = do
  loadIndices indexedFields
  where
    loadIndices [] = do return []
    loadIndices (fld:fields) = do
                head <- loadIndex fld
                tail <- loadIndices fields
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

