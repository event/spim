module SpimCommon where

import IO
import System.Directory as SysDir
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
--                         commit
                         


printIdxs :: [SI.SpimIndex] -> IO ()
printIdxs [] = do return ()
printIdxs (idx:idxs) = do 
  putStr (MD.mimeDirToString idx)
  printIdxs idxs

nothing = error "not yet implemented"

commit :: IO()
commit = nothing

-- updateIndicies have to generate warnings
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
saveMimeDir dir = do writeFile (MD.getSpimUID dir) (MD.mimeDirToString dir)

saveIndices :: [SI.SpimIndex] -> IO ()
saveIndices [] = do return ()
saveIndices (idx:idxs) = do
  saveIndex idx
  saveIndices idxs

saveIndex :: SI.SpimIndex -> IO ()
saveIndex idx = do writeFile ("indices/" ++ (SI.getIndexField idx) ++ ".idx") 
                                 (MD.mimeDirToString idx)

loadIndices :: IO [SI.SpimIndex]
loadIndices = do
  x <- readFiles (map (\fld -> "indices/" ++ fld ++ ".idx") indexedFields)
  return (map 
          (Mb.fromJust . SI.toSpimIndex) 
          x)

isSpimRepo :: FilePath -> IO Bool
isSpimRepo _ = do return True

readFiles :: [FilePath] -> IO [String]
readFiles [] = do return []
readFiles (fp:fps) = do 
  t <- readFile fp
  ts <- readFiles fps
  return (t:ts)

