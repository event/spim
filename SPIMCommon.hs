module SpimCommon where

import IO
import qualified SpimIndex as SI
import qualified Maybe as Mb

indecies = ["indecies/email.idx", "indecies/cellphone.idx"]
badRepoEC = 1

addToRepo :: [MIMEDir] -> IO SpimAddResult
addToRepo = do indices <- loadIndices
               repoChanges <- mergeToRepo indices piObjects
               updateAndStoreIndices repoChanges
               commit


loadIndecies :: IO [SI.SpimIndex]
loadIndecies = do
  x <- readFiles indecies
  return (map 
          (Mb.fromJust . SI.toSpimIndex) 
          x)

updateAndStoreIndicies []

isSpimRepo :: FilePath -> IO Bool
isSpimRepo _ = do return True

readFiles :: [FilePath] -> IO [String]
readFiles [] = do return []
readFiles (fp:fps) = do 
  t <- readFile fp
  ts <- readFiles fps
  return (t:ts)

