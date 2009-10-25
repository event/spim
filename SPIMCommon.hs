module SPIMCommon where

import IO
import qualified SpimIndex as SI

indecies = ["indecies/email.idx", "indecies/cellphone.idx"]
badRepoEC = 1

isSpimRepo :: FilePath -> IO Bool
isSpimRepo _ = do return True

loadIndecies :: IO [SI.SpimIndex]
loadIndecies = do
  x <- readFiles indecies
  return map SI.toSpimIndex x

readFiles :: [FilePath] -> IO [String]
readFiles [] = IO []
readFiles [fp:fps] = do 
  t <- readFile fp
  ts <- readFiles fps
  return t:ts

