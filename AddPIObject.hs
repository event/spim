module Main where

import IO
import qualified System.Environment as SysEnv
import qualified System.Directory as SysDir
import qualified System.Cmd as Cmd
import qualified System.Exit as Exit
import qualified SpimCommon as Spim
import qualified MIMEDir as MD
import qualified Data.Map as Map
import qualified Data.Char as Char
import qualified VCard
import Data.Map ((!))
--import qualified VCal

main :: IO()
main = do repoDir:objectFNames <- SysEnv.getArgs
          isRepo <- Spim.isSpimRepo repoDir
          if not isRepo 
              then do putStr ("Error: '" ++ repoDir ++ "' is not an spim repository")
                      Exit.exitWith (Exit.ExitFailure Spim.badRepoEC) 
              else do piObjects <- checkAndProcess objectFNames
                      oldDir <- SysDir.getCurrentDirectory
                      SysDir.setCurrentDirectory repoDir
                      Spim.addToRepo piObjects
                      SysDir.setCurrentDirectory oldDir
                      
  

checkAndProcess :: [FilePath] -> IO [MD.MIMEDir]
checkAndProcess fnames = do 
  strings <- Spim.readFiles fnames
  let dirs = convert strings
  fnames <- SysDir.getDirectoryContents "."
  return (setUids fnames dirs)

         
convert :: [String] -> [MD.MIMEDir]         
convert = map MD.mimeDirFromString

setUids :: [String] -> [MD.MIMEDir] -> [MD.MIMEDir]
setUids _ [] = []
setUids namesInUse (dir:dirs) = let uids = digestList (digest (snd $ head $ dir!"FN") 
                                                       ++ digest (snd $ head $ dir!"TEL")) 
                                    uid = chooseUnseen namesInUse uids
                                    newNamesInUse = uid : namesInUse
                                in
                                  (setUid uid dir) : (setUids newNamesInUse dirs) 
                                      where
                                        setUid uid = Map.insert 
                                                     MD.spimUIDProp [(Map.empty, uid)] 

chooseUnseen :: [String] -> [String] -> String
chooseUnseen seen (src:srcs) = if src `elem` seen then
                                   chooseUnseen seen srcs
                               else 
                                   src

digestList x = x : digestList (x ++ "0")

digest :: String -> String 
digest s = show  (foldr ((+) . Char.ord) 0 s)