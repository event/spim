module Main where

import IO
import qualified System.Environment as SysEnv
import qualified System.Directory as SysDir
import qualified System.Exit as Exit
import qualified SpimCommon as Spim
import qualified MIMEDir as MD
import qualified Data.Map as Map

eventTypes = Map.fromList [("TIME", processTimeUpdate), ("GEO", processGeoUpdate)]

main :: IO()
main = do repoDir:eventType:params <- SysEnv.getArgs
          isRepo <- Spim.isSpimRepo repoDir
          if not isRepo 
              then do putStr ("Error: '" ++ repoDir ++ "' is not a spim repository")
                      Exit.exitWith (Exit.ExitFailure Spim.badRepoEC) 
              else do oldDir <- SysDir.getCurrentDirectory
                      SysDir.setCurrentDirectory repoDir
                      case Map.lookup eventType eventTypes of
                        Just f -> do f params
                        Nothing -> 
                            do putStr ("Event type '" ++ eventType ++ "' is not supported")
                      do SysDir.setCurrentDirectory oldDir

processTimeUpdate :: [String] -> IO ()
processTimeUpdate = error "not yet"

processGeoUpdate :: [String] -> IO ()
processGeoUpdate =  error "not yet"