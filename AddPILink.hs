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
import qualified Data.List as List
import Data.Map ((!))

main :: IO()
main = do repoDir:linkType:fromUID:toUIDs <- SysEnv.getArgs
          isRepo <- Spim.isSpimRepo repoDir
          if not isRepo 
              then do putStr ("Error: '" ++ repoDir ++ "' is not a spim repository")
                      Exit.exitWith (Exit.ExitFailure Spim.badRepoEC) 
              else do oldDir <- SysDir.getCurrentDirectory
                      SysDir.setCurrentDirectory repoDir
                      link <- Spim.loadLink linkType
                      let link = Map.insert fromUID 
                                 [(Map.empty, List.intercalate "," toUIDs)] link
                      Spim.saveLink link
                      SysDir.setCurrentDirectory oldDir



