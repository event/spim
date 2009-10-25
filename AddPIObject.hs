module Main where

import IO
import qualified System.Environment as SysEnv
import qualified System.Directory as SysDir
import qualified System.Cmd as Cmd
import qualified System.Exit as Exit
import qualified SPIMCommon as SPIM
import qualified VCard
--import qualified VCal

main :: IO()
main = do 
  repoDir:objectFNames <- SysEnv.getArgs
  isRepo <- SPIM.isSpimRepo repoDir
  if not isRepo then
      do putStr ("Error: '" ++ repoDir ++ "' is not an spim repository")
         exitWith ExitFailure SPIM.badRepoEC
  else
      do piObjects <- checkAndProcess objectFNames
         SysDir.setCurrentDirectory repoDir
         indices <- SPIM.loadIndices
         repoChanges <- mergeToRepo indices piObjects
         SPIM.updateAndStoreIndices repoChanges
         SPIM.commit

data SPIMRepoChange = RepoChange {fname :: String
                                 , was :: Maybe VCommon
                                 , is :: Maybe VCommon}

checkAndProcess :: [FilePath] -> IO [VCommon]

mergeToRepo :: [SPIMIndex] -> [VCommon] -> IO [SPIMRepoChange]

         
         
         
