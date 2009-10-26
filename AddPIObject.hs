module Main where

import IO
import qualified System.Environment as SysEnv
import qualified System.Directory as SysDir
import qualified System.Cmd as Cmd
import qualified System.Exit as Exit
import qualified SpimCommon as Spim
import qualified MIMEDir as MD
import qualified VCard
--import qualified VCal

main :: IO()
main = do repoDir:objectFNames <- SysEnv.getArgs
          isRepo <- Spim.isSpimRepo repoDir
          if not isRepo 
              then do putStr ("Error: '" ++ repoDir ++ "' is not an spim repository")
                      Exit.exitWith (Exit.ExitFailure Spim.badRepoEC) 
              else do piObjects <- checkAndProcess objectFNames
                      SysDir.setCurrentDirectory repoDir
                      Spim.addToRepo piObjects
                      
  

checkAndProcess :: [FilePath] -> IO [MD.MIMEDir]
checkAndProcess = error "erorr"

         
         
         
