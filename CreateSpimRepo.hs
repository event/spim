hmodule Main where

import IO
import qualified System.Environment as SysEnv
import qualified System.Directory as SysDir
import qualified System.Cmd as Cmd
import qualified System.Exit as Exit

main :: IO()
main = do 
  args <- SysEnv.getArgs
  SysDir.createDirectoryIfMissing True (head args)
  SysDir.setCurrentDirectory (head args)
  exitCode <- Cmd.system "git init"  
  case exitCode of 
    Exit.ExitSuccess -> do SysDir.createDirectory "indices"
                           putStr "Successfully created new personal data repository\n"
                           
    Exit.ExitFailure x -> do putStr ("Repository creation failed: git exit code: " 
                                     ++ show x ++ "\n")

