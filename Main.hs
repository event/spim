-- add a vcard/vcalendar file into the pim graph
module Main where

import IO
import qualified System.Console.GetOpt as GetOpt
import qualified System.Environment as SysEnv
import qualified System.Directory as SysDir
import qualified System.IO as SIO
import qualified Text.Printf as Printf

main :: IO()
main = do 
  args <- SysEnv.getArgs
  lines <- getFileContents (head args)  
  showLines lines 1

getFileContents :: String -> IO [String]
getFileContents fName = do
  contents <- readFile fName
  return (lines contents)
      
      
showLines :: [String] -> Int -> IO()

showLines [] _ = Printf.printf "End.\n"

showLines contents idx = do
  Printf.printf "%03d. %s\n"  idx (head contents)
  showLines (tail contents) (idx + 1)
  