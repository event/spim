-- add a vcard/vcalendar file into the pim graph
module Main where

import IO
import qualified System.Console.GetOpt as GetOpt
import qualified System.Environment as SysEnv
import qualified System.Directory as SysDir

main :: IO()
main = do 
  args <- SysEnv.getArgs
  fname <- head args
  contents <- getFileContents fname  `catch` (\err -> return ("Failed to open `" ++ fname ++ "' because of " ++ show err))
  showLines contents 1

getFileContents :: String -> IO [String]
getFileContents(fName) = do
  contents <- readFile fName
  return (lines contents)
      
      
showLines :: [String] -> Int -> IO()
showLines(contents, idx) = do
  printf "%03d. %s" (head contents) idx
  showLines(tail contents, idx + 1)
  