--  Copyright 2010 Leonid Movshovich <event.riga@gmail.com>

-- This file is part of SPIM.

-- SPIM is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- SPIM is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.

-- You should have received a copy of the GNU Affero General Public License
-- along with SPIM.  If not, see <http://www.gnu.org/licenses/>.

module Main where

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
                           SysDir.createDirectory "links"
                           putStr "Successfully created new personal data repository\n"
                           
    Exit.ExitFailure x -> do putStr ("Repository creation failed: git exit code: " 
                                     ++ show x ++ "\n")

