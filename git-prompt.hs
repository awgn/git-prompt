--
-- Copyright (c) 2011 Bonelli Nicola <bonelli@antifork.org>
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
-- git-prompt:  
-- 
-- Add the following line to ~/.bashrc
--
-- export PS1='\u@\h \[\033[1;32m\]$(git-prompt path)\[\033[0m\]$(git-prompt git)$ '

import System.Process
import System.Directory
import System.Environment
import Data.List

main :: IO ()
main = do
       getArgs >>= dispatch

dispatch :: [String] -> IO ()
dispatch ["git"]  =  gitPrompt >>= putStr
dispatch ["path"] =  pathPrompt >>= putStr
dispatch _        =  error "[git] [path]" 


gitPrompt :: IO String
gitPrompt = do 
            status <- gitStatus
            brev <- case (gitBranch status) of 
                         Just "" -> gitRev 
                         Just xs -> return xs
                         Nothing -> return ""
            return $ compose' (brev) (gitIcon status) 
                where compose' [] _ = ""
                      compose' b  i = "[" ++ b ++ i ++ "]"


gitStatus :: IO [String]
gitStatus = readProcessWithExitCode "git" ["status"] [] >>= \(_, xs, _) -> return (lines xs)


gitRev :: IO String
gitRev = readProcessWithExitCode "git" ["name-rev", "--name-only", "HEAD"] [] >>= \(_,xs,_) -> return (init xs) 


gitBranch :: [String] -> Maybe String
gitBranch xs = case xs of
                    [] -> Nothing
                    _  -> Just $ gitBranch' xs
               where gitBranch' [] = ""
                     gitBranch' (y:ys) | "# On branch" `isPrefixOf` y = (words y) !! 3
                                       | otherwise =  gitBranch' ys 


gitIcon :: [String] -> String
gitIcon [] = ""
gitIcon (x:xs) | "# Changes to be committed" `isPrefixOf` x = "!"
               | "# Changes not staged"      `isPrefixOf` x = "*"
               | "# Untracked files:"        `isPrefixOf` x = "+"
               | otherwise = gitIcon xs


pathPrompt :: IO String
pathPrompt = do 
            path <- getCurrentDirectory
            home <- getEnv "HOME"
            return $ shorten (setHome home path) 
       

shorten :: FilePath -> FilePath
shorten xs | len < (gl + gr + 1) = xs
           | otherwise = take gl xs ++ "..." ++ drop (len - gr) xs
           where len = length xs
                 gl  = 10
                 gr  = 30


setHome :: FilePath -> FilePath -> FilePath
setHome xs ps | xs `isPrefixOf` ps = '~' : (snd $ splitAt (length xs) ps)  
              | otherwise = ps 
