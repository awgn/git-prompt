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
-- export COLUMNS
-- export PS1='\u@\h \[\033[1;32m\]$(git-prompt path)\[\033[0m\]$(git-prompt git)$ '

import System.Process
import System.Directory
import System.Environment
import System.Console.ANSI

import Control.Applicative
import Data.List

main :: IO ()
main = do
       getArgs >>= dispatch


dispatch :: [String] -> IO ()
dispatch ["git"]  =  gitPrompt >>= putStr
dispatch ["path"] =  pathPrompt >>= putStr
dispatch _        =  error "[git] [path]" 


magenta, blue, red, bold, reset :: String

magenta = setSGRCode [SetColor Foreground Vivid Magenta]
blue    = setSGRCode [SetColor Foreground Vivid Blue]
red     = setSGRCode [SetColor Foreground Vivid Red]
bold    = setSGRCode [SetConsoleIntensity BoldIntensity]
reset   = setSGRCode []


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
gitStatus = readProcessWithExitCode "git" ["status"] [] >>= \(_,x,_) -> 
                return (lines x)


gitRev :: IO String
gitRev = readProcessWithExitCode "git" ["name-rev", "--name-only", "HEAD"] [] >>= \(_,x,_) -> 
            return (init x) 


gitBranch :: [String] -> Maybe String
gitBranch xs = case xs of
                    [] -> Nothing
                    _  -> Just $ gitBranch' xs
               where gitBranch' [] = ""
                     gitBranch' (y:ys) | "# On branch" `isPrefixOf` y = magenta ++ bold ++ (words y) !! 3 ++ reset
                                       | otherwise =  gitBranch' ys 


hasUntrackedFiles :: [String] -> Bool
hasUntrackedFiles = any (isPrefixOf "# Untracked files:") 


hasChangesToBeCommitted :: [String] -> Bool
hasChangesToBeCommitted =  any (isPrefixOf "# Changes to be committed")


hasChangesNotStaged :: [String] -> Bool
hasChangesNotStaged = any (isPrefixOf "# Changes not staged")


gitIcon :: [String] -> String
gitIcon xs = gitIcon' (hasUntrackedFiles xs) (hasChangesNotStaged xs) (hasChangesToBeCommitted xs)
             where  gitIcon' False  False  False = ""  
                    gitIcon' True   False  False = "|…" 
                    gitIcon' False  True   False = "|" ++ bold ++ blue ++ "×" ++ reset  
                    gitIcon' True   True   False = "|" ++ bold ++ blue ++ "×" ++ reset ++ "…" 
                    gitIcon' False  False  True  = "|" ++ bold ++ red  ++ "٭" ++ reset  
                    gitIcon' True   False  True  = "|" ++ bold ++ red  ++ "٭" ++ reset ++ "…" 
                    gitIcon' False  True   True  = "|" ++ bold ++ red  ++ "¡" ++ reset 
                    gitIcon' True   True   True  = "|" ++ bold ++ red  ++ "¡" ++ reset ++ "…"    


pathPrompt :: IO String
pathPrompt = shorten <$> (read <$> getEnv "COLUMNS") <*> (setHome <$> (getEnv "HOME") <*> getCurrentDirectory)


shorten :: Int -> FilePath -> FilePath
shorten col xs | len < (gl + 3 + gr) = xs
               | otherwise = take gl xs ++ "..." ++ drop (len - gr) xs
           where len = length xs
                 ml  = col `div` 2
                 gl  = 10
                 gr  = ml - gl - 20  


setHome :: FilePath -> FilePath -> FilePath
setHome xs ps | xs `isPrefixOf` ps = '~' : (snd $ splitAt (length xs) ps)  
              | otherwise = ps 

