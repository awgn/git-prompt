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
-- export PS1='\u :: \[\033[1;32m\]$(git-prompt path)\[\033[0m\] $(git-prompt git) -> '
--
-- or
-- export PS1='$(git-prompt full)'
--

import System.Process
import System.Directory
import System.Environment
import System.Console.ANSI
import System.Posix.User

import Control.Applicative
import Data.List


main :: IO ()
main = getArgs >>= dispatch


dispatch :: [String] -> IO ()
dispatch ["git"]    =  gitPrompt  >>= putStr
dispatch ["path"]   =  pathPrompt >>= putStr
dispatch ["full"]   =  fullPrompt >>= putStr
dispatch _          =  error "[git] [path] [full]" 


magenta, blue, red, cyan, green, bold, reset :: String

magenta = setSGRCode [SetColor Foreground Vivid Magenta]
blue    = setSGRCode [SetColor Foreground Vivid Blue]
cyan    = setSGRCode [SetColor Foreground Vivid Cyan]
green   = setSGRCode [SetColor Foreground Vivid Green]
red     = setSGRCode [SetColor Foreground Vivid Red]
bold    = setSGRCode [SetConsoleIntensity BoldIntensity]
reset   = setSGRCode []


fullPrompt :: IO String
fullPrompt =  liftA3 make pathPrompt gitPrompt getEffectiveUserName
                where make path git user = user ++ " :: " ++ green ++ bold ++ path ++ reset ++ " " ++ git ++ " -> " 


gitPrompt :: IO String
gitPrompt =  liftA3 (\a b c -> a ++ b ++ c) gitNameRev gitAheadIcon gitStatusIcon  >>= \prompt -> 
                return $ if (null prompt) then "" else "[" ++ prompt ++ "]" 


gitStatusIcon :: IO String
gitStatusIcon = liftA (concat . nub . (map gitIcon)) gitStatus >>= \icon ->
                    return $ if (null icon) then "" else ("|" ++ icon)  


gitIcon :: String -> String              
gitIcon (' ':'M':_) =  bold ++ blue  ++ "±" ++ reset
gitIcon (_  :'D':_) =  bold ++ red   ++ "—" ++ reset
gitIcon ('M':' ':_) =  bold ++ blue  ++ "٭" ++ reset
gitIcon ('A':' ':_) =  bold ++ blue  ++ "⦁" ++ reset
gitIcon ('M':_  :_) =  bold ++ cyan  ++ "٭" ++ reset
gitIcon ('A':_  :_) =  bold ++ cyan  ++ "⦁" ++ reset
gitIcon ('D':_  :_) =  bold ++ red   ++ "╌" ++ reset
gitIcon ('R':_  :_) =  bold ++ cyan  ++ "ʀ" ++ reset
gitIcon ('C':_  :_) =  bold ++ cyan  ++ "‡" ++ reset
gitIcon ('?':'?':_) =  "…"
gitIcon  _          =  ""


gitStatus :: IO [String]
gitStatus = readProcessWithExitCode "git" ["status", "--porcelain"] [] >>= \(_,x,_) -> 
                return (lines x)


gitNameRev :: IO String
gitNameRev = readProcessWithExitCode "git" ["name-rev", "--name-only", "HEAD"] [] >>= \(_,x,_) -> 
                return $ if (null x) then "" else (magenta ++ bold ++ init x ++ reset) 


gitAheadIcon :: IO String
gitAheadIcon = readProcessWithExitCode "git" ["rev-list", "-n", "1", "HEAD@{upstream}..HEAD"] [] >>= \(_,x,_) ->
                    return $ if (not . null $ lines x) then (bold ++ "↑" ++ reset) else "" 


pathPrompt :: IO String
pathPrompt = liftA2 shorten (read <$> getEnv "COLUMNS") (setHome <$> (getEnv "HOME") <*> getCurrentDirectory)


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

