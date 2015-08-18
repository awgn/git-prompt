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


import System.Process
import System.Directory
import System.Environment
import System.Console.ANSI

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import Control.Applicative
import Data.Maybe
import Data.List
import Data.List.Split


type MaybeIO = MaybeT IO

main :: IO ()
main = getArgs >>= dispatch

dispatch :: [String] -> IO ()
dispatch [xs]  =  gitPrompt xs >>= putStr
dispatch _     =  error "git-prompt [blue|red|green|cyan|magenta|yellow|white]"


magenta, blue, red, cyan, green, bold, reset :: String


magenta = setSGRCode [SetColor Foreground Vivid Magenta]
blue    = setSGRCode [SetColor Foreground Vivid Blue]
cyan    = setSGRCode [SetColor Foreground Vivid Cyan]
green   = setSGRCode [SetColor Foreground Vivid Green]
red     = setSGRCode [SetColor Foreground Vivid Red]
yellow  = setSGRCode [SetColor Foreground Vivid Yellow]
white   = setSGRCode [SetColor Foreground Vivid White]
bold    = setSGRCode [SetConsoleIntensity BoldIntensity]
reset   = setSGRCode []


getColorByName :: String -> String
getColorByName "blue"    = blue
getColorByName "red"     = red
getColorByName "green"   = green
getColorByName "cyan"    = cyan
getColorByName "magenta" = magenta
getColorByName "yellow"  = yellow
getColorByName "white"   = white
getColorByName _         = reset


-- 0: gitBranchName

sepPrompt :: String -> MaybeIO String
sepPrompt xs = return $ if null xs then xs
                             else xs ++ "|"

gitPrompt :: String -> IO String
gitPrompt colorname = do
    prompt <- runMaybeT
        (liftM concat $ sequence [gitBranchName colorname,
                                  return "|",
                                  gitDescribe,
                                  return "|",
                                  gitStashCounter >>= sepPrompt,
                                  gitAheadIcon,
                                  gitStatusIcon
                                  ])
    return $ if isJust prompt
                then bold ++ "(" ++ reset ++ fromJust prompt ++ bold ++ ")" ++ reset
                else ""


-- 1: gitBranchName

gitBranchName :: String -> MaybeIO String
gitBranchName colorname =
    MaybeT $ liftA2 (<|>) (gitSymbolicRef color) (gitNameRev color)
        where color = getColorByName colorname


gitSymbolicRef :: String -> IO (Maybe String)
gitSymbolicRef color = do
    xs <- gitCommand ["symbolic-ref", "HEAD"]
    return $ if null xs then Nothing
                        else Just (color ++ bold ++ filter (/= '\n') (last $ splitOn "/" xs) ++ reset)


gitNameRev :: String -> IO (Maybe String)
gitNameRev color = do
    xs <- liftIO $ gitCommand ["name-rev", "--name-only", "HEAD"]
    return $ if null xs then Nothing
                        else Just (replace "~" (reset ++ bold ++ "↓" ++ reset) (color ++ bold ++ init xs ++ reset))


-- 2: gitDescribe

gitDescribe :: MaybeIO String
gitDescribe = liftIO (gitCommand ["describe", "--abbrev=6", "--dirty=!", "--always", "--all", "--long"]) >>= \xs ->
    failIfNull xs >> return (filter (/= '\n') xs)



-- 3: gitAheadIcon

gitAheadIcon :: MaybeIO String
gitAheadIcon = do
    xs <- liftIO $ gitCommand ["rev-list", "--count", "HEAD@{upstream}..HEAD"]
    return (if null xs || read xs == (0 :: Integer)
               then ""
               else bold ++ "↑" ++ reset ++ show(read xs :: Integer))


-- 4: gitStatusIcon

gitStatusIcon :: MaybeIO String
gitStatusIcon = liftIO $ liftM (concat . nub . map gitIcon . lines) (gitCommand ["status", "--porcelain"])


-- 5: gitStashCounter:

gitStashCounter:: MaybeIO String
gitStashCounter = do
    n <- liftIO $ liftM (length . lines) (gitCommand ["stash", "list"])
    if n == 0 then return ""
              else return $ bold ++ show n ++ reset


gitIcon :: String -> String
gitIcon (' ':'M':_) =  bold ++ blue  ++ "±" ++ reset
gitIcon (_  :'D':_) =  bold ++ red   ++ "—" ++ reset
gitIcon ('M':' ':_) =  bold ++ blue  ++ "٭" ++ reset
gitIcon ('A':' ':_) =  bold ++ green ++ "✛" ++ reset
gitIcon ('M':_  :_) =  bold ++ cyan  ++ "٭" ++ reset
gitIcon ('A':_  :_) =  bold ++ cyan  ++ "✛" ++ reset
gitIcon ('D':_  :_) =  bold ++ red   ++ "╌" ++ reset
gitIcon ('R':_  :_) =  bold ++ cyan  ++ "ʀ" ++ reset
gitIcon ('C':_  :_) =  bold ++ cyan  ++ "‡" ++ reset
gitIcon ('?':'?':_) =  "…"
gitIcon  _          =  ""


replace :: String -> String -> String -> String
replace x y xs =  intercalate y $ splitOn x xs


gitCommand :: [String] -> IO String
gitCommand arg = liftM(\(_, x, _) -> x) $ readProcessWithExitCode "git" arg []


failIfNull :: String -> MaybeIO ()
failIfNull xs = when (null xs) (fail "null")


