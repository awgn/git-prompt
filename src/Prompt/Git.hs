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



module Prompt.Git ( mkPrompt ) where

import System.Process

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Parallel as P

import Control.Applicative
import Data.Maybe
import Data.List
import Data.List.Split

import Colors

type MaybeIO = MaybeT IO


mkPrompt :: String -> IO String
mkPrompt colorname = do
    promptList <- runMaybeT
        (P.sequence [ gitBranchName colorname
                    , return "|"
                    , gitDescribe
                    , return "|"
                    , gitStashCounter >>= sepPrompt
                    , gitAheadIcon
                    , gitStatusIcon
                    ])
    return $ maybe "" (\prompt ->  bold ++ "(" ++ reset ++ concat prompt ++ bold ++ ")" ++ reset) promptList

sepPrompt :: String -> MaybeIO String
sepPrompt xs = 
    return $ if null xs 
                then xs
                else xs ++ "|"


-- 1: gitBranchName

gitBranchName :: String -> MaybeIO String
gitBranchName colorname =
    gitSymbolicRef color <|> gitNameRev color
        where color = getColorByName colorname


gitSymbolicRef :: String -> MaybeIO String
gitSymbolicRef color = do
    xs <- liftIO $ git ["symbolic-ref", "HEAD"]
    MaybeT $ return $ if null xs then Nothing
                                 else Just (color ++ bold ++ filter (/= '\n') (last $ splitOn "/" xs) ++ reset)


gitNameRev :: String -> MaybeIO String
gitNameRev color = do
    xs <- liftIO $ git ["name-rev", "--name-only", "HEAD"]
    MaybeT $ return $ if null xs then Nothing
                                 else Just (replace "~" (reset ++ bold ++ "↓" ++ reset) (color ++ bold ++ init xs ++ reset))


-- 2: gitDescribe

gitDescribe :: MaybeIO String
gitDescribe = liftIO (git ["describe", "--abbrev=6", "--always", "--all", "--long"]) >>= \xs ->
    failIfNull xs >> return (filter (/= '\n') xs)



-- 3: gitAheadIcon

gitAheadIcon :: MaybeIO String
gitAheadIcon = do
    xs <- liftIO $ git ["rev-list", "--count", "HEAD@{upstream}..HEAD"]
    return (if null xs || read xs == (0 :: Integer)
               then ""
               else bold ++ "↑" ++ reset ++ show(read xs :: Integer))


-- 4: gitStatusIcon

gitStatusIcon :: MaybeIO String
gitStatusIcon = liftIO $ mergeIcons . map mkGitIcon . lines <$> git ["status", "--porcelain"]


-- 5: gitStashCounter:

gitStashCounter:: MaybeIO String
gitStashCounter = do
    n <- liftIO $ length . lines <$> git ["stash", "list"]
    if n == 0 then return ""
              else return $ bold ++ "≡" ++ show n ++ reset


type Color = String
type GitIcon = (Color, String)


mergeIcons :: [GitIcon] -> String
mergeIcons = concatMap (renderIcon . (\xs -> (head xs, length xs))) . group . sort
  where renderIcon :: (GitIcon, Int) -> String
        renderIcon ((color, xs), 1) = bold ++ color ++ xs ++ reset
        renderIcon ((color, xs), n) = bold ++ color ++ xs ++ show n ++ reset


mkGitIcon :: String -> GitIcon
mkGitIcon (' ':'M':_) =  (blue  , "±" )
mkGitIcon (_  :'D':_) =  (red   , "-" )
mkGitIcon ('M':' ':_) =  (green , "⁕" )
mkGitIcon ('A':' ':_) =  (green , "✛" )
mkGitIcon ('M':_  :_) =  (cyan  , "⁕" )
mkGitIcon ('A':_  :_) =  (cyan  , "✛" )
mkGitIcon ('C':_  :_) =  (cyan  , "•" )
mkGitIcon ('R':_  :_) =  (red   , "ʀ" )
mkGitIcon ('D':_  :_) =  (red   , "—" )
mkGitIcon ('?':'?':_) =  (reset , "…" )
mkGitIcon  _          =  (reset , "")


replace :: String -> String -> String -> String
replace x y xs =  intercalate y $ splitOn x xs


git :: [String] -> IO String
git arg = readProcessWithExitCode "git" arg [] >>= \(_,x,_) -> return x


failIfNull :: String -> MaybeIO ()
failIfNull xs = when (null xs) (fail "null")

