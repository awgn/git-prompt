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

module Git ( mkPrompt ) where

import System.Process
import System.FilePath
import System.Directory
import System.IO (FilePath)

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Arrow

import qualified Control.Monad.Parallel as P

import Control.Applicative
import Control.Exception

import Data.List
import Data.Function
import Data.List.Split
import Data.Monoid

import Colors

type MaybeIO = MaybeT IO


mkPrompt :: Bool -> Maybe String -> Maybe FilePath -> IO String
mkPrompt short Nothing path =

    withPath path $ do
        promptList <- runMaybeT
            (P.sequence $ [ gitBranchName
                          , sepPrefix "|" =<< gitStashCounter
                          , sepPrefix "|" =<< gitAheadIcon
                          , sepPrefix "|" =<< gitStatusIcon False
                          , sepPrefix "|" =<< gitDescribe
                          ]
                        <> if not short
                            then [ return "|"
                                 , gitListFiles False
                                 ]
                            else [])

        return $ maybe "" (\prompt -> "(" <> concat prompt <> ")") promptList

mkPrompt short (Just theme) path =
    withPath path $ do
        promptList <- runMaybeT
            (P.sequence $ [ boldS =<< colorS theme =<< gitBranchName
                          , sepPrefix "|" =<< boldS =<< gitStashCounter
                          , sepPrefix "|" =<< boldS =<< gitAheadIcon
                          , sepPrefix "|" =<< gitStatusIcon True
                          , sepPrefix "|" =<< gitDescribe
                          ]
                        <> if not short
                            then [ return "|"
                                 , gitListFiles True
                                 ]
                            else [])

        return $ maybe "" (\prompt -> bold <> "(" <> reset <> concat prompt <> bold <> ")" <> reset) promptList


withPath :: Maybe FilePath -> IO a -> IO a
withPath Nothing action = action
withPath (Just repo) action = getCurrentDirectory >>= (\pwd -> bracket
                                                         (setCurrentDirectory repo)
                                                         (\_ -> setCurrentDirectory pwd)
                                                         (const action))


isNotPrefixOf :: Eq a => [a] -> [a] -> Bool
isNotPrefixOf x y = not $ x `isPrefixOf` y


sepPrefix :: String -> String -> MaybeIO String
sepPrefix _ "" = return ""
sepPrefix sep xs = return $ sep <> xs


boldS :: (Monad m) => String -> m String
boldS "" = return ""
boldS xs = return $ bold <> xs <> reset


colorS :: (Monad m) => String -> String -> m String
colorS _ "" = return ""
colorS color xs = return $ getColorByName color <> xs <> reset


-- 1: gitBranchName

gitBranchName :: MaybeIO String
gitBranchName = gitSymbolicRef <|> gitNameRev


gitSymbolicRef :: MaybeIO String
gitSymbolicRef = do
    xs <- liftIO $ git ["symbolic-ref", "HEAD"]
    MaybeT $ return $ if null xs then Nothing
                                 else Just (filter (/= '\n') (last $ splitOn "/" xs))


gitNameRev :: MaybeIO String
gitNameRev = do
    xs <- liftIO $ git ["name-rev", "--name-only", "HEAD"]
    MaybeT $ return $ if null xs then Nothing
                                 else Just (replace "~" "↓" (init xs))

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
               else "↑" <> show(read xs :: Integer))


-- 4: gitStatusIcon

gitStatusIcon :: Bool -> MaybeIO String
gitStatusIcon color = liftIO $ mergeIcons . map (mkGitIcon color) . lines <$> git ["status", "--porcelain"]


-- 5: gitStashCounter:

gitStashCounter:: MaybeIO String
gitStashCounter = do
    n <- liftIO $ length . lines <$> git ["stash", "list"]
    if n == 0 then return ""
              else return $ "≡" <> show n

-- 6: gitListFiles


gitListFiles :: Bool -> MaybeIO String
gitListFiles bl = liftIO $ do
    ls <- lines <$> git ["status", "--porcelain"]
    let xs = filter ("??" `isNotPrefixOf`) ls
    let r = intercalate "," . filter (not.null) . map (takeFileName . drop 3) $ xs
    let dots = if any ("??" `isPrefixOf`) ls
                then "…"
                else ""
    return $ if not bl || null r
                then r <> dots
                else bold <> r <> dots <> reset

type Color = String

data GitIcon = GitIcon {
        _colorIcon :: Maybe Color
        , icon     :: String
    } deriving (Eq, Ord)


mergeIcons :: [GitIcon] -> String
mergeIcons = concatMap (renderIcon . (head &&& length)) . groupBy ((==) `on` icon) . sortBy (compare `on` icon)
  where renderIcon :: (GitIcon, Int) -> String
        renderIcon (GitIcon Nothing xs, 1)      = xs
        renderIcon (GitIcon Nothing xs, n)      = xs <> show n
        renderIcon (GitIcon (Just color) xs, 1) = bold <> color <> xs <> reset
        renderIcon (GitIcon (Just color) xs, n) = bold <> color <> xs <> show n <> reset


mkGitIcon :: Bool -> String -> GitIcon
mkGitIcon c (' ':'M':_) =  GitIcon (c ?? blue ) "±"
mkGitIcon c (_  :'D':_) =  GitIcon (c ?? red  ) "-"
mkGitIcon c ('M':' ':_) =  GitIcon (c ?? green) "⁕"
mkGitIcon c ('A':' ':_) =  GitIcon (c ?? green) "✛"
mkGitIcon c ('M':_  :_) =  GitIcon (c ?? cyan ) "⁕"
mkGitIcon c ('A':_  :_) =  GitIcon (c ?? cyan ) "✛"
mkGitIcon c ('C':_  :_) =  GitIcon (c ?? cyan ) "•"
mkGitIcon c ('R':_  :_) =  GitIcon (c ?? red  ) "ʀ"
mkGitIcon c ('D':_  :_) =  GitIcon (c ?? red  ) "—"
mkGitIcon c ('?':'?':_) =  GitIcon (c ?? reset) "…"
mkGitIcon c  _          =  GitIcon (c ?? reset) ""


(??) :: Bool -> a -> Maybe a
True  ?? a = Just a
False ?? _ = Nothing


replace :: String -> String -> String -> String
replace x y xs =  intercalate y $ splitOn x xs


git :: [String] -> IO String
git arg = readProcessWithExitCode "git" arg [] >>= \(_,x,_) -> return x


failIfNull :: String -> MaybeIO ()
failIfNull xs = when (null xs) (fail "null")

