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

import System.Process ( readProcessWithExitCode )
import System.FilePath ( takeFileName )
import System.Directory
    ( getCurrentDirectory, setCurrentDirectory )
import Control.Monad ( when )
import Control.Monad.Trans ( MonadIO(liftIO) )
import Control.Monad.Trans.Maybe ( MaybeT(..))
import Control.Arrow ( Arrow((&&&)) )

import qualified Control.Monad.Parallel as P
import Control.Applicative ( Alternative((<|>)) )
import Control.Exception ( bracket )
import Data.List ( sortBy, isPrefixOf, isInfixOf, groupBy, intercalate )

import Data.Function ( on )
import Data.List.Split ( splitOn )
import Data.Tuple.Select ( Sel2(sel2) )

import Colors
    ( magenta, blue, cyan, green, red, bold, reset, getColorByName )

type MaybeIO = MaybeT IO


mkPrompt :: Bool -> Maybe String -> Maybe FilePath -> IO String
mkPrompt short Nothing path =

    withPath path $ do
        promptList <- runMaybeT $ do
            [branch, descr] <- P.sequence [gitBranchName, gitDescribe]
            P.sequence $ [ gitBranchIcon
                          , boldS =<< pure branch
                          , sep "|" =<< gitCommitName branch descr
                          , sep "|" =<< gitStashCounter
                          , sep "|" =<< gitAheadIcon
                          , sep "|" =<< gitBehindIcon
                          , sep "|" =<< pure descr
                          , sep "|" =<< gitStatusIcon False
                          ] <> [ sep "|" =<< gitListFiles False | not short ]
        return $ maybe "" (\prompt -> "(" <> concat prompt <> ")") promptList

mkPrompt short (Just theme) path =
    withPath path $ do
        promptList <- runMaybeT $ do
            [branch, descr] <- P.sequence [gitBranchName, gitDescribe]
            P.sequence $ [ gitBranchIcon
                          , boldS =<< colorS theme =<< pure branch
                          , sep "|" =<< boldS =<< gitCommitName branch descr
                          , sep "|" =<< boldS =<< gitStashCounter
                          , sep "|" =<< boldS =<< gitAheadIcon
                          , sep "|" =<< boldS =<< gitBehindIcon
                          , sep "|" =<< pure descr
                          , sep "|" =<< gitStatusIcon True
                          ] <> [ sep "|" =<< gitListFiles True | not short]

        return $ maybe "" (\prompt -> bold <> "(" <> reset <> concat prompt <> bold <> ")" <> reset) promptList


withPath :: Maybe FilePath -> IO a -> IO a
withPath Nothing action = action
withPath (Just repo) action = getCurrentDirectory >>= (\pwd -> bracket
                                                         (setCurrentDirectory repo)
                                                         (\_ -> setCurrentDirectory pwd)
                                                         (const action))

isNotPrefixOf :: Eq a => [a] -> [a] -> Bool
isNotPrefixOf x y = not $ x `isPrefixOf` y
{-# INLINE isNotPrefixOf #-}


sep :: String -> String -> MaybeIO String
sep _ "" = return ""
sep s xs = return $ s <> xs
{-# INLINE sep #-}


boldS :: (Monad m) => String -> m String
boldS "" = return ""
boldS xs = return $ bold <> xs <> reset
{-# INLINE boldS #-}


colorS :: (Monad m) => String -> String -> m String
colorS _ "" = return ""
colorS color xs = return $ getColorByName color <> xs <> reset
{-# INLINE colorS #-}


gitBranchIcon :: MaybeIO String
gitBranchIcon = do
    l <- gitRevParse False
    r <- gitRevParse True
    if l == r
    then return "⟝ "
    else return "⎇ "

-- 1: gitBranchName


gitBranchName :: MaybeIO String
gitBranchName = gitBranchShow <|> gitDescribeExactMatch <|> gitRevParse False
{-# INLINE gitBranchName #-}


gitCommitName :: String -> String -> MaybeIO String
gitCommitName bname descr = do
    namerev <- gitNameRev
    if bname `isInfixOf` namerev || namerev `isInfixOf` bname || namerev `isInfixOf` descr
        then pure ""
        else pure namerev
{-# INLINE gitCommitName #-}


gitBranchShow :: MaybeIO String
gitBranchShow = do
    xs <- liftIO $ git ["branch", "--show"]
    MaybeT . pure $ if null xs then Nothing
                                 else Just (replace "\n" "" xs)
{-# INLINE gitBranchShow #-}


gitDescribeExactMatch :: MaybeIO String
gitDescribeExactMatch = do
    xs <- liftIO $ git ["describe", "--exact-match"]
    MaybeT . pure $ if null xs then Nothing
                                 else Just (replace "~" "↓" (init xs))


gitRevParse :: Bool -> MaybeIO String
gitRevParse origin = do
    xs <- liftIO $ git (args origin)
    MaybeT . pure $ if null xs then Nothing
                                 else case filter (/= '\n') (last $ splitOn "/" xs) of
                                           "HEAD" -> Just "⚠ "
                                           ys     -> Just ys
        where args :: Bool -> [String]
              args False = ["rev-parse", "--abbrev-ref", "HEAD"]
              args True  = ["rev-parse", "--abbrev-ref", "origin/HEAD"]


gitNameRev :: MaybeIO String
gitNameRev = do
    xs <- liftIO $ git ["name-rev", "--name-only", "HEAD"]
    MaybeT . pure $
        if null xs
            then Nothing
            else Just $ foldr (\(o,n) acc -> replace o n acc) (init xs) [("tags/",""),  ("~","↓"), ("remotes/origin/", "ᐲ "), ("remotes/", "")]

-- 2: gitDescribe

gitDescribe :: MaybeIO String
gitDescribe = liftIO (git ["describe", "--abbrev=8", "--always", "--tag", "--long"]) >>= \xs -> do
    failIfNull xs
    let ys = splitOn "-" (filter (/= '\n') xs)
        tag = intercalate "-" $ take (length ys - 2) ys
        [com, hash] = drop (length ys - 2) ys
    if com == "0"
        then pure $ bold <> tag <> reset <> " " <> hash
        else pure $ bold <> tag <> "▴" <> com <> " " <> hash


-- 3: gitAheadIcon

gitAheadIcon :: MaybeIO String
gitAheadIcon = do
    xs <- liftIO $ git ["rev-list", "--count", "HEAD@{upstream}..HEAD"]
    return (if null xs || read xs == (0 :: Integer)
               then ""
               else "↑" <> show(read xs :: Integer))


gitBehindIcon :: MaybeIO String
gitBehindIcon = do
    xs <- liftIO $ git ["rev-list", "--count", "HEAD..@{upstream}"]
    return (if null xs || read xs == (0 :: Integer)
               then ""
               else "↓" <> show(read xs :: Integer))

-- 4: gitStatusIcon

gitStatusIcon :: Bool -> MaybeIO String
gitStatusIcon color = liftIO $ mergeIcons . map (mkGitIcon color) . lines <$> git ["status", "--porcelain"]
{-# INLINE gitStatusIcon #-}


-- 5: gitStashCounter:

gitStashCounter:: MaybeIO String
gitStashCounter = do
    n <- liftIO $ length . lines <$> git ["stash", "list"]
    if n == 0 then return ""
              else return $ "≡" <> show n

-- 6: gitListFiles

takeString :: Int -> [String] -> [String]
takeString n xs | length xs <= n = xs
                | otherwise      = take n xs <> ["…"]
{-# INLINE takeString #-}


gitListFiles :: Bool -> MaybeIO String
gitListFiles bl = liftIO $ do
    ls <- lines <$> git ["status", "--porcelain"]
    let xs = filter ("??" `isNotPrefixOf`) ls
    let r = intercalate "," . takeString 4 . filter (not.null) . map (takeFileName . drop 3) $ xs
    return $ if not bl || null r
                then r
                else bold <> r <> reset

type Color = String

data GitIcon = GitIcon {
        _colorIcon :: Maybe Color
        , icon     :: String
    } deriving (Eq, Ord)


mergeIcons :: [GitIcon] -> String
mergeIcons = concatMap (renderIcon . (head &&& length)) . groupBy ((==) `on` icon) . sortBy (compare `on` icon) . filter (not.null.icon)
  where renderIcon :: (GitIcon, Int) -> String
        renderIcon (GitIcon Nothing xs, 1)      = xs
        renderIcon (GitIcon Nothing xs, n)      = xs <> show n
        renderIcon (GitIcon (Just color) xs, 1) = bold <> color <> xs <> reset
        renderIcon (GitIcon (Just color) xs, n) = bold <> color <> xs <> show n <> reset


mkGitIcon :: Bool -> String -> GitIcon
mkGitIcon c ('D':'D':_) =  GitIcon (c ?? magenta ) "╌"
mkGitIcon c ('A':'U':_) =  GitIcon (c ?? green   ) "✛"
mkGitIcon c ('U':'D':_) =  GitIcon (c ?? magenta ) "-"
mkGitIcon c ('U':'A':_) =  GitIcon (c ?? cyan    ) "+"
mkGitIcon c ('A':'A':_) =  GitIcon (c ?? green   ) "ǂ"
mkGitIcon c ('U':'U':_) =  GitIcon (c ?? blue    ) "☢"

mkGitIcon c (' ':'M':_) =  GitIcon (c ?? blue ) "±"
mkGitIcon c (_  :'D':_) =  GitIcon (c ?? red  ) "-"
mkGitIcon c ('M':' ':_) =  GitIcon (c ?? green) "⁕"
mkGitIcon c ('A':' ':_) =  GitIcon (c ?? green) "✛"
mkGitIcon c ('D':_  :_) =  GitIcon (c ?? red  ) "—"
mkGitIcon c ('R':_  :_) =  GitIcon (c ?? red  ) "ʀ"
mkGitIcon c ('C':_  :_) =  GitIcon (c ?? cyan ) "©"
mkGitIcon c ('M':_  :_) =  GitIcon (c ?? cyan ) "⁕"
mkGitIcon c ('A':_  :_) =  GitIcon (c ?? cyan ) "✛"
mkGitIcon c ('!':'!':_) =  GitIcon (c ?? reset) ""  -- ignored items..
mkGitIcon c ('?':'?':_) =  GitIcon (c ?? reset) ""  -- untracked files ...
mkGitIcon c  _          =  GitIcon (c ?? reset) ""


(??) :: Bool -> a -> Maybe a
True  ?? a = Just a
False ?? _ = Nothing
{-# INLINE (??) #-}


replace :: String -> String -> String -> String
replace x y xs =  intercalate y $ splitOn x xs
{-# INLINE replace #-}


git :: [String] -> IO String
git arg = sel2 <$> readProcessWithExitCode "git" arg []
{-# INLINE git #-}


failIfNull :: String -> MaybeIO ()
failIfNull xs = when (null xs) (fail "null")
{-# INLINE failIfNull #-}
