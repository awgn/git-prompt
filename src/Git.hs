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

{-# LANGUAGE MultiWayIf #-}

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

import Colors ( bold, reset, getColorByName )

type MaybeIO = MaybeT IO


mkPrompt :: Bool -> String -> Maybe FilePath -> IO String
mkPrompt short theme path =
    withPath path $ do
        promptList <- runMaybeT $ do
            [branch, descr] <- P.sequence [gitBranchName, gitDescribe]
            P.sequence $ [ gitBranchIcon
                         , gitStatusIcon theme
                         , sep "|" =<< boldS =<< gitStashCounter
                         , sep "|" =<< boldS =<< colorS theme =<< pure branch
                         , sep "|" =<< boldS =<< gitCommitName branch descr
                         , sep "|" =<< boldS =<< gitAheadIcon
                         , sep "|" =<< boldS =<< gitBehindIcon
                         , sep "|" =<< pure descr
                         ] <> [ sep "|" =<< gitListFiles (if short then 5 else 10)]

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
    if | l == "HEAD" -> pure "⚠ "
       | l == r      -> pure "⟝ "
       | otherwise   -> pure "⎇ "


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
    MaybeT . pure $
        if null xs
            then Nothing
            else Just $ filter (/= '\n') (last $ splitOn "/" xs)
    where args :: Bool -> [String]
          args False = ["rev-parse", "--abbrev-ref", "HEAD"]
          args True  = ["rev-parse", "--abbrev-ref", "origin/HEAD"]


gitNameRev :: MaybeIO String
gitNameRev = do
    xs <- liftIO $ git ["name-rev", "--name-only", "HEAD"]
    MaybeT . pure $
        if null xs
            then Nothing
            else Just $ foldr (\(o,n) acc -> replace o n acc) (init xs) [("tags/",""),  ("~","↓"), ("remotes/", "ʀ "), ("remotes/origin/", "ᐲ ")]


gitDescribe :: MaybeIO String
gitDescribe = liftIO (git ["describe", "--abbrev=8", "--always", "--tag", "--long"]) >>= \xs -> do
    failIfNull xs
    case splitOn "-" (filter (/= '\n') xs) of
        []                  ->  pure ""
        [tag]               ->  pure $ bold <> tag <> reset
        [tag, "0"]          ->  pure $ bold <> tag <> reset
        [tag, n]            ->  pure $ bold <> tag <> "▴" <> n <> reset
        [tag, "0", hash]    ->  pure $ bold <> tag <> reset <> "|" <> hash
        tag : n : hash : _  ->  pure $ bold <> tag <> "▴" <> n <> reset <> "|" <> hash


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


gitStatusIcon :: String -> MaybeIO String
gitStatusIcon theme = liftIO $ mergeIcons . map (mkGitIcon color) . lines <$> git ["status", "--porcelain"]
    where color = getColorByName  theme
{-# INLINE gitStatusIcon #-}


gitStashCounter:: MaybeIO String
gitStashCounter = do
    n <- liftIO $ length . lines <$> git ["stash", "list"]
    if n == 0 then return ""
              else return $ "≡" <> superscript (show n)


gitListFiles ::  Int -> MaybeIO String
gitListFiles n = liftIO $ do
    ls <- lines <$> git ["status", "--porcelain"]
    let xs = filter (\x -> "??" `isNotPrefixOf` x && "!!" `isNotPrefixOf` x) ls
    let r = intercalate "," . takeString n . filter (not.null) . map (takeFileName . drop 3) $ xs
    return $ bold <> r <> reset


takeString :: Int -> [String] -> [String]
takeString n xs | length xs <= n = xs
                | otherwise = take n xs <> ["…"]
{-# INLINE takeString #-}


data GitIcon = GitIcon {
        _colorIcon :: String
        , icon     :: String
    } deriving (Eq, Ord)


mergeIcons :: [GitIcon] -> String
mergeIcons = concatMap (renderIcon . (head &&& length)) . groupBy ((==) `on` icon) . sortBy (compare `on` icon) . filter (not.null.icon)
  where renderIcon :: (GitIcon, Int) -> String
        renderIcon (GitIcon color xs, 1) = color <> xs <> reset
        renderIcon (GitIcon color xs, n) = color <> xs <> superscript (show n) <> reset

superscript :: String -> String
superscript = (superscript' <$>)
{-# INLINE  superscript #-}

superscript' :: Char -> Char
superscript' '1' = '¹' -- 'ⁱ'
superscript' '2' = '²'
superscript' '3' = '³'
superscript' '4' = '⁴'
superscript' '5' = '⁵'
superscript' '6' = '⁶'
superscript' '7' = '⁷'
superscript' '8' = '⁸'
superscript' '9' = '⁹'
superscript' '0' = '⁰'
superscript' x = x

mkGitIcon :: String -> String -> GitIcon
mkGitIcon c (' ':'M':_) =  GitIcon c "•"
mkGitIcon c (' ':'D':_) =  GitIcon c "-"
mkGitIcon c (' ':'A':_) =  GitIcon c "+"
mkGitIcon c (' ':'C':_) =  GitIcon c "ᶜ"
mkGitIcon c (' ':'R':_) =  GitIcon c "ᵣ"

mkGitIcon c ('D':'D':_) =  GitIcon (bold <> c) "╌"
mkGitIcon c ('A':'U':_) =  GitIcon (bold <> c) "✛"
mkGitIcon c ('U':'D':_) =  GitIcon (bold <> c) "-"
mkGitIcon c ('U':'A':_) =  GitIcon (bold <> c) "⊕"
mkGitIcon c ('D':'U':_) =  GitIcon (bold <> c) "-"
mkGitIcon c ('A':'A':_) =  GitIcon (bold <> c) "ǂ"
mkGitIcon c ('U':'U':_) =  GitIcon (bold <> c) "☢"

mkGitIcon c ('M':'D':_) =  GitIcon (bold <> c) "✫"
mkGitIcon c ('M': _ :_) =  GitIcon (bold <> c) "★"
mkGitIcon c ('T': _ :_) =  GitIcon (bold <> c) "¿"
mkGitIcon c ('A':'D':_) =  GitIcon (bold <> c) "∓"
mkGitIcon c ('A':'M':_) =  GitIcon (bold <> c) "∔"
mkGitIcon c ('A': _ :_) =  GitIcon (bold <> c) "✛"
mkGitIcon c ('D':'A':_) =  GitIcon (bold <> c) "±"
mkGitIcon c ('D':'M':_) =  GitIcon (bold <> c) "߸"
mkGitIcon c ('D': _ :_) =  GitIcon (bold <> c) "—"
mkGitIcon c ('C': _ :_) =  GitIcon (bold <> c) "©"
mkGitIcon c ('R': _ :_) =  GitIcon (bold <> c) "ʀ"

mkGitIcon _ ('!':'!':_) =  GitIcon "" ""  -- ignored items...
mkGitIcon _ ('?':'?':_) =  GitIcon "" ""  -- untracked files ...
mkGitIcon _  _          =  GitIcon "" ""


replace :: String -> String -> String -> String
replace x y xs =  intercalate y $ splitOn x xs
{-# INLINE replace #-}


git :: [String] -> IO String
git arg = sel2 <$> readProcessWithExitCode "git" arg []
{-# INLINE git #-}


failIfNull :: String -> MaybeIO ()
failIfNull xs = when (null xs) (fail "null")
{-# INLINE failIfNull #-}
