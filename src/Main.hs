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

{-# LANGUAGE RecordWildCards #-}

module Main (module Main) where

-- import System.Environment

import qualified Git as G
import qualified Netns as N
import qualified Paths_GitPrompt as G

import Options ( parseOptions, Options(..) )
import Options.Applicative hiding (action, short)

import Data.Version (showVersion)

import GHC.IO.Encoding ( utf8, setLocaleEncoding )
import Data.Maybe ( fromMaybe )

import Control.Monad.Trans.Maybe ( MaybeT(..))

import qualified Control.Monad.Parallel as P
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import Control.Exception ( bracket )
import Colors ( bold, getColorByName, reset )

type MaybeIO = MaybeT IO

main :: IO ()
main = execParser opts >>= mainRun
    where opts = info (helper <*> parseOptions)
                      (fullDesc <> header "GitPrompt!")

mainRun :: Options -> IO ()
mainRun Options{..}
  | version         = putStrLn $ showVersion G.version
  | otherwise       = setLocaleEncoding utf8 *> mkPrompt showNetns shortMode (fromMaybe "black" themeColor) runPath >>= putStrLn


mkPrompt :: Bool -> Bool -> String -> Maybe FilePath -> IO String
mkPrompt netns short theme path =
    withPath path $ do
        promptNsList  <- if netns
            then runMaybeT $ P.sequence [sep "⁅" =<< N.netNamespace]
            else pure Nothing

        promptGitList <- runMaybeT $ do
            [branch, descr] <- P.sequence [G.gitBranchName, G.gitDescribe]

            P.sequence $ [ sep " " =<< G.gitBranchIcon
                         , sep "|" =<< G.gitStatusIcon theme
                         , sep "|" =<< boldS =<< G.gitStashCounter
                         , sep "|" =<< boldS =<< colorS theme =<< pure branch
                         , sep "|" =<< boldS =<< G.gitCommitName branch descr
                         , sep "|" =<< boldS =<< G.gitAheadIcon
                         , sep "|" =<< boldS =<< G.gitBehindIcon
                         , sep "|" =<< pure descr
                         ] <>
                         [ sep "|" =<< G.gitListFiles (if short then 5 else 10)]

        return $ maybe "" concat promptNsList <>
                 maybe "" concat promptGitList


withPath :: Maybe FilePath -> IO a -> IO a
withPath Nothing action = action
withPath (Just repo) action = getCurrentDirectory >>= (\pwd -> bracket
                                                         (setCurrentDirectory repo)
                                                         (\_ -> setCurrentDirectory pwd)
                                                         (const action))


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
