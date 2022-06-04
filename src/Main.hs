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

import qualified Git
import qualified Paths_GitPrompt as G

import Options ( parseOptions, Options(..) )
import Options.Applicative
    ( fullDesc, header, info, execParser, helper )

import Data.Version (showVersion)

import GHC.IO.Encoding ( utf8, setLocaleEncoding )
import Data.Maybe ( fromMaybe )

main :: IO ()
main = execParser opts >>= mainRun
    where opts = info (helper <*> parseOptions)
                      (fullDesc <> header "GitPrompt!")

mainRun :: Options -> IO ()
mainRun Options{..}
  | version         = putStrLn $ showVersion G.version
  | otherwise       = setLocaleEncoding utf8 *> Git.mkPrompt shortMode (fromMaybe "black" themeColor) runPath >>= putStrLn
