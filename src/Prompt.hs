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

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import Control.Applicative
import Data.Maybe
import Data.List
import Data.List.Split

import Colors
import Prompt.Git as Git


main :: IO ()
main = getArgs >>= dispatch

dispatch :: [String] -> IO ()
dispatch [xs]  =  Git.mkPrompt xs >>= putStr
dispatch _     =  error "git-prompt [blue|red|green|cyan|magenta|yellow|white]"


