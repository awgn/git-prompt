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

module Colors (module Colors) where

import System.Console.ANSI

magenta, blue, red, cyan, green, bold, reset, yellow, white :: String

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

