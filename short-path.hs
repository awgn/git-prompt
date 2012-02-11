-- git-prompt:  Nicola Bonelli <bonelli@antifork.org>
-- 
-- Add the following line to ~/.bashrc
--
-- export PS1='\u@\h \[\033[1;32m\]$(short-path)[\033[0m\]$(gitprompt)$ '

import System.Directory
import System.FilePath.Posix
import Data.List

main :: IO ()
main = do
       path <- getCurrentDirectory
       putStr $ shorten path 

shorten :: FilePath -> FilePath
shorten xs | length xs < (gl + gr + 1) = xs
           | otherwise = take gl xs ++ "..." ++ drop (len - gr) xs
           where len = length xs
                 gl = 10
                 gr = 30

