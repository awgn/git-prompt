-- git-prompt:  Nicola Bonelli <bonelli@antifork.org>
-- 
-- Add the following line to ~/.bashrc
--
-- export PS1='\u@\h \[\033[1;32m\]$(short-path)[\033[0m\]$(gitprompt)$ '

import System.Process
import Data.List

main :: IO ()
main = do
       xs <- readProcessWithExitCode "git" ["status"] []
       let ls = lines $ snd' xs
       putStr $ prompt (branch ls) (icon ls) 
       where snd' ( _, xs, _ ) = xs

branch :: [String] -> String
branch [] = ""
branch (x:xs) |  "# On branch" `isPrefixOf` x = (words x) !! 3
              | otherwise = branch xs
        
icon :: [String] -> String
icon [] = ""
icon (x:xs) | "# Changes to be committed" `isPrefixOf` x = "!"
            | "# Changes not staged"      `isPrefixOf` x = "*"
            | "# Untracked files:"        `isPrefixOf` x = "+"
            | otherwise = icon xs

prompt :: String -> String -> String
prompt b i | null b = ""
           | otherwise = "[" ++ b ++ i ++ "]"
