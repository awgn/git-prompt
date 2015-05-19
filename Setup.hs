import Distribution.Simple
import Distribution.Simple.Setup(InstallFlags(..))
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Distribution.PackageDescription (PackageDescription(..))

import System.Environment
import System.Directory
import System.FilePath.Posix

main = defaultMainWithHooks $
        simpleUserHooks
        {
            postInst = printBanner
        }

printBanner :: Args -> InstallFlags -> PackageDescription -> LocalBuildInfo -> IO ()
printBanner _ _ _ _ = do
    putStrLn "Almost done! The last steps are to export COLUMNS and customize PS1 in your bashrc. Example: "
    putStrLn "    export COLUMNS"
    putStrLn "    PS1='\\u@\\h :: \\[\\033[1;32m\\]$(~/.cabal/bin/git-prompt path)\\[\\033[0m\\] $(~/.cabal/bin/git-prompt git)\\n-> '"

