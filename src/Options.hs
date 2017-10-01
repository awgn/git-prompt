module Options
    (
      Options(..)
    , parseOptions
    ) where

import Options.Applicative
import Data.Semigroup ((<>), Semigroup(..))

-- import Control.Applicative

data Options = Options
    {
      runPath           :: Maybe String
    , themeColor        :: Maybe String
    , shortMode         :: Bool
    , version           :: Bool

    } deriving (Show)


parseOptions :: Parser Options
parseOptions = Options

     <$> optional (strOption ( long "path"
           <> short 'p'
           <> metavar "PATH"
           <> help "Specify the git-rository path ($PWD by default)"))

     <*> optional (strOption
            ( long "theme"
           <> short 't'
           <> metavar "COLOR"
           <> help "Specify the color theme"))

     <*> switch
            ( long "short"
           <> short 's'
           <> help "Use short mode")

     <*> switch
            ( long "version"
           <> short 'V'
           <> help "Print version")

