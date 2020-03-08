{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Options
    (
      Options(..)
    , parseOptions
    ) where

import Options.Applicative

-- import Control.Applicative

data Options = Options
    {
      runPath           :: Maybe String
    , themeColor        :: Maybe String
    , shortMode         :: Bool
    , version           :: Bool

    } deriving (Show)


parseOptions :: Parser Options
parseOptions = do 

     runPath <- optional (strOption ( long "path"
           <> short 'p'
           <> metavar "PATH"
           <> help "Specify the git-rository path ($PWD by default)"))

     themeColor <- optional (strOption
            ( long "theme"
           <> short 't'
           <> metavar "COLOR"
           <> help "Specify the color theme"))

     shortMode <- switch
            ( long "short"
           <> short 's'
           <> help "Use short mode")

     version <- switch
            ( long "version"
           <> short 'V'
           <> help "Print version")

     return  Options{..}
