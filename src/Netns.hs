
module Netns (
   netNamespace
) where


import System.Process ( readProcessWithExitCode )
import Control.Monad.Trans ( MonadIO(liftIO) )
import Control.Monad.Trans.Maybe ( MaybeT(..))

import Data.Tuple.Select ( Sel2(sel2) )


type MaybeIO = MaybeT IO


netNamespace :: MaybeIO String
netNamespace = do
    xs <- liftIO $ ip ["netns", "identify"]
    MaybeT . pure $ case words xs of
         [] -> Nothing
         (x:_) -> Just x


ip :: [String] -> IO String
ip arg = sel2 <$> readProcessWithExitCode "ip" arg []
{-# INLINE ip #-}

