-- | A simple module for making our output paged, by cheating and just spawning
-- less.
module Pager (
  redirectThroughPager
) where

import Control.Monad.Trans
import System.IO
import System.Posix.IO
import System.Posix.Process
import System.Posix.Types

-- | Run an IO action with its output paged through less.
-- stdout is hosed after this point, so it's best to wrap your entire
-- program with redirectThroughLess.
redirectThroughPager :: MonadIO m => m a -> m a
redirectThroughPager action = do
  pid <- liftIO $ do
    (rd, wr) <- createPipe
    pid <- forkProcess $ do
      dupTo rd 0
      closeFd rd
      closeFd wr
      executeFile "less" True ["-FRX"] Nothing

    closeFd rd
    dupTo wr 1
    closeFd wr
    return pid

  result <- action

  liftIO $ do
    hFlush stdout
    closeFd 1

    getProcessStatus {- hang -} True {- untraced -} False pid
    return result

test = do
  redirectThroughPager $ do
    mapM_ (\x -> print ("line", x)) [1..100]
