{-# LANGUAGE GeneralizedNewtypeDeriving, CPP #-}

module State (
    PackFile(..), PackState(..)
  , GitM, newState, runGit, gets, modify
  , CaughtMonadIO, gcatch
  , state_pack, putPackState
) where

import qualified Data.ByteString as B
import Control.Exception
import Control.Monad.State

data GitState = GitState {
  state_pack :: PackState
}
newState = GitState newPackState

newtype GitM a = GitM (StateT GitState IO a)
#ifndef __HADDOCK__
                 deriving (Monad, MonadIO, MonadState GitState)
#endif

runGit :: GitM a -> IO a
runGit (GitM st) = do
  (result, state') <- runStateT st newState
  return result

-- The following gnarlyness is just so we can catch exceptions from within
-- GitM.  Thanks to Oleg for the hints!
-- http://www.haskell.org/pipermail/haskell/2006-February/017547.html
class MonadIO m => CaughtMonadIO m where
  gcatch :: m a -> (Exception -> m a) -> m a
instance CaughtMonadIO IO where
  gcatch = Control.Exception.catch
instance CaughtMonadIO GitM where
  (GitM m) `gcatch` h =
    GitM $ StateT $ \s -> runStateT m s
             `gcatch` \e ->
               let (GitM st) = h e
               in runStateT st s

-- | Cached state: all open pack files and their mmaps.
newtype PackState = PackState (Maybe [PackFile])
-- | New uninitialized PackState.
newPackState = PackState Nothing
--getPackState = gets state_pack
putPackState ps = modify $ \state -> state { state_pack=PackState (Just ps) }

-- | A single pack file.  We keep these around after we open them.
data PackFile = PackFile {
    pack_name :: String                   -- ^ Pack file name, without suffix
  , pack_mmapIndex :: Maybe B.ByteString  -- ^ mmap of pack file index
  -- TODO: git maintains mmap windows into pack file.
  , pack_mmapPack  :: Maybe B.ByteString  -- ^ mmap of pack file data
  , pack_entryCount :: Int
}
instance Show PackFile where
  show pack = "[Pack " ++ pack_name pack
           ++ " index: " ++ mapped pack_mmapIndex ++ "]"
              where
                mapped getter = maybe "un" (const "") (getter pack) ++ "mapped"

