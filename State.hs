{-# LANGUAGE GeneralizedNewtypeDeriving, CPP #-}

module State (
    PackFile(..), PackState(..)
  , GitM, newState, runGit, gets, modify
  , state_pack, putPackState
) where

import qualified Data.ByteString as B
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
}
instance Show PackFile where
  show pack = "[Pack " ++ pack_name pack
           ++ " index: " ++ mapped pack_mmapIndex ++ "]"
              where
                mapped getter = maybe "un" (const "") (getter pack) ++ "mapped"

