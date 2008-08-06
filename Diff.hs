module Diff where

import Control.Monad
import Control.Monad.Error
import Data.Maybe
import System.Posix.Files

import Index
import Shared

statDiffers :: IndexEntry -> FileStatus -> Bool
statDiffers entry stat =
  -- XXX check all the fields on this sucker, read-cache.c:202
  let basic_change = or [
          ie_ctime entry /= statusChangeTime stat 
        , ie_mtime entry /= modificationTime stat 
        , ie_size entry /= fromIntegral (fileSize stat)
        ]
  in basic_change

data DiffEntry = DiffEntry (Maybe Hash) (Maybe Hash) FilePath deriving Show
diffAgainstIndex :: Index -> IOE ()
diffAgainstIndex index = do
  allpairs <- forM (in_entries index) $ \entry -> do
    stat <- liftIO $ getFileStatus (ie_name entry)
    -- XXX handle stat failure as removal
    let changed = statDiffers entry stat
    liftIO $ print (ie_name entry, changed)
    if changed
      then return $ Just $ DiffEntry (Just (ie_hash entry)) Nothing (ie_name entry)
      else return Nothing
  let pairs = catMaybes allpairs
  liftIO $ print pairs
  return ()
  -- for each entry in index:
  --   stat same file
  --   special symlink/dir handling diff-lib.c:355
  --   if stat failed:
  --     mark as removed
  --     continue
  --   compare against stat cache
  --   if different:
  --     add to diff list
