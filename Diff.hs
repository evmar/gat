module Diff where

import Control.Monad
import Control.Monad.Error
import Data.Maybe
import Text.Printf
import System.Exit
import System.IO
import System.Posix.Files
import System.Process

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
  -- Here, git does a bunch of filtering/munging of the list of diffs to
  -- handle renames and command-line flags.
  -- Rely on an external diff command for now?
  liftIO $ print pairs
  diffPair (head pairs)
  return ()

hashFile :: FilePath -> IOE Hash
hashFile path = do
  -- FIXME: super-slow for now, will fix later.
  (output, exit) <- liftIO $ do
    content <- readFile path
    (inp,out,err,pid) <- runInteractiveCommand "sha1sum"
    hPutStr inp $ printf "blob %u\0" (length content)
    hPutStr inp $ content
    hClose inp
    output <- hGetContents out
    exit <- waitForProcess pid
    return (output, exit)
  case exit of
    ExitFailure n -> throwError $ printf "sha1sum '%s' failed: %d" path n
    ExitSuccess -> do
      return $ Hash (fromHex (take 40 output))

-- diff.c:2730
diffPair :: DiffEntry -> IOE String
diffPair (DiffEntry mhasha mhashb path) = do
  -- test if they're directories and skip
  hasha <- hashIfNecessary mhasha
  hashb <- hashIfNecessary mhashb
  let mode = 0 :: Int  -- FIXME
  if hasha /= hashb
    then liftIO $ printf "index %s..%s %06o\n" (shortHash hasha) (shortHash hashb) mode
    else return ()
  return ""

  where
    shortHash :: Hash -> String
    shortHash (Hash bs) = take 7 $ asHex bs
    hashIfNecessary (Just hash) = return hash
    hashIfNecessary Nothing     = hashFile path

