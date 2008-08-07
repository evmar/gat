module Diff where

import qualified Data.ByteString.Lazy as BL
import Control.Monad
import Control.Monad.Error
import Data.List
import Data.Maybe
import Text.Printf
import System.Directory
import System.Exit
import System.IO
import System.Posix.Files
import System.Posix.IO
import System.Process

import Index
import ObjectStore
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

data DiffItem = GitItem Hash | TreeItem FilePath (Maybe Hash) deriving Show
data DiffEntry = DiffEntry DiffItem DiffItem deriving Show

diffAgainstIndex :: Index -> IOE ()
diffAgainstIndex index = do
  allpairs <- forM (in_entries index) $ \entry -> do
    stat <- liftIO $ getFileStatus (ie_name entry)
    -- XXX handle stat failure as removal
    let changed = statDiffers entry stat
    liftIO $ print (ie_name entry, changed)
    if changed
      then return $ Just $ DiffEntry (GitItem (ie_hash entry)) (TreeItem (ie_name entry) Nothing)
      else return Nothing
  let pairs = catMaybes allpairs
  -- Here, git does a bunch of filtering/munging of the list of diffs to
  -- handle renames and command-line flags.
  liftIO $ print pairs
  mapM_ diffPair pairs
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

runFancyDiffCommand :: FilePath -> FilePath -> IO ExitCode
runFancyDiffCommand path1 path2 = do
  (_,out,_,pid) <- runInteractiveProcess "diff" ["-u", path1, path2]
                   Nothing Nothing
  difftext <- hGetContents out
  mapM_ (putStrLn . colorDiffLine) (lines difftext)
  exit <- waitForProcess pid
  return exit
  where
    col_reset = "\x1b[m"
    col_meta  = "\x1b[1m"
    col_old   = "\x1b[31m"
    col_new   = "\x1b[32m"
    col_frag  = "\x1b[36m"
    colorLine color line = color ++ line ++ col_reset
    prefixColors = [
        ("--- ", col_meta)
      , ("+++ ", col_meta)
      , ("-",   col_old)
      , ("+",   col_new)
      , ("@@ ",  col_frag)
      ]
    colorDiffLine line =
      case filter (\(pfx,_) -> pfx `isPrefixOf` line) prefixColors of
        ((_,color):_) -> color ++ line ++ col_reset
        -- TODO: trailing whitespace.
        _ -> line

-- diff.c:2730
diffPair :: DiffEntry -> IOE String
diffPair (DiffEntry item1 item2) = do
  -- XXX test if they're directories and skip
  (item1, hash1) <- itemWithHash item1
  (item2, hash2) <- itemWithHash item2
  let mode = 0 :: Int  -- FIXME
  when (hash1 /= hash2) $ do
    liftIO $ printf "index %s..%s %06o\n" (shortHash hash1) (shortHash hash2) mode
    withItemPath item1 $ \path1 -> do
    withItemPath item2 $ \path2 -> do
      exit <- liftIO $ runFancyDiffCommand path1 path2
      case exit of
        ExitSuccess -> return ()
        ExitFailure 1 -> return ()  -- diff returns this?
        ExitFailure n -> throwError $ printf "diff failed: %d" n
  return ""

  where
    shortHash :: Hash -> String
    shortHash (Hash bs) = take 7 $ asHex bs

    itemWithHash :: DiffItem -> IOE (DiffItem, Hash)
    itemWithHash item@(GitItem hash) = return (item, hash)
    itemWithHash item@(TreeItem path (Just hash)) = return (item, hash)
    itemWithHash (TreeItem path Nothing) = do
      hash <- hashFile path
      let item = TreeItem path (Just hash)
      return (item, hash)

    withItemPath :: DiffItem -> (FilePath -> IOE a) -> IOE a
    withItemPath (GitItem hash) use = do
      (_,_,contents) <- getObject hash
      path <- liftIO $ do
        (path, handle) <- openBinaryTempFile "/tmp" "gat-diff-"
        BL.hPut handle contents
        hClose handle
        return path
      result <- use path  -- XXX catch error
      liftIO $ removeFile path
      return result
    withItemPath (TreeItem path _) use = use path
