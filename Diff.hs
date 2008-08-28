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

import FileMode
import Index
import Object
import ObjectStore
import Shared

-- One half of a diff: a thing to be diffed.
-- It doesn't make sense to construct a DiffItem with neither a hash nor a
-- FilePath, but we can't enforce that.
data DiffItem = DiffItem {
    di_mode :: GitFileMode
  , di_hash :: Maybe Hash
  , di_path :: Maybe FilePath
} deriving Show

diffItemFromHash mode hash = DiffItem mode (Just hash) Nothing
diffItemFromPath mode path = DiffItem mode Nothing (Just path)

diffItemFromStat path = do
  mode <- modeFromPath path
  return $ diffItemFromPath mode path

-- |Compare a cached index info against the result of stat().
-- Returns True if we think they differ.
statDiffers :: IndexEntry -> FileStatus -> Bool
statDiffers entry stat =
  -- XXX check all the fields on this sucker, read-cache.c:202
  let basic_change = or [
          ie_ctime entry /= statusChangeTime stat 
        , ie_mtime entry /= modificationTime stat 
        , ie_size entry /= fromIntegral (fileSize stat)
        ]
  in basic_change

-- |Diff the current tree of files against the index.
diffAgainstIndex :: Index -> IOE ()
diffAgainstIndex index = do
  allpairs <- forM (in_entries index) $ \entry -> do
    stat <- liftIO $ getFileStatus (ie_name entry)
    -- XXX handle stat failure as removal
    let changed = statDiffers entry stat
    liftIO $ print (ie_name entry, changed)
    if changed
      then return $ Just $ (diffItemFromHash (ie_mode entry) (ie_hash entry),
                            diffItemFromPath (ie_mode entry) (ie_name entry))
      else return Nothing
  let pairs = catMaybes allpairs
  -- Here, git does a bunch of filtering/munging of the list of diffs to
  -- handle renames and command-line flags.
  liftIO $ print pairs
  mapM_ diffPair pairs
  return ()

diffAgainstTree :: Tree -> IOE ()
diffAgainstTree (Tree entries) = do
  diffpairs <-liftIO $ mapM diffPairFromTreeEntry entries
  mapM_ diffPair diffpairs
  where
    diffPairFromTreeEntry (mode,path,hash) = do
      localitem <- diffItemFromStat path
      return (diffItemFromHash mode hash, localitem)

diffTrees :: Tree -> Tree -> IOE ()
diffTrees (Tree e1) (Tree e2) = do
  -- XXX this is totally broken because it assumes tree filenames line up.
  mapM_ diffPair (zipWith diffPairFromTrees e1 e2)
  where
    diffPairFromTrees (mode1,_,hash1) (mode2,_,hash2) =
      (diffItemFromHash mode1 hash1, diffItemFromHash mode2 hash2)

-- |Hash a file as a git-style blob.
hashFileAsBlob :: FilePath -> IOE Hash
hashFileAsBlob path = do
  -- FIXME: we use a pipe to sha1sum for now.
  -- Should probably use haskell-gcrypt's SHA1.
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

-- |Run "diff" over two paths, coloring the output.
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

-- |Diff a pair of DiffItems, outputting git-diff-style diffs to stdout.
diffPair :: (DiffItem, DiffItem) -> IOE ()
diffPair (item1, item2) | di_mode item1 == di_mode item2
                       && di_mode item1 == GitFileDirectory = return ()
diffPair (item1, item2) = do
  -- diff.c:2730
  (item1, hash1) <- itemWithHash item1
  (item2, hash2) <- itemWithHash item2
  -- XXX use modes properly
  when (hash1 /= hash2) $ do
    liftIO $ printf "index %s..%s %s\n" (shortHash hash1) (shortHash hash2) (modeToString $ di_mode item1)
    withItemPath item1 $ \path1 -> do
    withItemPath item2 $ \path2 -> do
      exit <- liftIO $ runFancyDiffCommand path1 path2
      case exit of
        ExitSuccess -> return ()
        ExitFailure 1 -> return ()  -- diff returns this?
        ExitFailure n -> throwError $ printf "diff failed: %d" n

  where
    shortHash :: Hash -> String
    shortHash (Hash bs) = take 7 $ asHex bs

    itemWithHash :: DiffItem -> IOE (DiffItem, Hash)
    itemWithHash item@(DiffItem _ (Just hash) _) = return (item, hash)
    itemWithHash (DiffItem mode Nothing (Just path)) = do
      hash <- hashFileAsBlob path
      let item = DiffItem mode (Just hash) (Just path)
      return (item, hash)

    withItemPath :: DiffItem -> (FilePath -> IOE a) -> IOE a
    withItemPath (DiffItem _ (Just hash) Nothing) use = do
      (Blob contents) <- getObject hash
      path <- liftIO $ do
        (path, handle) <- openBinaryTempFile "/tmp" "gat-diff-"
        BL.hPut handle contents
        hClose handle
        return path
      result <- use path  -- XXX catch error
      liftIO $ removeFile path
      return result
    withItemPath (DiffItem _ _ (Just path)) use = use path
