-- | The Diff module covers diffing: comparing files between trees, indexes,
-- and the working copy; running the diff commands and coloring the output;
-- hashing on-disk files.
module Diff (
    DiffPair, DiffItem(..)
  , diffAgainstIndex
  , diffAgainstTree
  , diffTrees
  , showDiff
) where

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

import Color
import FileMode
import Index
import Object
import ObjectStore
import Shared
import State

-- | One half of a diff: a file (or blob) to be diffed.
data DiffItem = DiffItem {
    di_mode :: GitFileMode
  , di_path :: FilePath
  , di_hash :: Maybe Hash
  , di_ondisk :: Bool  -- ^ Whether the path refers to an on-disk file.
} deriving Show
-- | A pair of potentially-differing blobs.
type DiffPair = (DiffItem, DiffItem)

noDiffItem = DiffItem (GitFileRegular False) "/dev/null" (Just emptyHash) False

-- Construct a DiffItem from a path by calling stat().
diffItemFromStat path = do
  mode <- modeFromPath path
  return $ DiffItem mode path Nothing True

-- Extract the hash field from a DiffItem, lazily filling it in if necessary.
itemWithHash :: DiffItem -> IO (DiffItem, Hash)
itemWithHash item@(DiffItem _ _ (Just hash) _) = return (item, hash)
itemWithHash item@(DiffItem _ name Nothing True) = do
  hash <- hashFileAsBlob name
  let item' = item { di_hash=Just hash }
  return (item', hash)

-- Compare cached index info against the result of stat().
-- Returns True if we think they differ (e.g. the file doesn't match the index).
statDiffers :: IndexEntry -> FileStatus -> Bool
statDiffers entry stat =
  -- XXX check all the fields on this sucker, read-cache.c:202
  let basic_change = or [
          ie_ctime entry /= statusChangeTime stat 
        , ie_mtime entry /= modificationTime stat 
        , ie_size entry /= fromIntegral (fileSize stat)
        ]
  in basic_change

-- | Diff the current working directory of files against an index.
diffAgainstIndex :: Index -> IO [DiffPair]
diffAgainstIndex index = do
  allpairs <- forM (in_entries index) $ \entry -> do
    stat <- liftIO $ getFileStatus (ie_name entry)
    -- XXX handle stat failure as removal
    let changed = statDiffers entry stat
    liftIO $ print (ie_name entry, changed)
    if changed
      then return $ Just $ (DiffItem (ie_mode entry) (ie_name entry) (Just (ie_hash entry)) False,
                            DiffItem (ie_mode entry) (ie_name entry) Nothing True)
      else return Nothing
  let pairs = catMaybes allpairs
  -- Here, git does a bunch of filtering/munging of the list of diffs to
  -- handle renames and command-line flags.
  filterM pairDiffers pairs

-- | Diff the current working directory of files against a Tree.
diffAgainstTree :: Tree -> IO [DiffPair]
diffAgainstTree (Tree entries) = do
  diffpairs <- liftIO $ mapM diffPairFromTreeEntry entries
  filterM pairDiffers diffpairs
  where
    diffPairFromTreeEntry (mode,path,hash) = do
      localitem <- diffItemFromStat path
      return (DiffItem mode path (Just hash) False, localitem)

-- | Diff one tree against another.
-- XXX this is totally broken because it assumes tree filenames line up.
diffTrees :: Tree -> Tree -> IO [DiffPair]
diffTrees (Tree e1) (Tree e2) = do
  filterM pairDiffers (compareTrees e1 e2)
  where
    compareTreeEntries :: TreeEntry -> TreeEntry -> (DiffPair, Ordering)
    compareTreeEntries e1@(mode1,path1,hash1) e2@(mode2,path2,hash2) =
      case path1 `compare` path2 of
        LT -> ((diffItem e1, noDiffItem), LT)
        GT -> ((noDiffItem, diffItem e2), GT)
        EQ -> ((diffItem e1, diffItem e2), EQ)

    compareTrees :: [TreeEntry] -> [TreeEntry] -> [DiffPair]
    compareTrees [] [] = []
    compareTrees t1@(e1:rest1) t2@(e2:rest2) =
      let (pair, cmp) = compareTreeEntries e1 e2 in
      pair : case cmp of
               LT -> compareTrees rest1 t2
               GT -> compareTrees t1 rest2
               EQ -> compareTrees rest1 rest2
    compareTrees es [] = map (\e -> (noDiffItem, diffItem e)) es
    compareTrees [] es = map (\e -> (diffItem e, noDiffItem)) es

    diffItem (mode,path,hash) = DiffItem mode path (Just hash) False

-- Hash a file as a git-style blob.
hashFileAsBlob :: FilePath -> IO Hash
hashFileAsBlob path = do
  -- FIXME: we use a pipe to sha1sum for now.
  -- Should probably use haskell-gcrypt's SHA1.
  (output, exit) <- do
    content <- readFile path
    (inp,out,err,pid) <- runInteractiveCommand "sha1sum"
    hPutStr inp $ printf "blob %u\0" (length content)
    hPutStr inp $ content
    hClose inp
    output <- hGetContents out
    exit <- waitForProcess pid

    return (output, exit)
  case exit of
    ExitFailure n -> fail $ printf "sha1sum '%s' failed: %d" path n
    ExitSuccess -> do
      return $ Hash (fromHex (take 40 output))

-- Run "diff" over two paths, coloring the output.
runFancyDiffCommand :: FilePath -> FilePath -> IO ExitCode
runFancyDiffCommand path1 path2 = do
  (_,out,_,pid) <- runInteractiveProcess "diff" ["-u", path1, path2]
                   Nothing Nothing
  difftext <- hGetContents out
  mapM_ (putStrLn . colorDiffLine) (lines difftext)
  exit <- waitForProcess pid
  return exit
  where
    prefixColors = [
        ("--- ", Bold)
      , ("+++ ", Bold)
      , ("-",    Red)
      , ("+",    Green)
      , ("@@ ",  Cyan)
      ]
    colorDiffLine line =
      case find (\(pfx,_) -> pfx `isPrefixOf` line) prefixColors of
        Just (_,color) -> coloredLine color line
        -- TODO: trailing whitespace.
        _ -> line

-- Return true if the items in the pair are different.
pairDiffers :: DiffPair -> IO Bool
pairDiffers (item1, item2) | di_mode item1 == di_mode item2
                          && di_mode item1 == GitFileDirectory = return False
pairDiffers (item1, item2) = do
  -- diff.c:2730
  (item1, hash1) <- itemWithHash item1
  (item2, hash2) <- itemWithHash item2
  -- XXX use modes properly
  return (hash1 /= hash2)

-- | Diff a pair of DiffItems, outputting git-diff-style diffs to stdout.
-- Should be filtered with pairDiffers first.
showDiff :: DiffPair -> GitM ()
showDiff (item1, item2) = do
  -- diff.c:2730
  (item1, hash1) <- liftIO $ itemWithHash item1
  (item2, hash2) <- liftIO $ itemWithHash item2
  -- XXX use modes properly
  when (hash1 /= hash2) $ do
    liftIO $ printf "index %s..%s %s\n" (shortHash hash1) (shortHash hash2) (modeToString $ di_mode item1)
    withItemPath item1 $ \path1 -> do
    withItemPath item2 $ \path2 -> do
      exit <- liftIO $ runFancyDiffCommand path1 path2
      case exit of
        ExitSuccess -> return ()
        ExitFailure 1 -> return ()  -- diff returns this?
        ExitFailure n -> fail $ printf "diff failed: %d" n

  where
    shortHash :: Hash -> String
    shortHash (Hash bs) = take 7 $ asHex bs

    -- Given a DiffItem, run an action over it as file, constructing a
    -- temporary path for the DiffItem if necessary.
    withItemPath :: DiffItem -> (FilePath -> GitM a) -> GitM a
    withItemPath (DiffItem _ _ (Just hash) False) action = do
      (Blob contents) <- getObject hash
      path <- liftIO $ do
        (path, handle) <- openBinaryTempFile "/tmp" "gat-diff-"
        BL.hPut handle contents
        hClose handle
        return path
      result <- action path  -- XXX catch error
      liftIO $ removeFile path
      return result
    withItemPath (DiffItem _ path _ True) action = action path
