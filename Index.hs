-- | The git \"index\" is a disk-based packed binary file that represents
-- a tree of files (complete with cached last-stat info).  It is used for
-- quick diffing\/status queries, to stage commits, and to hold extra data
-- during merges.

module Index (
    Index(..)
  , IndexEntry(..)
  , IndexTree(..)
  , loadIndex
) where

import Control.Monad
import Control.Monad.Error
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.ByteString.Internal (c2w, w2c)
import Data.Binary.Strict.Get
import Data.Bits
import Data.Word
import Text.Printf
import System.IO.MMap (mmapFileByteString, Mode(..))
import System.Posix.Types (EpochTime)

import FileMode
import Shared

-- Parse an integer out of a ByteString.
-- XXX is there really no better way to do this?
readInt :: B.ByteString -> Int
readInt str =
  case B8.readInt $ B8.pack $ map w2c $ B.unpack str of
    Just (int, _) -> int
    Nothing -> 0  -- XXX should we do something smarter here?

-- Parse a string up to a terminator and advance past the terminator.
readStringTo :: Word8 -> Get B.ByteString
readStringTo stop = do
  text <- spanOf (/= stop)
  skip 1
  return text

-- Like parsec's @many@: repeats a Get until the end of the input.
many :: Get a -> Get [a]
many get = do
  done <- isEmpty
  if done
    then return []
    else do
      x <- get
      xs <- many get
      return (x:xs)

-- | An IndexTree holds a a tree of hashes associated with an Index.
-- (XXX needs more investigation.)
data IndexTree = IndexTree [Hash] [IndexTree] deriving Show

-- | An Index is a collection of "IndexEntry"s along with an IndexTree
-- extension.  (XXX: is there ever more than one tree?  Needs more analysis.)
data Index = Index {
    in_entries :: [IndexEntry]
  , in_tree :: IndexTree
  } deriving Show

readIndex = do
  entrycount <- readHeader
  entries <- sequence (replicate entrycount readEntry)
  -- The index ends with a series of "extensions".
  -- So far, we've only seen an empty "tree" extension, so these aren't of much
  -- use yet.  But we parse it here just to verify it's as we expect.
  tree <- readExtension
  return $ Index { in_entries=entries, in_tree=tree }

readHeader :: Get Int
readHeader = do
  let cache_signature = 0x44495243  -- "DIRC"
  signature <- getWord32be
  version   <- getWord32be
  entries   <- getWord32be
  unless (signature == cache_signature) $
    fail "bad signature"
  unless (version == 2) $
    fail "bad version"
  return $ fromIntegral entries

-- | An IndexEntry is a single entry in an Index, representing a single
-- file along with its hash, mode, as well as stat()-related info used to
-- decide whether an on-disk file has changed.
data IndexEntry = IndexEntry {
    ie_ctime :: EpochTime
  , ie_mtime :: EpochTime
  -- TODO: add dev, ino
  , ie_mode :: GitFileMode
  , ie_realMode :: Word32  -- Temporary until I flesh out GitFileMode.
  -- TODO: add uid, gid  (?)
  , ie_size :: Int
  , ie_hash :: Hash
  , ie_flags :: Word8
  , ie_name :: String
  } deriving Show

readCacheTime :: Get EpochTime
readCacheTime = do
  sec <- getWord32be
  nsec <- getWord32be
  -- It appears nsec is always zero (?!).
  unless (nsec == 0) $
    fail "nsec in cache time is non-zero"
  return (fromIntegral sec)

readEntry :: Get IndexEntry
readEntry = do
  start <- bytesRead
  ctime <- readCacheTime
  mtime <- readCacheTime
  dev  <- getWord32be
  ino  <- getWord32be
  mode <- getWord32be
  uid  <- getWord32be
  gid  <- getWord32be
  size <- getWord32be
  hash <- liftM Hash $ getByteString 20
  flagsword <- getWord16be
  let namemask = 0xFFF
  let namelen = flagsword .&. namemask
  namebytes <-
    if namelen == namemask
      then fail "long name in index"
      else getByteString (fromIntegral namelen)
  -- TODO: parse flags.
  let flags = fromIntegral $ flagsword `shiftR` 12
  end <- bytesRead
  -- Need to read remaining pad bytes.
  --   (offsetof(struct cache_entry,name) + (len) + 8) & ~7
  -- It appears to waste 8 bytes if we're at an even multiple of 8?
  let padbytes = 8 - ((end - start) `mod` 8)
  skip padbytes
  let name = map w2c $ B.unpack namebytes
  return $ IndexEntry {
    ie_ctime=ctime, ie_mtime=mtime,
    ie_mode=modeFromInt (fromIntegral mode), ie_realMode=mode,
    ie_size=fromIntegral size,
    ie_hash=hash, ie_flags=flags, ie_name=name
    }

readExtension = do
  let ext_tree = 0x54524545  -- "TREE"
  name <- getWord32be
  size <- getWord32be
  unless (name == ext_tree) $ fail "expected TREE in extension"
  body <- readTree  -- XXX should be limited to end of extension, not input.
  return body

-- Like cache-tree.c:cache_tree_read().
readTree = do
  readStringTo 0  -- Skip initial NUL-terminated string.
  -- "%d %d\n" % (entrycount, subtreecount)
  entrycountstr <- readStringTo (c2w ' ')
  subtreecountstr <- readStringTo (c2w '\n')
  -- Note we only read zero or one hashes depending on the entry count.
  entries <- if readInt entrycountstr > 0
               then do str <- getByteString 20; return [Hash str]
               else return []
  subtrees <- sequence $ replicate (readInt subtreecountstr) $ do
    sub <- readTree
    -- XXX cache_tree_sub() here... unclear what it does.
    return sub
  return $ IndexTree entries subtrees

-- | Load and parse @.git\/index@.
loadIndex :: IO Index
loadIndex = do
  mmap <- mmapFileByteString ".git/index" Nothing
  -- Last 20 bytes are SHA1.
  let raw = B.take (B.length mmap - 20) mmap
  (Right result, rest) <- return $ runGet readIndex raw
  unless (B.length rest == 0) $
    fail $ "index had leftover unparsed data: " ++ show (result, rest)
  return result
