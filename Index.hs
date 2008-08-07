module Index where

import Control.Monad
import Control.Monad.Error
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.ByteString.Internal (c2w, w2c)
import Data.Binary.Strict.Get
import Data.Bits
import Data.Char
import Data.Word
import Text.Printf
import System.IO.MMap (mmapFileByteString, Mode(..))
import System.Posix.Types (EpochTime)

import Shared

newtype Hash = Hash B.ByteString deriving Eq

instance Show Hash where
  show (Hash bs) = "[Hash " ++ asHex bs ++ "]"

asHex :: B.ByteString -> String
asHex = concatMap hex . B.unpack where
  hex c = printf "%02x" (c :: Word8)
fromHex :: String -> B.ByteString
fromHex = B.pack . bytes where
  bytes (x1:x2:rest) = parseHex x1 x2 : bytes rest
  bytes []           = []
  parseHex x1 x2 = fromIntegral (digitToInt x1 * 16 + digitToInt x2)
hashAsHex :: Hash -> String
hashAsHex (Hash bs) = asHex bs

readInt :: B.ByteString -> Int
readInt str =
  case B8.readInt $ B8.pack $ map w2c $ B.unpack str of
    Just (int, _) -> int
    Nothing -> 0  -- XXX should we do something smarter here?

readStringTo :: Word8 -> Get B.ByteString
readStringTo stop = do
  text <- spanOf (/= stop)
  skip 1
  return text

-- |Like parsec's @many@: repeats a Get until the end of the input.
many :: Get a -> Get [a]
many get = do
  done <- isEmpty
  if done
    then return []
    else do
      x <- get
      xs <- many get
      return (x:xs)

data Index = Index {
    in_entries :: [IndexEntry]
  , in_tree :: [(Int,Int)]
  } deriving Show

readIndex = do
  entrycount <- readHeader
  entries <- sequence (replicate entrycount readEntry)
  exts <- many readExtension
  return $ Index { in_entries=entries, in_tree=exts }

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

data IndexEntry = IndexEntry {
    ie_ctime :: EpochTime
  , ie_mtime :: EpochTime
  -- dev, ino, mode
  -- uid, gid
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
    ie_size=fromIntegral size, ie_hash=hash, ie_flags=flags, ie_name=name
    }

readExtension = do
  let ext_tree = 0x54524545  -- "TREE"
  name <- getWord32be
  size <- getWord32be
  unless (name == ext_tree) $ fail "expected TREE in extension"
  body <- readTree  -- XXX should be limited to end of extension, not input.
  return body

readTree = do
  readStringTo 0  -- Skip initial NUL-terminated string.
  -- "%d %d\n" % (entrycount, subtreecount)
  entrycountstr <- readStringTo (c2w ' ')
  subtreecountstr <- readStringTo (c2w '\n')
  unless (readInt entrycountstr == -1 && readInt subtreecountstr == 0) $
    fail "tree had unhandled values"
  --forM [0..readInt subtreecountstr] $ do
  --  readTree
  return (readInt entrycountstr, readInt subtreecountstr)

loadIndex :: IOE Index
loadIndex = do
  mmap <- liftIO $ mmapFileByteString ".git/index" Nothing
  -- Last 20 bytes are SHA1.
  let raw = B.take (B.length mmap - 20) mmap
  let (result, rest) = runGet readIndex raw
  unless (B.length rest == 0) $
    throwError "index had leftover unparsed data"
  ErrorT (return result)
