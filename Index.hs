
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.ByteString.Internal (c2w, w2c)
import Data.Binary.Strict.Get
import Data.Bits
import Data.Word
import Text.Printf
import System.IO.MMap (mmapFileByteString, Mode(..))

newtype Hash = Hash B.ByteString

instance Show Hash where
  show (Hash bs) = asHex bs

asHex :: B.ByteString -> String
asHex = concatMap hex . B.unpack where
  hex c = printf "%02x" (c :: Word8)

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

readIndex = do
  entrycount <- readHeader
  entries <- sequence (replicate entrycount readEntry)
  ext <- many readExtension
  return (entries, ext)

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

data CacheEntry = CacheEntry {
  -- ctime, mtime
  -- dev, ino, mode
  -- uid, gid
    ce_size :: Int
  , ce_hash :: Hash
  --, ce_flags :: Word8
  , ce_name :: String
  } deriving Show

--readEntry :: Get (Word32, String, B.ByteString, Word16)
readEntry = do
  start <- bytesRead
  ctime1 <- getWord32be
  ctime2 <- getWord32be
  mtime1 <- getWord32be
  mtime2 <- getWord32be
  dev  <- getWord32be
  ino  <- getWord32be
  mode <- getWord32be
  uid  <- getWord32be
  gid  <- getWord32be
  size <- getWord32be
  hash <- liftM Hash $ getByteString 20
  flags <- getWord16be
  let namemask = 0xFFF
  let namelen = flags .&. namemask
  namebytes <-
    if namelen == namemask
      then fail "long name in index"
      else getByteString (fromIntegral namelen)
  end <- bytesRead
  -- Need to read remaining pad bytes.
  --   (offsetof(struct cache_entry,name) + (len) + 8) & ~7
  -- It appears to waste 8 bytes if we're at an even multiple of 8?
  let padbytes = 8 - ((end - start) `mod` 8)
  skip padbytes
  let name = map w2c $ B.unpack namebytes
  return $ CacheEntry {
    ce_size=fromIntegral size, ce_hash=hash, ce_name=name
    }

readExtension = do
  let ext_tree = 0x54524545  -- "TREE"
  name <- getWord32be
  size <- getWord32be
  unless (name == ext_tree) $ fail "expected TREE in extension"
  body <- readTree  -- XXX should be limited to end of extension, not input.
  return (name, size, body)

readTree = do
  readStringTo 0
  entrycountstr <- readStringTo (c2w ' ')
  subtreecountstr <- readStringTo (c2w '\n')
  return (readInt entrycountstr, readInt subtreecountstr)

main = do
  str <- mmapFileByteString ".git/index" Nothing
  let raw = B.take (B.length str - 20) str  -- sha1
  let x = runGet readIndex raw
  print x

