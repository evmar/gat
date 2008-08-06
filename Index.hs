
import Control.Monad
import qualified Data.ByteString as B
import Data.ByteString.Internal (w2c)
import Data.Binary.Strict.Get
import Data.Bits
import Data.Word
import Text.Printf
import System.IO.MMap (mmapFileByteString, Mode(..))

asHex :: B.ByteString -> String
asHex = concatMap hex . B.unpack where
  hex c = printf "%02x" (c :: Word8)

readHeader :: Get (Word32, Word32, Word32)
readHeader = do
  let cache_signature = 0x44495243  -- "DIRC"
  signature <- getWord32be
  version   <- getWord32be
  entries   <- getWord32be
  return (signature, version, entries)

data CacheEntry = CacheEntry {
  -- ctime, mtime
  -- dev, ino, mode
  -- uid, gid
    ce_size :: Int
  , ce_hash :: String
  , ce_flags :: Word8
  , ce_name :: String
  }

--readEntry :: Get (Word32, String, B.ByteString, Word16)
readEntry = do
  let entrybasesize = 8 + 8 + 6*4 + 20 + 2
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
  hash <- getByteString 20
  flags <- getWord16be
  let namemask = 0xFFF
  let namelen = flags .&. namemask
  namebytes <-
    if namelen == namemask
      then return $ B.pack [1]  -- XXX handle long names: more than 12 bits.
      else getByteString (fromIntegral namelen)
  end <- bytesRead
  -- Need to read remaining pad bytes.
  --   (offsetof(struct cache_entry,name) + (len) + 8) & ~7
  -- It appears to waste 8 bytes if we're at an even multiple of 8?
  let padbytes = 8 - ((end - start) `mod` 8)
  skip padbytes
  let name = map w2c $ B.unpack namebytes
  return (size, asHex hash, name, flags)

main = do
  str <- mmapFileByteString ".git/index" Nothing
  let x = flip runGet str $ do
            h@(_,_,len) <- readHeader
            entries <- sequence (replicate (fromIntegral len) readEntry)
            return (h, entries)
  print x

