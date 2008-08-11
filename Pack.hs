import Control.Monad
import Control.Monad.Error
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Codec.Compression.Zlib (decompress)
--import Data.ByteString.Internal (c2w, w2c)
import Data.Binary.Strict.Get
import Data.List
import Data.Word
import Data.Bits
import Text.Printf
import System.IO.MMap (mmapFileByteString, Mode(..))

import Shared

-- Git's "Documentation/pack-format.txt" was very helpful for writing this.

data PackObjectType = PackCommit | PackTree | PackBlob | PackTag
                      deriving Show
instance Enum PackObjectType where
  toEnum 1 = PackCommit
  fromEnum PackCommit = 1

dumpPack :: IOE ()
dumpPack = do
  mmap <- liftIO $ mmapFileByteString "pack" Nothing
  let (result,body) = runGet readHeader mmap
  (signature, version, entries) <- ErrorT $ return result
  liftIO $ printf "signature: %x. version: %d. entries: %d.\n"
                  signature version entries
  let (x,rest) = runGet (readObject body) body
  junk <- ErrorT $ return x
  liftIO $ print junk
  where
    readHeader = do
      let pack_signature = 0x5041434b  -- "PACK"
      signature <- getWord32be
      version <- getWord32be
      entries <- getWord32be
      return (signature, version, entries)
    readObject raw = do
      flag <- getByteAsWord32
      let msb = flag `shiftR` 7
      let typ = toEnum $ fromIntegral $ (flag `shiftR` 4) .&. 0x7
      let initial_size = flag .&. 0xF
      size <- if msb == 1
                then readVariableSize initial_size
                else return initial_size
      ofs <- bytesRead
      -- We rely on zlib to know when to stop decompressing.
      -- XXX does passing it the remainder of the buffer cause the mmap to
      -- read in the remainder of the file?
      let expanded = decompress (BL.fromChunks [B.drop ofs raw])
      return (msb,typ :: PackObjectType,size, expanded)
    readVariableSize size = do
      byte <- getByteAsWord32
      let size' = size `shiftL` 7 + byte .&. 0x7F
      let msb = byte `shiftR` 7
      if msb == 1
        then readVariableSize size'
        else return size'
    getByteAsWord32 :: Get Word32
    getByteAsWord32 = liftM fromIntegral getWord8


dumpIndex = do
  mmap <- mmapFileByteString "idx" Nothing
  let (Right stuff,rest) = runGet get mmap
  print stuff
  where
    get = do
      signature <- lookAhead getWord32be
      case signature of
        0 -> getV1
        _ -> fail "unexpected signature in idx"
    getV1 = do
      fanout <- sequence (replicate 256 getWord32be)
      entries <- sequence (replicate 10 getIndexEntry)
      return (fanout, entries)

getIndexEntry = do
  ofs <- getWord32be
  hash <- getByteString 20
  return (ofs, Hash hash)

findInPackIndex :: Hash -> IOE Word32
findInPackIndex hash@(Hash hashbytes) = do
  mmap <- liftIO $ mmapFileByteString "idx" Nothing
  -- XXX check index version.
  let (ofs, rest) = runGet get mmap
  case ofs of
    Left error -> throwError error
    Right Nothing -> throwError "couldn't find hash in pack index"
    Right (Just ofs) -> return ofs
  where
    get = do
      -- First 256 words are fanout:
      --   fanout[byte] = # hashes whose first byte <= byte
      -- We get lower and upper bounds for the range we need to search.
      let idx = fromIntegral (B.head hashbytes) :: Int
      lower_bound <-
        if idx > 0
          then getFanOut (idx-1)
          else return 0
      upper_bound <- getFanOut idx
      skip (4 * 256)  -- Skip over fanout.

      -- Now we have a range we should search.
      -- XXX linear scan for now; do binary search later.
      -- Seek to initial position; index entries are 4+20 bytes.
      skip ((4 + 20) * lower_bound)
      entries <- sequence (replicate (upper_bound - lower_bound) getIndexEntry)
      return $ do
        (ofs,_) <- find (\(ofs, Hash h) -> h == hashbytes) entries
        return ofs

    getFanOut idx = lookAhead $ do
      skip (4 * idx)
      count <- getWord32be
      return (fromIntegral count)

--getPackEntry

main = do
  let hash = Hash (fromHex "a9ecdab0bd5757cd90122f822fa802b4b95d34af")
  --let hash = Hash (fromHex "4f8841f8a3370e0e34ce456e1bb52c0702affdbf")
  print ("seeking",hash)
  runErrorT $ do
    ofs <- findInPackIndex hash
    liftIO $ print ofs
    dumpPack
