module Pack where

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
import System.Directory

import Shared

-- Git's "Documentation/pack-format.txt" was very helpful for writing this.

-- XXX fill these in from cache.h.
data PackObjectType = PackCommit | PackTree | PackBlob | PackTag
                    | PackOfsDelta | PackRefDelta
                      deriving Show
instance Enum PackObjectType where
  toEnum 1 = PackCommit
  toEnum 2 = PackTree
  toEnum 3 = PackBlob
  toEnum 4 = PackTag
  toEnum 5 = PackOfsDelta
  toEnum 6 = PackRefDelta
  fromEnum = undefined

packDataPath = (".git/objects/pack/" ++)

getPackEntry :: FilePath -> Word32 -> IOE BL.ByteString
getPackEntry file offset = do
  mmap <- liftIO $ mmapFileByteString (packDataPath file ++ ".pack") Nothing
  let (result,body) = runGet readHeader mmap
  (signature, version, entry_count) <- ErrorT $ return result
  when (signature /= pack_signature) $
    throwError "bad pack signature"
  when (version /= 2) $
    throwError "bad pack version"

  liftIO $ printf "signature: %x. version: %d. entries: %d.\n"
                  signature version entry_count
  let (x,rest) = runGet (readObject body) (B.drop (fromIntegral offset) body)
  junk <- ErrorT $ return x
  liftIO $ print junk
  return $ BL.pack []
  where
    pack_signature = 0x5041434b  -- "PACK"
    readHeader = do
      signature <- getWord32be
      version <- getWord32be
      entry_count <- getWord32be
      return (signature, version, entry_count)
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

findInPackIndex :: FilePath -> Hash -> IOE Word32
findInPackIndex file hash@(Hash hashbytes) = do
  mmap <- liftIO $ mmapFileByteString (packDataPath file ++ ".idx") Nothing
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

getPackObject :: Hash -> IOE (Maybe BL.ByteString)
getPackObject hash = do
  let file = "pack-429e6d8a6bcc03645c8b9286d8c38d12b37f3691"
  offset <- findInPackIndex file hash
  liftIO $ print offset
  entry <- getPackEntry file offset
  liftIO $ print entry
  return (Just entry)

-- |Get a list of all pack files (without a path or an extension).
findPackFiles :: IO [FilePath]
findPackFiles = do
  files <- getDirectoryContents (packDataPath "")
  print files
  return $ map dropIdxSuffix $ filter (".idx" `isSuffixOf`) files
  where
    dropIdxSuffix path = take (length path - length ".idx") path

{-
main = do
  --let hash = Hash (fromHex "a9ecdab0bd5757cd90122f822fa802b4b95d34af")
  let hash = Hash (fromHex "4f8841f8a3370e0e34ce456e1bb52c0702affdbf")
  print ("seeking",hash)
  runErrorT $ do
    offset <- findInPackIndex hash
    liftIO $ print offset
    entry <- getPackEntry offset
    liftIO $ print entry
-}
