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

import Object
import Shared

-- Git's "Documentation/pack-format.txt" was very helpful for writing this.

-- XXX fill these in from cache.h.
data PackObjectType = PackSimple ObjectType
                    | PackOfsDelta | PackRefDelta
                    deriving Show
-- The PackObjectType integer values are encoded in pack files.
decodePackObjectType :: Int -> Either String PackObjectType
decodePackObjectType 1 = return $ PackSimple TypeCommit
decodePackObjectType 2 = return $ PackSimple TypeTree
decodePackObjectType 3 = return $ PackSimple TypeBlob
decodePackObjectType 4 = return $ PackSimple TypeTag
-- 5 is "for future expansion", according to cache.h.
decodePackObjectType 6 = return $ PackOfsDelta
decodePackObjectType 7 = return $ PackRefDelta
decodePackObjectType n = throwError $ "bad object type: " ++ show n

packDataPath = (".git/objects/pack/" ++)

hexDump :: B.ByteString -> String
hexDump str =
  concatMap (printf "%02x ") $ B.unpack str

getPackEntry :: FilePath -> Word32 -> IOE RawObject
getPackEntry file offset = do
  mmap <- liftIO $ mmapFileByteString (packDataPath file ++ ".pack") Nothing
  let result = fst $ runGet readHeader mmap
  (signature, version, entry_count) <- ErrorT $ return result
  when (signature /= pack_signature) $
    throwError "bad pack signature"
  when (version /= 2) $
    throwError "bad pack version"

  let entry_raw = B.drop (fromIntegral offset) mmap
  (typint, raw) <- ErrorT $ return $ fst $ runGet (readObject entry_raw) entry_raw
  case decodePackObjectType typint of
    Left error -> throwError error
    Right (PackSimple typ) -> return (typ, raw)
    Right _ -> throwError "complicated object type"  -- XXX handle these.
  where
    pack_signature = 0x5041434b  -- "PACK"
    readHeader = do
      signature <- getWord32be
      version <- getWord32be
      entry_count <- getWord32be
      return (signature, version, entry_count)
    readObject :: B.ByteString -> Get (Int, BL.ByteString)
    readObject raw = do
      flag <- getByteAsWord32
      let msb = flag `shiftR` 7
      let typ = fromIntegral $ (flag `shiftR` 4) .&. 0x7
      let initial_size = flag .&. 0xF
      size <- if msb == 1
                then readVariableSize initial_size
                else return initial_size
      ofs <- bytesRead
      -- We rely on zlib to know when to stop decompressing.
      -- XXX does passing it the remainder of the buffer cause the mmap to
      -- read in the remainder of the file?
      let expanded = decompress (BL.fromChunks [B.drop ofs raw])
      return (typ, expanded)
    readVariableSize size = do
      byte <- getByteAsWord32
      let size' = size `shiftL` 7 + byte .&. 0x7F
      let msb = byte `shiftR` 7
      if msb == 1
        then readVariableSize size'
        else return size'
    getByteAsWord32 :: Get Word32
    getByteAsWord32 = liftM fromIntegral getWord8


dumpIndex file = do
  mmap <- mmapFileByteString (packDataPath file ++ ".idx") Nothing
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

getPackObject :: Hash -> IOE RawObject
getPackObject hash = do
  let file = "pack-429e6d8a6bcc03645c8b9286d8c38d12b37f3691"
  offset <- findInPackIndex file hash
  entry <- getPackEntry file offset
  return entry

-- |Get a list of all pack files (without a path or an extension).
findPackFiles :: IO [FilePath]
findPackFiles = do
  files <- getDirectoryContents (packDataPath "")
  print files
  return $ map dropIdxSuffix $ filter (".idx" `isSuffixOf`) files
  where
    dropIdxSuffix path = take (length path - length ".idx") path
