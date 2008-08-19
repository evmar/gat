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
decodePackObjectType :: Word32 -> Either String PackObjectType
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
  concatMap (printf "%02x ") $ take 20 $ B.unpack str

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
  let (header, rest) = runGet readObjectHeader entry_raw
  (typint, size) <- returnE header
  let body = B.take (fromIntegral size + 10000) rest
  case decodePackObjectType typint of
    Left error -> throwError error
    Right (PackSimple typ) -> do
      -- We rely on zlib to know when to stop decompressing.
      -- XXX does passing it the remainder of the buffer cause the mmap to
      -- read in the remainder of the file?
      let expanded = decompress (BL.fromChunks [body])
      return (typ, expanded)
    Right PackOfsDelta -> do
      let (Right ofs, r') = runGet readDeltaOffset rest
      --getPackEntry file (offset - ofs)
      return (TypeBlob, decompress (BL.fromChunks [r']))
    Right x -> throwError $ "complicated object type: " ++ show x  -- XXX handle these.
  where
    pack_signature = 0x5041434b  -- "PACK"
    readHeader = do
      signature <- getWord32be
      version <- getWord32be
      entry_count <- getWord32be
      return (signature, version, entry_count)
    readObjectHeader :: Get (Word32, Word32)
    readObjectHeader = do
      flag <- getByteAsWord32
      let msb = flag `shiftR` 7
      let typ = fromIntegral $ (flag `shiftR` 4) .&. 0x7
      let initial_size = flag .&. 0xF
      size <- if msb == 1
                then readVariableSize initial_size
                else return initial_size
      return (typ, size)
    readVariableSize size = do
      byte <- getByteAsWord32
      let size' = size `shiftL` 7 + byte .&. 0x7F
      let msb = byte `shiftR` 7
      if msb == 1
        then readVariableSize size'
        else return size'

readDeltaOffset = do
  (msb, bits) <- liftM splitMSB getWord8
  (if msb then overflow else return) (fromIntegral bits)
  where
    overflow :: Word32 -> Get Word32
    overflow offset = do
      (msb, bits) <- liftM splitMSB getWord8
      let offset' = ((offset + 1) `shiftL` 7) + fromIntegral bits
      (if msb then overflow else return) offset'

splitMSB :: Word8 -> (Bool, Word8)
splitMSB byte = (msb, bits) where
  msb = (byte .&. 0x80) /= 0
  bits = byte .&. 0x7F

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

findInPackIndex :: FilePath -> Hash -> IOE (Maybe Word32)
findInPackIndex file hash@(Hash hashbytes) = do
  mmap <- liftIO $ mmapFileByteString (packDataPath file ++ ".idx") Nothing
  -- XXX check index version.
  let (result, rest) = runGet get mmap
  returnE result
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
  packfiles <- liftIO $ findPackFiles
  entry <- firstTrue $ flip map packfiles $ \file -> do
    offset <- findInPackIndex file hash
    case offset of
      Nothing -> return Nothing
      Just offset -> do
        entry <- getPackEntry file offset
        return (Just entry)
  case entry of
    Just entry -> return entry
    Nothing -> throwError "couldn't find hash in pack files"

-- |Get a list of all pack files (without a path or an extension).
findPackFiles :: IO [FilePath]
findPackFiles = do
  files <- getDirectoryContents (packDataPath "")
  return $ map dropIdxSuffix $ filter (".idx" `isSuffixOf`) files
  where
    dropIdxSuffix path = take (length path - length ".idx") path
