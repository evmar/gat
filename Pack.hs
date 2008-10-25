-- A git pack file efficiently stores a bunch of objects, compressed and
-- possibly deltafied against each other.
module Pack (
    getPackObject

  , dumpPackIndex

  -- * Exposed for testing
  , binarySearch
  , readDeltaOffset
) where

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
import System.Posix.Files (getFileStatus, modificationTime)
import System.IO.MMap (mmapFileByteString, Mode(..))
import System.Directory

import Delta
import Object
import Shared
import State

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

-- Convert a filename to a path to the pack file with that name.
packDataPath :: PackFile -> FilePath
packDataPath pack = (".git/objects/pack/" ++ pack_name pack)

-- Decompress gzipped data strictly.
decompressStrict :: B.ByteString -> B.ByteString
decompressStrict str = strictifyBS $ decompress $ BL.fromChunks [str]

-- Initialize the PackFile's fields (e.g. mmap) if necessary.
initPackFile :: PackFile -> IO (PackFile, B.ByteString)
initPackFile pack = do
  case pack_mmapPack pack of
    Just mmap -> do
      return (pack, mmap)
    Nothing -> do
      mmap <- mmapFileByteString (packDataPath pack ++ ".pack") Nothing
      (signature, version, entry_count) <-
        forceError $ fst $ runGet readPackHeader mmap
      when (signature /= pack_signature) $
        fail "bad pack signature"
      when (version /= 2) $
        fail "bad pack version"
      return $ (pack { pack_mmapPack=(Just mmap),
                       pack_entryCount=fromIntegral entry_count },
                mmap)
  where
  pack_signature = 0x5041434b  -- "PACK"
  readPackHeader = do
    signature <- getWord32be
    version <- getWord32be
    entry_count <- getWord32be
    return (signature, version, entry_count)

-- Get an entry from a pack file at a given byte offset.
getPackEntry :: PackFile -> Word32 -> IO (PackFile, RawObject)
getPackEntry pack offset = do
  (pack, mmap) <- initPackFile pack
  let entry_raw = B.drop (fromIntegral offset) mmap
  (Right (typint, size), rest) <- return $ runGet readObjectHeader entry_raw
  let body = B.take (fromIntegral size + 10000) rest
  case decodePackObjectType typint of
    Left error -> fail error
    Right (PackSimple typ) -> do
      -- We rely on zlib to know when to stop decompressing.
      -- XXX does passing it the remainder of the buffer cause the mmap to
      -- read in the remainder of the file?
      let expanded = decompress (BL.fromChunks [body])
      return (pack, (typ, expanded))
    Right PackOfsDelta -> do
      -- Get the offset to the delta base.
      (Right refoffset, compressed) <- return $ runGet readDeltaOffset rest
      -- Read the delta out of this object.
      (Right delta, _) <- return $ runGet readDelta (decompressStrict compressed)
      -- Read the base object.
      (pack, (basetype, baseraw)) <- getPackEntry pack (offset - refoffset)
      -- Apply the delta.
      let result = applyDelta (strictifyBS baseraw) delta
      unless (BL.length result == (fromIntegral $ d_resultSize delta)) $
        fail $ printf "error applying delta: expected %d, got %d bytes"
                      (d_resultSize delta) (BL.length result)
      return (pack, (basetype, result))
    Right x -> fail $ "complicated object type: " ++ show x  -- XXX handle these.
  where
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

-- Read the packed integer used in the PackOfsDelta header (indicating where
-- the delta data is).
readDeltaOffset = do
  (msb, bits) <- liftM splitMSB getWord8
  (if msb then overflow else return) (fromIntegral bits)
  where
    overflow :: Word32 -> Get Word32
    overflow offset = do
      (msb, bits) <- liftM splitMSB getWord8
      let offset' = ((offset + 1) `shiftL` 7) + fromIntegral bits
      (if msb then overflow else return) offset'

-- Get a byte as a Word32.
getByteAsWord32 :: Get Word32
getByteAsWord32 = liftM fromIntegral getWord8

-- Get the version of a pack index file.
getIndexVersion = do
  signature <- lookAhead getWord32be
  if signature /= magicValue
    then return 1  -- Assume old-format index.
    else do
      skip 4       -- Skip over the signature.
      getWord32be  -- Index version is next.
  where
    -- This magic value was chosen because it's invalid in all old-format
    -- (signatureless) index files.  See pack.h.
    magicValue = 0xff744f63

-- Dump a pack index file.
dumpPackIndex file = do
  mmap <- mmapFileByteString file Nothing
  let (stuffe,rest) = runGet get mmap
  stuff <- forceError stuffe
  print stuff
  where
    get = do
      ver <- getIndexVersion
      case ver of
        1 -> do
          v1 <- getV1
          return (1, Just v1)
        ver -> return (ver, Nothing)

    getV1 = do
      fanout <- sequence (replicate 256 getWord32be)
      entries <- sequence (replicate 10 getIndexEntryV1)
      return (fanout, entries)

    -- Parse an entry out of a pack index.
    getIndexEntryV1 :: Get (Word32, Hash)
    getIndexEntryV1 = do
      ofs <- getWord32be
      hash <- getByteString 20
      return (ofs, Hash hash)

binarySearch :: (Monad m, Ord a) => (Int, Int) -> (Int -> m a) -> a
             -> m (Maybe Int)
binarySearch (lo,hi) get target = search lo hi where
  search lo hi | lo == hi = return Nothing
  search lo hi = do
    let mid = (lo + hi) `div` 2
    try <- get mid
    case try `compare` target of
      EQ -> return (Just mid)
      LT -> search (mid+1) hi
      GT -> search lo mid

-- Look for a given hash in a given pack index.
findInPackIndex :: PackFile -> Hash -> IO (PackFile, Maybe Word32)
findInPackIndex pack hash@(Hash hashbytes) = do
  (pack, mmap) <-
    case pack_mmapIndex pack of
      Just mmap -> do
        return (pack, mmap)
      Nothing -> do
        mmap <- liftIO $ mmapFileByteString (packDataPath pack ++ ".idx") Nothing
        return (pack { pack_mmapIndex=Just mmap }, mmap)
  offset <- forceError $ fst $ runGet get mmap
  return (pack, offset)
  where
    get = do
      version <- getIndexVersion
      unless (version == 1 || version == 2) $
        fail $ "we don't handle index version: " ++ show version

      -- First 256 words are fanout:
      --   fanout[byte] = # hashes whose first byte <= byte
      -- We get lower and upper bounds for the range we need to search.
      bounds <- getSearchBounds hash
      object_count <- getFanOut 255
      skip (4 * 256)  -- Skip over fanout.

      case version of
        1 -> getV1 bounds
        2 -> getV2 bounds object_count

    getV1 (lower_bound, upper_bound) = do
      -- We have a range we should search.
      -- V1 format is a (4-byte offset, 20-byte sha-1) sorted list.
      target <- binarySearch (lower_bound, upper_bound)
          (\i -> lookAhead $ do skip ((4 + 20) * i); skip 4; getByteString 20) hashbytes
      case target of
        Nothing -> return Nothing
        Just idx -> do
          skip ((4 + 20) * idx)
          ofs <- getWord32be
          return (Just ofs)

    getV2 (lower_bound, upper_bound) object_count = do
      -- We have a range we should search.
      -- V2 format is a (20-byte sha-1) sorted list,
      -- followed by a CRC table and then a matching 4-byte offset list.
      target <- binarySearch (lower_bound, upper_bound)
          (\i -> lookAhead $ do skip (20*i); getByteString 20) hashbytes
      case target of
        Nothing -> return Nothing
        Just idx -> do
          skip (object_count * 20)  -- Skip SHA-1 list.
          skip (object_count * 4)   -- Skip CRC list.
          skip (idx * 4)            -- Skip to the entry we want.
          ofs <- getWord32be
          if ofs .&. 0x80000000 == 0
            then return (Just ofs)
            else fail $ "not yet implemented: v2 index file has 8-byte offset"

    -- Get the bounds of hash range we need to search.
    getSearchBounds (Hash hashbytes) = do
      let idx = fromIntegral (B.head hashbytes) :: Int
      lower_bound <-
        if idx > 0
          then getFanOut (idx-1)
          else return 0
      upper_bound <- getFanOut idx
      return (lower_bound, upper_bound)

    -- Get an entry from the fanout table.
    getFanOut idx = lookAhead $ do
      skip (4 * idx)
      count <- getWord32be
      return (fromIntegral count)

-- | Fetch an object, trying all pack files available.
getPackObject :: Hash -> GitM (Maybe RawObject)
getPackObject hash = do
  packs <- getPackState
  (packs, obj) <- liftIO $ tryPacks packs
  modify $ \state -> state { state_pack=PackState (Just packs) }
  return obj
  where
    tryPacks :: [PackFile] -> IO ([PackFile], Maybe RawObject)
    tryPacks (pack:rest) = do
      (pack, offset) <- findInPackIndex pack hash
      case offset of
        Nothing -> do
          (rest, obj) <- tryPacks rest
          return (pack:rest, obj)
        Just offset -> do
          (pack, obj) <- getPackEntry pack offset
          return (pack:rest, Just obj)
    tryPacks [] = return ([], Nothing)

getPackState :: GitM [PackFile]
getPackState = do
  (PackState packs) <- gets state_pack
  case packs of
    Just packs -> return packs
    Nothing -> do
      packs <- liftIO initPackFiles
      putPackState packs
      return packs

-- Get an ordered list of pack files.
initPackFiles :: IO [PackFile]
initPackFiles = do
  let packdir = ".git/objects/pack"
  files <- findPackFileNames packdir
  stats <- mapM (\f -> getFileStatus (packdir ++ "/" ++ f ++ ".idx")) files
  -- TODO: sort list also by locality once we support alternates db.
  let sortedFiles = map snd $ sortBy compareByMTime $ zip stats files
  return $ map (\name -> PackFile name Nothing Nothing 0) sortedFiles
  where
    compareByMTime (a,_) (b,_) =
      compare (modificationTime b) (modificationTime a)

-- Get a list of all pack files (without a path or an extension).
findPackFileNames :: FilePath -> IO [FilePath]
findPackFileNames packdir = do
  files <- getDirectoryContents packdir
  return $ map dropIdxSuffix $ filter (".idx" `isSuffixOf`) files
  where
    dropIdxSuffix path = take (length path - length ".idx") path
