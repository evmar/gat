-- | ObjectStore represents the git repository's collection of objects,
-- mostly found under @.git\/objects@ as loose files or in pack files.
module ObjectStore (
    getObject, getRawObject
  , findTree

  -- Exposed for testing.
  , getLooseObject
) where

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Codec.Compression.Zlib (decompress)
import Control.Exception
import Control.Monad
import Control.Monad.Error
import Data.Bits
import Data.ByteString.Internal (c2w, w2c)
import Data.Char (ord)
import Data.Word
import System.FilePath

import Commit
import FileMode
import Pack
import Object
import Shared
import State

-- |@splitAround sep str@ finds @sep@ in @str@ and returns the before and after
-- parts.
splitAround :: Word8 -> BL.ByteString -> Maybe (BL.ByteString, BL.ByteString)
splitAround sep input = do
  pos <- BL.elemIndex sep input
  let (a, b) = BL.splitAt pos input
  return (a, BL.tail b)

-- |Parse an int out of a ByteString.Lazy.
-- Best I can figure out is to repack as a Char8.  :(
parseIntBL :: BL.ByteString -> Maybe (Int, BC.ByteString)
parseIntBL = BC.readInt . BC.pack . map w2c . BL.unpack

-- |Parse a loose git blob, returning @(type, content)@.
parseLoose :: BL.ByteString -> Either String RawObject
parseLoose loose = do
  -- The header looks like "%s %ld\0".
  let parse = do
      (typestr, loose') <- splitAround (c2w ' ')  loose
      (sizestr, rest)   <- splitAround (c2w '\0') loose'
      (size, _) <- parseIntBL sizestr  -- XXX Unused?
      return (typestr, rest)
  case parse of
    Nothing -> throwError $ "error parsing loose object header"
    Just (typestr, raw) -> do
      typ <- objectTypeFromString $ map w2c $ BL.unpack typestr
      return (typ, raw)

-- |Return the path to a loose object.
objectPath :: Hash -> FilePath
objectPath hash = ".git/objects" </> before </> after
  where (before, after) = splitAt 2 (hashAsHex hash)

-- | Get a \"loose\" (found in @.git\/objects\/@...) object.
getLooseObject :: FilePath -> IO (Maybe RawObject)
getLooseObject path = do
  compressed <-
    catchJust ioErrors (BL.readFile path >>= return . Just)
                       (\err -> return Nothing)
  case compressed of
    Nothing -> return Nothing
    Just compressed -> do
      checkHeader compressed
      -- The normal format for loose objects is a compressed blob with a textual
      -- header.
      let raw = decompress compressed
      case parseLoose raw of
        Left err -> fail err
        Right ok -> return (Just ok)
  where
    checkHeader raw = do
      -- There is an older format that put info in the first few bytes of the
      -- file.  Git uses the following check to verify it's not this older format.
      -- 1) First byte must be 0x78.
      -- 2) First 16-bit word (big-endian) divisible by 31.
      -- Grab the bytes as Word16s so the left shift works.
      let (byte1, byte2) = (fromIntegral $ BL.index raw 0,
                            fromIntegral $ BL.index raw 1) :: (Word16, Word16)
      let word = (byte1 `shiftL` 8) + byte2
      unless (byte1 == 0x78 && word `mod` 31 == 0) $
        fail "object appears to be in old loose format"

bsToString = map w2c . BL.unpack

-- |Fetch an object, from both the objects/../ dirs and one pack file.
-- TODO: multiple pack files, alternates, etc.
getRawObject :: Hash -> GitM RawObject
getRawObject hash = do
  obj <- getPackObject hash
  case obj of
    Just obj -> return obj
    Nothing -> do
      obj <- liftIO $ getLooseObject (objectPath hash)
      case obj of
        Just obj -> return obj
        Nothing -> fail $ "can't find object: " ++ hashAsHex hash

-- |Fetch an object, from both the objects/../ dirs and one pack file.
-- TODO: multiple pack files, alternates, etc.
getObject :: Hash -> GitM Object
getObject hash = do
  (objtype, raw) <- getRawObject hash
  case objtype of
    TypeBlob -> return $ Blob raw
    TypeTree -> do
      tree <- forceError (parseTree raw)
      return (ObTree tree)
    TypeCommit -> do
      commit <- forceError (parseCommit (strictifyBS raw))
      return (ObCommit commit)

-- | @findTree hash@ fetches objects, starting at @hash@, following commits
-- until it finds a Tree object.
findTree :: Hash -> GitM Tree
findTree hash = do
  obj <- getObject hash
  case obj of
    Blob _ -> fail "found blob while looking for tree"
    ObCommit commit -> findTree (Hash (fromHex (commit_tree commit)))
    ObTree tree -> return tree

type TreeEntry = (GitFileMode, FilePath, Hash)

-- Parse a raw tree object's bytes into an Object.
parseTree :: BL.ByteString -> ErrorOr Tree
parseTree raw | BL.null raw = return $ Tree []
              | otherwise   = do
  (entry, rest) <- parseTreeEntry raw
  (Tree xs) <- parseTree rest
  return $ Tree (entry:xs)

-- Parse a ByteString as an octal integer.
bsToOctal :: BL.ByteString -> ErrorOr Int
bsToOctal str = mapM digit (BL.unpack str) >>= return . foldl octal 0 where
  octal cur digit = cur * 8 + digit
  digit x =
    case fromIntegral x - ord '0' of
      value | value >= 0 && value <= 7 -> return value
      _ -> throwError $ "bad octal digit: " ++ show x

parseTreeEntry :: BL.ByteString -> Either String (TreeEntry, BL.ByteString)
parseTreeEntry raw = do
  let header = do
      -- The header looks like "%s %ld\0".
      (mode, raw')  <- splitAround (c2w ' ')  raw
      (path, raw'') <- splitAround (c2w '\0') raw'
      let (hash, rest) = BL.splitAt 20 raw''
      return (mode, bsToString path, Hash (strictifyBS hash), rest)
  case header of
    Just (modestr, path, hash, rest) -> do
      mode <- bsToOctal modestr
      return ((modeFromInt mode, path, hash), rest)
    Nothing -> throwError "error parsing tree entry"
