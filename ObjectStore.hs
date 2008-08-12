module ObjectStore (
    getObject
  , Object(..)
  , findTree
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
import Data.Word
import System.FilePath

import Pack
import Object
import Shared

-- |@splitAround sep str@ finds @sep@ in @str@ and returns the before and after
-- parts.
splitAround :: Word8 -> BL.ByteString -> Maybe (BL.ByteString, BL.ByteString)
splitAround sep input = do
  pos <- BL.elemIndex sep input
  let (a, b) = BL.splitAt pos input
  return (a, BL.tail b)

-- |Parse an int out of a ByteString.Lazy.
-- Best I can figure out is to repack as a Char8.
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
objectPath hash =
  let (before, after) = splitAt 2 (hashAsHex hash)
  in ".git/objects" </> before </> after

getLooseObject :: Hash -> IOE RawObject
getLooseObject hash = do
  let path = objectPath hash
  compressed <- liftIO $ BL.readFile path
  checkHeader compressed
  -- The normal format for loose objects is a compressed blob with a textual
  -- header.
  let raw = decompress compressed
  returnE $ parseLoose raw
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
        throwError "object appears to be in old loose format"

bsToString = map w2c . BL.unpack

-- |Like @catchJust ioErrors@, but working with IOE instead of IO.
catchIOErrors :: IOE a -> (IOError -> IOE a) -> IOE a
catchIOErrors action fallback =
  ErrorT $ catchJust ioErrors (runErrorT action) (runErrorT . fallback)

-- |Fetch an object, from both the objects/../ dirs and one pack file.
-- TODO: multiple pack files, alternates, etc.
getObject :: Hash -> IOE Object
getObject hash = do
  (objtype, raw) <- do
    getLooseObject hash `catchIOErrors` const (getPackObject hash)
  case objtype of
    TypeBlob -> return $ Blob raw
    TypeTree -> do
      entries <- ErrorT $ return $ parseTree raw
      return $ Tree entries
    TypeCommit -> let (headers, message) = parseCommit raw
                  in return $ Commit headers message

parseCommit :: BL.ByteString -> ([(String,String)], String)
parseCommit raw = (headers, message) where
  (headerlines, messagelines) = breakAround null $ lines (bsToString raw)
  headers = map (breakAround (== ' ')) headerlines
  -- XXX unlines loses whether there was a trailing newline.  do we care?
  message = unlines messagelines

findTree :: Hash -> IOE Object
findTree hash = do
  obj <- getObject hash
  case obj of
    Blob _ -> throwError "found blob while looking for tree"
    Commit headers _ ->
      case lookup "tree" headers of
        Just hash -> getObject (Hash (fromHex hash))
        Nothing -> throwError "no commit?"
    Tree _ -> return obj

parseTree :: BL.ByteString -> Either String [(String, FilePath, Hash)]
parseTree raw = do
  if BL.null raw
    then return []
    else
      case parseTreeEntry raw of
        Just (mode, path, hash, rest) -> do
          xs <- parseTree rest
          return ((mode,path,hash):xs)
        Nothing ->
          throwError "error parsing tree entry"

parseTreeEntry :: BL.ByteString -> Maybe (String, FilePath, Hash, BL.ByteString)
parseTreeEntry raw = do
  -- The header looks like "%s %ld\0".
  (mode, raw')  <- splitAround (c2w ' ')  raw
  (path, raw'') <- splitAround (c2w '\0') raw'
  let hash = B.concat $ BL.toChunks $ BL.take 20 raw''
  return (bsToString mode, bsToString path, Hash hash, BL.drop 20 raw'')
