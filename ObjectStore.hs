module ObjectStore (
    getObjectRaw, getObject
  , Object(..)
  , findTree
) where

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Codec.Compression.Zlib (decompress)
import Control.Monad
import Control.Monad.Error
import Data.Bits
import Data.ByteString.Internal (c2w, w2c)
import Data.Word
import System.FilePath

import Shared

data Object = Blob BL.ByteString | Commit [(String,String)] String
            | Tree (BL.ByteString, FilePath, Hash, BL.ByteString) deriving Show

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

-- |Parse a loose git blob, returning @(type, size, content)@.
parseHeader :: BL.ByteString -> Maybe (BL.ByteString, Int, BL.ByteString)
parseHeader header = do
  -- The header looks like "%s %ld\0".
  (objtype, header') <- splitAround (c2w ' ')  header
  (sizestr, rest)    <- splitAround (c2w '\0') header'
  (size, _) <- parseIntBL sizestr
  return (objtype, size, rest)

-- |Return the path to a loose object.
objectPath :: Hash -> FilePath
objectPath hash =
  let (before, after) = splitAt 2 (hashAsHex hash)
  in ".git/objects" </> before </> after

getObjectRaw :: Hash -> IOE (BL.ByteString, BL.ByteString)
getObjectRaw hash = do
  let path = objectPath hash
  raw <- liftIO $ BL.readFile path
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
  -- The normal format for loose objects is a compressed blob with a textual
  -- header.
  let uncompressed = decompress raw
  case parseHeader uncompressed of
    Just (objtype, size, raw) -> return (objtype, raw)
    Nothing -> throwError "error parsing object"

bsToString = map w2c . BL.unpack

getObject :: Hash -> IOE Object
getObject hash = do
  (objtype, raw) <- getObjectRaw hash
  case bsToString objtype of
    "blob" -> return $ Blob raw
    "tree" ->
      case parseTreeEntry raw of
        Just x -> return $ Tree x
        Nothing -> throwError "couldn't parse tree"
    "commit" -> let (headers, message) = parseCommit raw
                in return $ Commit headers message
    typ -> throwError $ "unknown object type: " ++ typ

breakAround :: Eq a => (a -> Bool) -> [a] -> ([a], [a])
breakAround pred list = (before, after) where
  (before, rest) = break pred list
  after = case rest of
            (x:xs) | pred x -> xs
            _ -> after

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

parseTreeEntry :: BL.ByteString -> Maybe (BL.ByteString, FilePath, Hash, BL.ByteString)
parseTreeEntry raw = do
  -- The header looks like "%s %ld\0".
  (mode, raw')  <- splitAround (c2w ' ')  raw
  (path, raw'') <- splitAround (c2w '\0') raw'
  let hash = B.pack $ BL.unpack $ BL.take 20 raw''
  return (mode, bsToString path, Hash hash, BL.drop 20 raw'')
