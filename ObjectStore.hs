module ObjectStore (
  getObject
) where

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Codec.Compression.Zlib (decompress)
import Control.Monad
import Control.Monad.Error
import Data.Bits
import Data.ByteString.Internal (c2w, w2c)
import Data.Word
import System.FilePath

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

getObject :: Hash -> IOE (BL.ByteString, Int, BL.ByteString)
getObject sha1 = do
  let path = objectPath sha1
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
    Just parts -> return parts
    Nothing -> throwError "error parsing object"

