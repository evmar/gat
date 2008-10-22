-- | Some shared utilites.
module Shared (
    breakAround
  , firstTrue
  , ErrorOr, forceError
  , asHex
  , Hash(..), hashAsHex
  , isHashString
  , fromHex
  , splitMSB
  , strictifyBS, makeBS
  , trace
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Control.Monad.Error
import Data.Bits
import Data.Char
import Data.Word
import Debug.Trace
import Text.Printf

-- | An alias for @Either String a@, for things that may fail with a String.
type ErrorOr a = Either String a
-- | Force an ErrorOr to either a monad failure or the ok value.
forceError :: (MonadIO m) => ErrorOr a -> m a
forceError (Left err) = fail err
forceError (Right ok) = return ok

-- | A SHA-1 hash.
newtype Hash = Hash B.ByteString deriving Eq

instance Show Hash where
  show (Hash bs) = "[Hash " ++ asHex bs ++ "]"

-- | Dump a ByteString as a String of hexidecimal characters.
asHex :: B.ByteString -> String
asHex = concatMap hex . B.unpack where
  hex c = printf "%02x" (c :: Word8)
-- | Parse a string of hex into a ByteString.
fromHex :: String -> B.ByteString
fromHex = B.pack . bytes where
  bytes (x1:x2:rest) = parseHex x1 x2 : bytes rest
  bytes []           = []
  parseHex x1 x2 = fromIntegral (digitToInt x1 * 16 + digitToInt x2)

-- | A hash as a hex string.
hashAsHex :: Hash -> String
hashAsHex (Hash bs) = asHex bs

-- | Test whether a string looks like a hash (40 chars long, all hex).
isHashString :: String -> Bool
isHashString str = length str == 40 && all isHexDigit str

-- |Like @break@, but drops the matched item.
breakAround :: Eq a => (a -> Bool) -> [a] -> ([a], [a])
breakAround pred list = (before, after) where
  (before, rest) = break pred list
  after = case rest of
            (x:xs) | pred x -> xs
            _ -> after

-- |firstTrue takes a list of things to do and gives you back the first one
-- that produces a result.  (XXX this is probably the composition of some
-- other monadic operators -- which?)
firstTrue :: Monad m => [m (Maybe a)] -> m (Maybe a)
firstTrue []     = return Nothing
firstTrue (x:xs) = do
  test <- x
  case test of
    Just _ -> return test
    Nothing -> firstTrue xs

-- |Split a byte into a (Bool, Word8) pair that has the most significant bit
-- and the lower 7 bits.  This is a common pattern in Git bit-packing schemes.
splitMSB :: Word8 -> (Bool, Word8)
splitMSB byte = (msb, bits) where
  msb = (byte .&. 0x80) /= 0
  bits = byte .&. 0x7F

-- |Convert a ByteString.Lazy to a strict ByteString.
strictifyBS :: BL.ByteString -> B.ByteString
strictifyBS = B.concat . BL.toChunks

-- | Convert a String into a strict ByteString.
makeBS :: String -> B.ByteString
makeBS = B.pack . map (fromIntegral . fromEnum)
