module Shared where

import qualified Data.ByteString as B
import Control.Monad.Error
import Data.Bits
import Data.Char
import Data.Word
import Text.Printf

type IOE a = ErrorT String IO a
returnE :: Either String a -> IOE a
returnE = ErrorT . return

newtype Hash = Hash B.ByteString deriving Eq

instance Show Hash where
  show (Hash bs) = "[Hash " ++ asHex bs ++ "]"

asHex :: B.ByteString -> String
asHex = concatMap hex . B.unpack where
  hex c = printf "%02x" (c :: Word8)
fromHex :: String -> B.ByteString
fromHex = B.pack . bytes where
  bytes (x1:x2:rest) = parseHex x1 x2 : bytes rest
  bytes []           = []
  parseHex x1 x2 = fromIntegral (digitToInt x1 * 16 + digitToInt x2)
hashAsHex :: Hash -> String
hashAsHex (Hash bs) = asHex bs

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

