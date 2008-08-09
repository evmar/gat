module Shared where

import qualified Data.ByteString as B
import Control.Monad.Error
import Data.Char
import Data.Word
import Text.Printf

type IOE a = ErrorT String IO a

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
