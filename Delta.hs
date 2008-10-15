-- | The Delta module parses and applies the delta format found within Git
-- pack files.
--
-- See patch-delta.c in the git code.

module Delta (
    Delta(..)
  , readDelta
  , applyDelta
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Strict.Get
import Data.Bits
import Data.Word
import Control.Monad

import Shared (splitMSB)

-- | A Delta contains some metadata about what it's delta-ing and a sequence
-- of DeltaOp commands that describe the transformation.
data Delta = Delta {
    d_origSize :: Word32     -- ^ Original (pre-patch) data size.
  , d_resultSize :: Word32   -- ^ Resulting (post-patch) data size.
  , d_commands :: [DeltaOp]  -- ^ Delta commands.
  } deriving Show

-- | A DeltaOp is one operation contained within a Delta.
data DeltaOp = Copy Word32 Word32  -- ^ Copy from source at offset, length bytes.
             | Paste B.ByteString  -- ^ Paste raw data embedded in delta.
               deriving Show

-- | Parse a raw delta ByteString into a Delta.
readDelta :: Get Delta
readDelta = do
  orig_size <- readDeltaVarInt
  result_size <- readDeltaVarInt
  ops <- loopUntil isEmpty readOpCode
  return $ Delta orig_size result_size ops
  where
    loopUntil :: (Monad m) => m Bool -> m a -> m [a]
    loopUntil pred action = do
      done <- pred
      if not done
        then do x <- action
                xs <- loopUntil pred action
                return (x:xs)
        else return []

    readOpCode = do
      split <- liftM splitMSB getWord8
      case split of
        (True,  opcode) -> readCopy opcode
        (False, 0)      -> fail "zero opcode"
        (False, opcode) -> readPaste opcode

    readCopy :: Word8 -> Get DeltaOp
    readCopy opcode = do
      offset  <- readPacked (opcode .&. 0xF)
      rawsize <- readPacked (opcode `shiftR` 4)
      let size = if rawsize == 0 then 0x10000 else rawsize
      return $ Copy offset size

    readPaste :: Word8 -> Get DeltaOp
    readPaste length = do
      raw <- getByteString (fromIntegral length)
      return $ Paste raw

    -- Read a delta varint, given the length byte.
    -- This is super-ugly and I'm unhappy with it.  Hmm.
    readPacked :: Word8 -> Get Word32
    readPacked bits = do
      a <- if (bits .&. 0x1 /= 0)
             then getByteWord32
             else return 0
      b <- if (bits .&. 0x2 /= 0)
             then liftM (`shiftL` 8) getByteWord32
             else return 0
      c <- if (bits .&. 0x4 /= 0)
             then liftM (`shiftL` 16) getByteWord32
             else return 0
      d <- if (bits .&. 0x8 /= 0)
             then liftM (`shiftL` 24) getByteWord32
             else return 0
      return (a+b+c+d)

    getByteWord32 :: Get Word32
    getByteWord32 = do
      b <- getWord8
      return $ fromIntegral b

-- Read the varint format found in the delta header.
-- delta.h:get_delta_hdr_size().
readDeltaVarInt :: Get Word32
readDeltaVarInt = read 0 0 where
  read shift value = do
    byte <- getWord8
    let value' = value + (fromIntegral (byte .&. 0x7F)) `shiftL` shift
    if byte .&. 0x80 /= 0
      then read (shift+7) value'
      else return value'

-- Apply a Delta to a ByteString.
applyDelta :: B.ByteString -> Delta -> BL.ByteString
applyDelta src delta = BL.fromChunks substrs where
  substrs = map apply (d_commands delta)
  apply (Copy offset length) =
    B.take (fromIntegral length) $ B.drop (fromIntegral offset) src
  apply (Paste bytes) = bytes
