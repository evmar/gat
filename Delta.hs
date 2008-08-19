
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Array.ST
import Data.Binary.Strict.Get
import Data.STRef
import Data.Bits
import Data.Word
import Control.Monad

import Pack

{-

runSTUArray :: Ix i => (forall s . ST s (STUArray s i e)) -> UArray i e
newListArray :: (MArray a e m, Ix i) => (i, i) -> [e] -> m (a i e)

-}
type ByteVec s = STUArray s Int Word8
type Offset = Word32

-- applyDelta :: B.ByteString -> B.ByteString -> B.ByteString
-- applyDelta src delta =
--   let outarray = runSTUArray $ do
--     offset <-

getByteWord32 :: Get Word32
getByteWord32 = do
  b <- getWord8
  return $ fromIntegral b

readOfsVarInt :: Get Offset
readOfsVarInt = read 0 where
  read value = do
    byte <- getWord8
    let value' = (value `shiftL` 7) + fromIntegral (byte .&. 0x7F)
    if byte .&. 0x80 /= 0
      then read value'
      else return value'

data Delta = Delta {
    d_origSize :: Word32     -- Original (pre-patch) data size.
  , d_resultSize :: Word32   -- Resulting (post-patch) data size.
  , d_commands :: [DeltaOp]  -- Delta commands.
  } deriving Show
data DeltaOp = Copy Word32 Word32  -- Copy from source at offset, length bytes.
             | Raw B.ByteString    -- Raw data embedded in delta.
               deriving Show
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
      opcode <- getWord8
      command <- case opcode of
        _ | opcode .&. 0x80 /= 0 ->
          readCopy (opcode .&. 0x7F)
        _ | opcode /= 0 ->
          readRaw opcode
        _ -> fail "zero opcode"
      return command
    readCopy :: Word8 -> Get DeltaOp
    readCopy opcode = do
      offset <- readPacked (opcode .&. 0xF)
      rawsize <- readPacked (opcode `shiftR` 4)
      let size = if rawsize == 0 then 0x10000 else rawsize
      return $ Copy offset size
    readRaw :: Word8 -> Get DeltaOp
    readRaw length = do
      raw <- getByteString (fromIntegral length)
      return $ Raw raw
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

readDeltaVarInt :: Get Word32
readDeltaVarInt = read 0 0 where
  read shift value = do
    byte <- getWord8
    let value' = value + (fromIntegral (byte .&. 0x7F)) `shiftL` shift
    if byte .&. 0x80 /= 0
      then read (shift+7) value'
      else return value'

main = do
  delta <- B.readFile "testdata/delta"
  putStrLn $ hexDump delta
  print $ runGet readDelta delta
