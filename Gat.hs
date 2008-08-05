
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Internal (c2w, w2c)
import Data.List
import Data.Word
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import Text.Printf
import Codec.Compression.Zlib (decompress)

type SHA1 = String

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
objectPath :: SHA1 -> FilePath
objectPath sha1 =
  let (before, after) = splitAt 2 sha1
  in ".git/objects" </> before </> after

data ObjectRef = RefObject SHA1 | RefSymbolic String deriving Show

revParse :: String -> IO (Maybe ObjectRef)
revParse rev = do
  symref <- firstTrue (doesFileExist . (".git" </>)) sympaths
  case symref of
    Just symref -> return (Just (RefSymbolic symref))
    Nothing -> return Nothing
  where
    firstTrue :: (a -> IO Bool) -> [a] -> IO (Maybe a)
    firstTrue test []     = return Nothing
    firstTrue test (x:xs) = do
      ok <- test x
      if ok then return (Just x)
            else firstTrue test xs
    prefixes = ["", "refs", "refs/tags", "refs/heads", "refs/remotes"]
    sympaths = map (</> rev) prefixes

cmdCat :: SHA1 -> IO ()
cmdCat sha1 = do
  let path = objectPath sha1
  compressed <- BL.readFile path
  let raw = decompress compressed
  case parseHeader raw of
    Just (objtype, size, content) -> BL.putStr content
    Nothing -> hPutStr stderr "error loading file"

main = do
  args <- getArgs
  --cmdCat (head args)
  parse <- revParse (head args)
  print parse

