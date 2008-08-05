
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import Control.Monad
import Control.Monad.Error
import Data.Bits
import Data.ByteString.Internal (c2w, w2c)
import Data.Char
import Data.List
import Data.Word
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import Text.Printf
import Codec.Compression.Zlib (decompress)

type SHA1 = String
type IOE a = ErrorT String IO a

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

data Ref = RefObject SHA1 | RefSymbolic String deriving Show

isSHA1 :: String -> Bool
isSHA1 str = length str == 40 && all isHexDigit str

firstTrue :: [IO (Maybe a)] -> IO (Maybe a)
firstTrue []     = return Nothing
firstTrue (x:xs) = do
  test <- x
  case test of
    Just _ -> return test
    Nothing -> firstTrue xs

revParse :: String -> IOE Ref
revParse name = do
  symref <- liftIO $ firstTrue $ map testPath sympaths
  case symref of
    Just symref -> return (RefSymbolic symref)
    Nothing | isSHA1 name -> return (RefObject name)
    _ -> throwError $ "couldn't parse ref: " ++ name
  where
    testPath path = do
      ok <- doesFileExist (".git" </> path)
      return $ if ok then Just path
                     else Nothing
    -- List of paths to search from "git help rev-parse".
    prefixes = ["", "refs", "refs/tags", "refs/heads", "refs/remotes"]
    sympaths = map (</> name) prefixes ++ ["refs/remotes" </> name </> "HEAD"]

getObject :: SHA1 -> IOE (BL.ByteString, Int, BL.ByteString)
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

stripTrailingWhitespace :: String -> String
stripTrailingWhitespace = reverse . (dropWhile isSpace) . reverse

resolveRef :: String -> IOE SHA1
resolveRef symref = do
  content <- liftIO $ readFile (".git" </> symref)
  let ref = stripTrailingWhitespace content
  case stripPrefix "ref: " ref of
    Just target -> resolveRef target
    Nothing | isSHA1 ref -> return ref
    _ -> throwError $ "bad ref: " ++ ref

cmdRef :: String -> IOE ()
cmdRef name = do
  (RefSymbolic ref) <- revParse name
  sha1 <- resolveRef ref
  liftIO $ putStrLn sha1

cmdCat :: SHA1 -> IOE ()
cmdCat name = do
  ref <- revParse name
  sha1 <- case ref of
            RefSymbolic ref -> resolveRef ref
            RefObject obj -> return obj
  (objtype, size, content) <- getObject sha1
  liftIO $ BL.putStr content

main = do
  args <- getArgs
  --cmdCat (head args)
  --out <- revParse (head args)
  res <- runErrorT $ cmdCat (head args)
  case res of
    Left err -> hPutStr stderr err
    Right _ -> return ()
  --print out

