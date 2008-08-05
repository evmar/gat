
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import Control.Monad.Error
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

firstTrue :: [IO (Maybe a)] -> IO (Maybe a)
firstTrue []     = return Nothing
firstTrue (x:xs) = do
  test <- x
  case test of
    Just _ -> return test
    Nothing -> firstTrue xs

revParse :: String -> IO (Either String Ref)
revParse name = do
  symref <- firstTrue $ map testPath sympaths
  case symref of
    Just symref -> return (Right (RefSymbolic symref))
    Nothing ->
      -- XXX check sha1 before returning it.
      return $ (Right (RefObject name))
  where
    testPath path = do
      ok <- doesFileExist (".git" </> path)
      return $ if ok then Just path
                     else Nothing
    -- List of paths to search from "git help rev-parse".
    prefixes = ["", "refs", "refs/tags", "refs/heads", "refs/remotes"]
    sympaths = map (</> name) prefixes ++ ["refs/remotes" </> name </> "HEAD"]

getObject :: SHA1 -> IO (Either String (BL.ByteString, Int, BL.ByteString))
getObject sha1 = do
  let path = objectPath sha1
  compressed <- BL.readFile path
  let raw = decompress compressed
  case parseHeader raw of
    Just parts -> return (Right parts)
    Nothing -> return (Left "error parsing object")

stripTrailingWhitespace :: String -> String
stripTrailingWhitespace = reverse . (dropWhile isSpace) . reverse

resolveRef :: String -> IO SHA1
resolveRef symref = do
  content <- readFile (".git" </> symref)
  let ref = stripTrailingWhitespace content
  case stripPrefix "ref: " ref of
    Just target -> resolveRef target
    Nothing -> return ref  -- XXX check sha1.

cmdRef :: String -> IO ()
cmdRef name = do
  ref <- revParse name
  case ref of
    Right (RefSymbolic ref) -> do
      sha1 <- resolveRef ref
      putStrLn sha1
    Left err -> hPutStr stderr err

cmdCat :: SHA1 -> IO ()
cmdCat name = do
  res <- runErrorT work
  case res of
    Left err -> hPutStr stderr err
    Right _ -> return ()
  where
    work :: ErrorT String IO ()
    work = do
      ref <- ErrorT $ revParse name
      sha1 <- liftIO $ case ref of
                         RefSymbolic ref -> resolveRef ref
                         RefObject obj -> return obj
      (objtype, size, content) <- ErrorT $ getObject sha1
      liftIO $ BL.putStr content


main = do
  args <- getArgs
  --cmdCat (head args)
  --out <- revParse (head args)
  cmdCat (head args)
  --print out

