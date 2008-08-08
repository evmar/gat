
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import Control.Monad
import Control.Monad.Error
import Data.Char
import Data.List
import System.Directory
import System.FilePath
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Text.Printf

import Diff
import Index
import ObjectStore
import Shared

data Ref = RefObject Hash | RefSymbolic String deriving Show

isHashString :: String -> Bool
isHashString str = length str == 40 && all isHexDigit str

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
    Nothing | isHashString name -> return $ RefObject (Hash (fromHex name))
    _ -> throwError $ "couldn't parse ref: " ++ name
  where
    testPath path = do
      ok <- doesFileExist (".git" </> path)
      return $ if ok then Just path
                     else Nothing
    -- List of paths to search from "git help rev-parse".
    prefixes = ["", "refs", "refs/tags", "refs/heads", "refs/remotes"]
    sympaths = map (</> name) prefixes ++ ["refs/remotes" </> name </> "HEAD"]

stripTrailingWhitespace :: String -> String
stripTrailingWhitespace = reverse . (dropWhile isSpace) . reverse

resolveRef :: Ref -> IOE Hash
resolveRef (RefObject hash) = return hash
resolveRef (RefSymbolic symref) = do
  content <- liftIO $ readFile (".git" </> symref)
  let ref = stripTrailingWhitespace content
  case stripPrefix "ref: " ref of
    Just target -> resolveRef (RefSymbolic target)
    Nothing | isHashString ref -> return $ Hash (fromHex ref)
    _ -> throwError $ "bad ref: " ++ ref

cmdRef :: [String] -> IOE ()
cmdRef args = do
  unless (length args == 1) $
    throwError "'ref' takes one argument"
  let [name] = args
  ref <- revParse name
  hash <- resolveRef ref
  liftIO $ print hash

cmdCat :: [String] -> IOE ()
cmdCat args = do
  unless (length args == 1) $
    throwError "'cat' takes one argument"
  let [name] = args
  ref <- revParse name
  hash <- resolveRef ref
  --(objtype, raw) <- getObjectRaw hash
  --liftIO $ BL.putStr raw
  obj <- getObject hash
  liftIO $ print obj

cmdDumpIndex args = do
  unless (length args == 0) $
    throwError "'dump-index' takes no arguments"
  index <- loadIndex
  liftIO $ print index

cmdDiffIndex args = do
  unless (length args == 0) $
    throwError "'diff-index' takes no arguments"
  index <- loadIndex
  diffAgainstIndex index

cmdDiffTree args = do
  unless (length args == 1) $
    throwError "'diff-tree' takes one arguments"
  let name = head args
  ref <- revParse name
  liftIO $ print ref
  hash <- resolveRef ref
  liftIO $ print hash
  treehash <- findTree hash
  liftIO $ print treehash
  diffAgainstTree treehash

commands = [
    ("cat", cmdCat)
  , ("dump-index", cmdDumpIndex)
  , ("diff-index", cmdDiffIndex)
  , ("diff-tree", cmdDiffTree)
  , ("ref", cmdRef)
  ]

usage message = do
  hPutStrLn stderr $ "Error: " ++ message ++ "."
  hPutStrLn stderr $ "Commands:"
  forM_ commands $ \(name, _) ->
    hPutStrLn stderr $ "  " ++ name
  return (ExitFailure 1)

main = do
  argv <- getArgs
  exit <- do
    case argv of
      (cmd:args) -> do
        case lookup cmd commands of
          Just cmdfunc -> do
            res <- runErrorT $ cmdfunc args
            case res of
              Left err -> do hPutStrLn stderr err; return (ExitFailure 1)
              Right _ -> return ExitSuccess
          _ -> usage $ "unknown command: '" ++ cmd ++ "'"
      _ -> usage $ "must provide command"
  exitWith exit
