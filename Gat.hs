
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
import RevParse
import Shared

isHashString :: String -> Bool
isHashString str = length str == 40 && all isHexDigit str

firstTrue :: [IO (Maybe a)] -> IO (Maybe a)
firstTrue []     = return Nothing
firstTrue (x:xs) = do
  test <- x
  case test of
    Just _ -> return test
    Nothing -> firstTrue xs

-- |Take a name like "foo" and map it to a full name like "refs/heads/foo",
-- following the resolution rules found in the Git docs.
expandSymRef :: String -> IO (Maybe String)
expandSymRef name = firstTrue $ map testPath sympaths
  where
    testPath path = do
      ok <- doesFileExist (".git" </> path)
      return $ if ok then Just path
                     else Nothing
    -- List of paths to search from "git help rev-parse".
    prefixes = ["", "refs", "refs/tags", "refs/heads", "refs/remotes"]
    sympaths = map (</> name) prefixes ++ ["refs/remotes" </> name </> "HEAD"]

revToHash :: String -> IOE Hash
revToHash name = do
  rev <- ErrorT $ return $ parseRev name
  expandRev rev
  where
    expandRev (RevHash hex) = return $ Hash (fromHex hex)
    expandRev (RevParent nth rev) = do
      hash <- expandRev rev
      getNthParent nth hash
    expandRev (RevSymRef name) = do
      expanded <- liftIO $ expandSymRef name
      case expanded of
        Just name -> resolveSymbolic name
        Nothing -> throwError $ "couldn't find symref " ++ name

    getNthParent nth hash = do
      -- TODO: obey the "nth".
      obj <- getObject hash
      case obj of
        Commit headers message ->
          case lookup "parent" headers of
            Just hex -> return $ Hash (fromHex hex)
            Nothing -> throwError "commit has no parent"
        _ -> throwError "object is not a commit"

stripTrailingWhitespace :: String -> String
stripTrailingWhitespace = reverse . (dropWhile isSpace) . reverse

resolveSymbolic :: String -> IOE Hash
resolveSymbolic symref = do
  content <- liftIO $ readFile (".git" </> symref)
  let ref = stripTrailingWhitespace content
  case stripPrefix "ref: " ref of
    Just target -> resolveSymbolic target
    Nothing | isHashString ref -> return $ Hash (fromHex ref)
    _ -> throwError $ "bad ref: " ++ ref

cmdRef :: [String] -> IOE ()
cmdRef args = do
  unless (length args == 1) $
    throwError "'ref' takes one argument"
  let [name] = args
  hash <- revToHash name
  liftIO $ print hash

cmdCat :: [String] -> IOE ()
cmdCat args = do
  unless (length args == 1) $
    throwError "'cat' takes one argument"
  let [name] = args
  hash <- revToHash name
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
  hash <- revToHash name
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
