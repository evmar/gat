
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import Control.Monad
import Control.Monad.Error
import Data.List
import System.FilePath
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Text.Printf

import Diff
import Index
import ObjectStore
import Refs
import RevParse
import Shared

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
      (_,hash) <- resolveRef name
      return hash

    getNthParent nth hash = do
      -- TODO: obey the "nth".
      obj <- getObject hash
      case obj of
        Commit headers message ->
          case lookup "parent" headers of
            Just hex -> return $ Hash (fromHex hex)
            Nothing -> throwError "commit has no parent"
        _ -> throwError "object is not a commit"

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
  case obj of
    Blob raw -> liftIO $ BL.putStr raw
    x -> liftIO $ print x

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

cmdDiff args = do
  case args of
    [] -> do
      tree <- revTree "HEAD"
      diffAgainstTree tree
    [name] -> do
      tree <- revTree name
      diffAgainstTree tree
    [name1,name2] -> do
      tree1 <- revTree name1
      tree2 <- revTree name2
      diffTrees tree1 tree2
  where
    revTree name = revToHash name >>= findTree

commands = [
    ("cat",  cmdCat)
  , ("dump-index", cmdDumpIndex)
  , ("diff-index", cmdDiffIndex)
  , ("diff", cmdDiff)
  , ("ref",  cmdRef)
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
