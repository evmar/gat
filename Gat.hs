
import qualified Data.ByteString.Lazy as BL
import Control.Monad.Error
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

cmdRef :: [String] -> IOE ()
cmdRef args = do
  unless (length args == 1) $
    throwError "'ref' takes one argument"
  let [name] = args
  hash <- resolveRev name
  liftIO $ print hash

cmdCat :: [String] -> IOE ()
cmdCat args = do
  let options = [
        Option "" ["raw"] (NoArg True) "dump raw object bytes"
        ]
  (raw, name) <-
    case getOpt Permute options args of
      (opts, [name], []) -> return (not (null opts), name)
      (_,    _,      []) ->
        throwError "expect 1 argument: name of object to cat"
      (_,    _,    errs) ->
        throwError $ concat errs ++ usageInfo "x" options
  hash <- resolveRev name
  if raw
    then getRawObject hash >>= liftIO . BL.putStr . snd
    else getObject hash >>= liftIO . print

cmdDumpIndex args = do
  unless (length args == 0) $
    throwError "'dump-index' takes no arguments"
  index <- loadIndex
  liftIO $ forM_ (in_entries index) $ \e -> do
    printf "%s %o %s\n" (show $ ie_mode e) (ie_realMode e) (ie_name e)

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
    revTree name = resolveRev name >>= findTree

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
