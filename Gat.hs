
import qualified Data.ByteString.Lazy as BL
import Control.Monad.Error
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Text.Printf

import Commit
import Diff
import Index
import Log
import Object
import ObjectStore
import Refs
import RevParse
import Shared
import State

cmdRef :: [String] -> GitM ()
cmdRef args = do
  unless (length args == 1) $
    fail "'ref' takes one argument"
  let [name] = args
  hash <- resolveRev name
  liftIO $ print hash

cmdCat :: [String] -> GitM ()
cmdCat args = do
  let options = [
        Option "" ["raw"] (NoArg True) "dump raw object bytes"
        ]
  (raw, name) <-
    case getOpt Permute options args of
      (opts, [name], []) -> return (not (null opts), name)
      (_,    _,      []) ->
        fail "expect 1 argument: name of object to cat"
      (_,    _,    errs) ->
        fail $ concat errs ++ usageInfo "x" options
  hash <- resolveRev name
  case hash of
    Left err -> fail err
    Right hash ->
      if raw
        then do
          (typ, obj) <- getRawObject hash
          liftIO $ BL.putStr obj
        else getObject hash >>= liftIO . print

cmdDumpIndex :: [String] -> GitM ()
cmdDumpIndex args = liftIO $ do
  unless (length args == 0) $
    fail "'dump-index' takes no arguments"
  index <- loadIndex
  forM_ (in_entries index) $ \e -> do
    printf "%s %o %s\n" (show $ ie_mode e) (ie_realMode e) (ie_name e)
  print (in_tree index)

cmdDiffIndex :: [String] -> GitM ()
cmdDiffIndex args = do
  unless (length args == 0) $
    fail "'diff-index' takes no arguments"
  index <- liftIO loadIndex
  pairs <- liftIO $ diffAgainstIndex index
  mapM_ showDiff pairs

cmdDiff :: [String] -> GitM ()
cmdDiff args = do
  diffpairs <-
    case args of
      [] -> do
        tree <- revTree "HEAD"
        liftIO $ diffAgainstTree tree
      [name] -> do
        tree <- revTree name
        liftIO $ diffAgainstTree tree
      [name1,name2] -> do
        tree1 <- revTree name1
        tree2 <- revTree name2
        liftIO $ diffTrees tree1 tree2
  mapM_ showDiff diffpairs
  where
    revTree :: String -> GitM Tree
    revTree name = do
      hash <- resolveRev name >>= forceError
      findTree hash

cmdDumpTree args = do
  unless (length args == 1) $
    fail "expects one arg"
  tree <- resolveRev (head args) >>= forceError >>= findTree
  liftIO $ print tree

cmdLog :: [String] -> GitM ()
cmdLog args = do
  let options = [
        Option "n" []
          (ReqArg (\n opts -> opts { logoptions_commitLimit=(read n) }) "LIMIT")
          "limit number of commits to show"
        ]
  opts <-
    case getOpt Permute options args of
      (o, [], []) -> return (foldl (flip id) defaultLogOptions o)
      (_, _,  []) -> fail "expects no args"
      (_, _,  errs) ->
        fail $ concat errs ++ usageInfo "x" options
  (branch, commithash) <- liftIO $ (resolveRef "HEAD") >>= forceError
  showLog opts commithash

commands = [
    ("cat",  cmdCat)
  , ("dump-index", cmdDumpIndex)
  , ("diff-index", cmdDiffIndex)
  , ("diff", cmdDiff)
  , ("dump-tree", cmdDumpTree)
  , ("log", cmdLog)
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
            runGit $ cmdfunc args
            return ExitSuccess
          _ -> usage $ "unknown command: '" ++ cmd ++ "'"
      _ -> usage $ "must provide command"
  exitWith exit
