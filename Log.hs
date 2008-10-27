-- | \"git log\"-like commit history output.
module Log (
    LogOptions(..), defaultLogOptions
  , printLog
) where

import qualified Data.ByteString as B
import Data.List
import Control.Monad.Error

import Color
import Commit
import Object
import ObjectStore
import Shared
import State

-- | Options for showLog.  Mostly a proof of concept for now.
data LogOptions = LogOptions {
    logoptions_commitLimit :: Int
}
-- | Default LogOptions settings.
defaultLogOptions = LogOptions (-1)

-- | Driver for \"gat log\" -- display a log with various options set.
printLog :: LogOptions -> Hash -> GitM ()
printLog (LogOptions {logoptions_commitLimit=0}) hash = return ()
printLog opts hash = do
  commit <- getObject hash
  case commit of
    ObCommit commit -> do
      let opts' = opts { logoptions_commitLimit=logoptions_commitLimit opts - 1 }
      liftIO $ printCommit hash commit
      case commit_parents commit of
        (parent:_) -> printLog opts' (Hash (fromHex parent))
        _ -> return ()
    _ -> fail $ "hash " ++ hashAsHex hash ++ " not a commit?"

-- | Pring a single Commit in a form similar to "git log".
printCommit :: Hash -> Commit -> IO ()
printCommit hash commit = do
  putStrLn $ coloredLine Yellow $ "commit " ++ hashAsHex hash
  when (length (commit_parents commit) > 1) $
    putStrLn $ "Parents: " ++ intercalate " " (commit_parents commit)
  putStrLn $ "Author: " ++ commit_author commit
  putStrLn ""
  printMessage (commit_message commit)

printMessage :: B.ByteString -> IO ()
printMessage msg = mapM_ printIndentedLine (B.split 10 msg) where
  printIndentedLine :: B.ByteString -> IO ()
  printIndentedLine str = do
    putStr "    "
    B.putStrLn str
