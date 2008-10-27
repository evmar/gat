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

-- | Options for printLog.
data LogOptions = LogOptions {
    logoptions_limit :: Int              -- ^ Number of commits to show.
  , logoptions_filter :: Commit -> Bool  -- ^ Show only commits passing a test.
}
-- | Default LogOptions settings.
defaultLogOptions = LogOptions (-1) (const True)

-- | Driver for \"gat log\" -- display a log with various options set.
printLog :: LogOptions -> Hash -> GitM ()
printLog (LogOptions {logoptions_limit=0}) hash = return ()
printLog opts hash = do
  commit <- getObject hash
  commit <- case commit of
              ObCommit commit -> return commit
              _ -> fail $ "hash " ++ hashAsHex hash ++ " not a commit?"
  opts' <-
    if logoptions_filter opts commit
      then do
        liftIO $ printCommit hash commit
        return $ opts { logoptions_limit=logoptions_limit opts - 1 }
      else return opts
  case commit_parents commit of
    (parent:_) -> printLog opts' (Hash (fromHex parent))
    _ -> return ()

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
