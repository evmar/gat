module Log where

import Control.Monad.Error
import Text.PrettyPrint

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

-- | Driver for "gat log" -- display a log with various options set.
showLog :: LogOptions -> Hash -> GitM ()
showLog (LogOptions {logoptions_commitLimit=0}) hash = return ()
showLog opts hash = do
  commit <- getObject hash
  case commit of
    ObCommit commit -> do
      let opts' = opts { logoptions_commitLimit=logoptions_commitLimit opts - 1 }
      liftIO $ putStrLn $ showCommit hash commit
      case commit_parents commit of
        (parent:_) -> showLog opts' (Hash (fromHex parent))
        _ -> return ()
    _ -> fail $ "hash " ++ hashAsHex hash ++ " not a commit?"

-- | Show a single Commit in a form similar to "git log".
showCommit :: Hash -> Commit -> String
showCommit hash commit = render $
  text "Commit: " <+> text (hashAsHex hash) $+$
  -- text "Tree:   " <+> text (commit_tree commit) $+$
  text "Parents:" <+> hcat (map text (commit_parents commit)) $+$
  text "" $+$
  nest 4 (text (commit_message commit))
