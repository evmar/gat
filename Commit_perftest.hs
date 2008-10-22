import qualified Data.ByteString as B
import Control.Monad
import Microbench

import Commit

main = microbench "commit parsing" parseOneCommit where
  parseOneCommit = do
    text <- B.readFile "testdata/commit"
    let Right commit = parseCommit text
    unless (length (commit_parents commit) > 0) $
      fail "misparse"
    return ()
