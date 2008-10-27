-- | Git commit object format parser and datatype.
module Commit (
    Commit(..)
  , parseCommit

  -- Exposed for testing.
  , searchBS
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BU
import Control.Monad.Error

import Shared

-- TODO:
--   Represent hashes as "Shared.Hash"es.
--   Parse author/committter properly.
--   Parse other fields (?).

-- | A single git commit.
data Commit = Commit {
    commit_tree      :: String    -- ^ Hash of this commit's tree object.
  , commit_parents   :: [String]  -- ^ Hash of this commit's parents.
  , commit_author    :: String    -- ^ Author of the commit.
  , commit_committer :: String    -- ^ Committer of the commit.
  , commit_message   :: B.ByteString  -- ^ Message associated with the commit.
} deriving Show
emptyCommit :: Commit
emptyCommit = Commit [] [] [] [] B.empty

bsToString = map BI.w2c . B.unpack

bsBreakAround :: Char -> B.ByteString -> (B.ByteString, B.ByteString)
bsBreakAround char str =
  let (before, after) = B.break (== BI.c2w char) str
  in (before, B.tail after)

-- | Search a ByteString for a substring, returning its offset.
-- Warning: naive search.  (Exposed for testing.)
searchBS :: B.ByteString -> B.ByteString -> Maybe Int
searchBS needle str = tryNext str where
  firstChar = B.head needle
  tryNext str = do
    ofs <- B.elemIndex firstChar str
    let substr = B.drop ofs str
    if needle `B.isPrefixOf` substr
      then return ofs
      else do
        ofs' <- tryNext (B.drop 1 substr)
        return (ofs + 1 + ofs')

-- | Parse a raw commit object bytes into a Commit.
parseCommit :: B.ByteString -> Either String Commit
parseCommit input = commit where
  commit = do
    case searchBS (makeBS "\n\n") input of
      Nothing -> fail "couldn't parse commit"
      Just ofs -> do
        let headers = parseHeaders (B.take ofs input)
        let message = B.drop (ofs+2) input
        let c = emptyCommit { commit_message=message }
        let c' = applyHeaders c headers
        return $ fixupParents c'
  fixupParents commit =
    commit { commit_parents=reverse (commit_parents commit) }

-- Parse a newline-separated list of headers into key,value pairs.
parseHeaders :: B.ByteString -> [(String, String)]
parseHeaders str = map splitHeader headerlines where
  headerlines = B.split (BI.c2w '\n') str
  splitHeader header =
    let (key, val) = bsBreakAround ' ' header
    in (bsToString key, bsToString val)

-- Apply a list of headers to a commit.
applyHeaders :: Commit -> [(String, String)] -> Commit
applyHeaders = foldl applyHeader where
  applyHeader commit (key, val)
    | key == "tree" = commit { commit_tree=val }
    | key == "parent" = commit { commit_parents=val:(commit_parents commit) }
    | key == "author" = commit { commit_author=val }
    | key == "committer" = commit { commit_committer=val }
    | otherwise = commit  -- XXX should we handle unparsed headers?

