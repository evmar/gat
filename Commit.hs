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

data Commit = Commit {
    commit_tree      :: String
  , commit_parents   :: [String]
  , commit_author    :: String
  , commit_committer :: String
  , commit_message   :: String
} deriving Show
emptyCommit :: Commit
emptyCommit = Commit [] [] [] [] []

bsToString = map BI.w2c . B.unpack

bsBreakAround :: Char -> B.ByteString -> (B.ByteString, B.ByteString)
bsBreakAround char str =
  let (before, after) = B.break (== BI.c2w char) str
  in (before, B.tail after)

-- Search a ByteString for a substring, returning its offset.
-- Warning: naive search.
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

-- Parse a raw commit object from the datastore into a commit.
parseCommit :: B.ByteString -> Either String Commit
parseCommit input = commit where
  commit = do
    case searchBS (makeBS "\n\n") input of
      Nothing -> fail "couldn't parse commit"
      Just ofs -> do
        let headers = parseHeaders (B.take ofs input)
        let message = bsToString $ B.drop (ofs+2) input
        return $ applyHeaders (emptyCommit { commit_message=message }) headers

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
    | key == "parent" = commit { commit_parents=words val }
    | key == "author" = commit { commit_author=val }
    | key == "committer" = commit { commit_committer=val }
    | otherwise = commit  -- XXX should we handle unparsed headers?

