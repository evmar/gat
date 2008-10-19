module Commit where

import Control.Monad.Error
import Text.ParserCombinators.Parsec

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

type CommitParser a = GenParser Char Commit a

-- Parse a raw commit object from the datastore into a commit.
parseCommit :: String -> Either String Commit
parseCommit input =
  -- I'm unclear on whether the header lines are guaranteed to be in any
  -- order, so for simplicitly we allow them in any order.
  -- Parsec's permutation phrases require at most one of each instance,
  -- so we can't use them (commits can have multiple parents).  Instead,
  -- we just update a Commit object's fields as we parse.
  case runParser p_commit emptyCommit "" input of
    Left error -> throwError $ "error parsing commit: " ++ show error
    Right parse -> return parse
  where
  p_commit = do many p_header; newline; p_message; getState
  p_header = p_tree <|> p_parent <|> p_author <|> p_committer
  p_keyval str rest = do
    try $ (string str >> char ' ')
    rest
  p_tree = do
    hash <- p_keyval "tree" p_eol
    updateState (\commit -> commit { commit_tree=hash })
  p_parent = do
    hash <- p_keyval "parent" p_eol
    updateState (\commit -> commit { commit_parents=(commit_parents commit)++[hash] })
  p_author = do
    person <- p_keyval "author" p_eol
    updateState (\commit -> commit { commit_author=person })
  p_committer = do
    person <- p_keyval "committer" p_eol
    updateState (\commit -> commit { commit_committer=person })
  p_eol = anyChar `manyTill` newline
  p_message = do
    message <- many anyChar
    updateState (\commit -> commit { commit_message=message })
