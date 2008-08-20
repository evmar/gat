module Refs (
    fullNameRef
  , readRef
  , resolveRef
) where

import Control.Exception
import Control.Monad.Error
import System.IO.Unsafe
import Data.Char
import Data.IORef
import Data.List
import System.FilePath
import System.Directory

import Shared

{-# NOINLINE packedRefs #-}
packedRefsState :: IORef (Maybe [(String, FilePath)])
packedRefsState = unsafePerformIO $ newIORef Nothing

-- |Get the packed references, if any.  Caches the result the first time
-- and returns the cached result in the future.
packedRefs :: IO [(String, FilePath)]
packedRefs = do
  ref <- readIORef packedRefsState
  case ref of
    Just refs -> return refs
    Nothing -> do
      content <- try $ readFile ".git/packed-refs"
      let packed = case content of
                     Left exn -> []
                     Right ok -> map parseLine (refLines ok)
      writeIORef packedRefsState (Just packed)
      return packed
  where
    -- XXX Git parses the comment header and finds the "peeled" attribute.
    -- refs.c:read_packed_refs
    refLines content = filter (not . ("#" `isPrefixOf`)) (lines content)
    parseLine line = breakAround (== ' ') line

stripWhitespace :: String -> String
stripWhitespace = reverse . dropSpace . reverse . dropSpace where
  dropSpace = dropWhile isSpace

-- |Take a name like "foo" and map it to a full name like "refs/heads/foo",
-- following the resolution rules found in the Git docs.
fullNameRef :: String -> IO (Maybe String)
fullNameRef name = firstTrue $ map testPath sympaths
  where
    testPath path = do
      found <- testRef path
      if found
        then return $ Just path
        else return Nothing

    testRef :: FilePath -> IO Bool
    testRef path = do
      found <- doesFileExist (".git" </> path)
      if found
        then return True
        else do
          packed <- packedRefs
          return $ any (\(_, p) -> p == path) packed

    -- List of paths to search from "git help rev-parse".
    prefixes = ["", "refs", "refs/tags", "refs/heads", "refs/remotes"]
    sympaths = map (</> name) prefixes ++ ["refs/remotes" </> name </> "HEAD"]


readRef :: FilePath -> IO (Maybe String)
readRef path = do
  plain <- try readPlain
  case plain of
    Right ok -> return $ Just ok
    Left exn -> readPacked
  where
    readPlain :: IO String
    readPlain = do
      content <- readFile (".git" </> path)
      case stripPrefix "ref:" content of
        Just target -> return $ stripWhitespace target
        Nothing     -> return $ stripWhitespace content
    readPacked :: IO (Maybe String)
    readPacked = do
      packed <- packedRefs
      return $ liftM fst $ find (\(_, p) -> p == path) packed

resolveRef :: FilePath -> IOE (String, Hash)
resolveRef ref = do
  liftIO$ print ("resolving",ref)
  target <- liftIO $ readRef ref
  case target of
    Nothing -> throwError $ "couldn't resolve ref: " ++ ref
    Just target -> do
      if isHashString target
        then return (ref, Hash (fromHex target))
        else resolveRef target

