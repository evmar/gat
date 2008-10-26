-- | RevParse manages parsing user-entered names for hashes (like
-- \"origin\/master~3\") into parse trees and targets hashes.
module RevParse (
  resolveRev,

  -- Exposed for testing
  Rev(..), parseRev, resolve
) where

import Control.Monad.Error
import Text.ParserCombinators.Parsec

import Commit
import Object (Object(..))
import ObjectStore (getObject)
import Refs
import Shared
import State

data Rev = RevHash String       -- ^ Explicit hash name.
         | RevParent Int Rev    -- ^ Nth parent of a Rev.
         | RevAncestor Int Rev  -- ^ Nth grandparent of a Rev.
         | RevSymRef String     -- ^ Name of a branch\/tag.
         deriving (Eq, Show)

-- |Parse a revision string, like \"origin\/master~3\", into a Rev.
parseRev :: String -> Either String Rev
parseRev input =
  case parse p_ref "" input of
    Left error -> throwError $ "error parsing revision: " ++ show error
    Right parse -> return parse


p_ref :: Parser Rev
p_ref = do rev <- (p_sha1 <|> p_symref)
           modrev <- nested rev (\rev -> p_parent rev <|> p_ancestor rev)
           eof; return modrev
  where
    -- |Sorta like a foldl of parsers: repeatedly parse, feeding output
    -- back in as input, until it no longer applies.
    nested base parser = do
      next <- optionMaybe (parser base)
      case next of
        Just next -> nested next parser
        Nothing -> return base
    p_sha1 = count 40 hexDigit >>= return . RevHash
    -- XXX what chars are allowed in a symref?
    p_symref = many1 (alphaNum <|> char '/') >>= return . RevSymRef
    -- Mods alter a Rev.
    p_parent rev = do
      char '^'
      num <- option 1 $ liftM read (many1 digit)
      return $ RevParent num rev
    p_ancestor rev = do
      char '~'
      num <- many1 digit
      return $ RevAncestor (read num) rev

getParent :: Int -> Hash -> GitM Hash
getParent nth hash = do
  -- TODO: obey the "nth".
  obj <- getObject hash
  case obj of
    ObCommit commit -> do
      let hashstr = "commit " ++ hashAsHex hash
      case commit_parents commit of
        [] ->
          fail $ hashstr ++ " has no parent"
        parents | length parents < nth ->
          fail $ hashstr ++ " has no " ++ show nth ++ "th parent"
        parents ->
         return $ Hash (fromHex (parents !! (nth-1)))
    _ -> fail "object is not a commit"

-- |Resolve a Rev into a Hash.
resolve :: Rev -> GitM Hash
resolve (RevHash hex) = return $ Hash (fromHex hex)
resolve (RevParent nth rev) = resolve rev >>= getParent nth
resolve (RevAncestor nth rev) = resolve rev >>= go nth where
  go 0 hash = return hash
  go n hash = do
    parent <- getParent 1 hash
    go (n-1) parent
resolve (RevSymRef name) = do
  fullname <- liftIO $ fullNameRef name
  case fullname of
    Nothing -> fail $ "couldn't resolve name: " ++ name
    Just name ->
      liftIO $ (resolveRef name >>= forceError)

-- | Resolve a string like \"origin\/master~3\" into a Hash.
resolveRev :: String -> GitM (ErrorOr Hash)
resolveRev input =
  case parseRev input of
    Left err -> return (throwError err)
    Right rev -> do
      hash <- resolve rev
      return (Right hash)
