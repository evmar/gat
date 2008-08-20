-- |RevParse manages parsing user-entered names for hashes (like
-- "origin/master~3") into parse trees and targets hashes.
module RevParse (
  resolveRev,

  -- * Exposed for testing
  Rev(..), parseRev, resolve
) where

import Control.Monad.Error
import Text.ParserCombinators.Parsec

import Object (Object(..))
import ObjectStore (getObject)
import Refs (resolveRef)
import Shared

data Rev = RevHash String     -- ^Explicit hash name.
         | RevParent Int Rev  -- ^Nth grandparent of a Rev.
         | RevSymRef String   -- ^Name of a branch/tag.
         deriving (Eq, Show)

-- |Parse a revision string, like "origin/master~3", into a Rev.
parseRev :: String -> Either String Rev
parseRev input =
  case parse p_ref "" input of
    Left error -> throwError $ "error parsing revision: " ++ show error
    Right parse -> return parse


p_ref :: Parser Rev
p_ref = do rev <- (p_sha1 <|> p_symref)
           modrev <- nested rev (p_parent)
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
    p_parent rev = do char '^'; return $ RevParent 1 rev


-- |Resolve a Rev into a Hash.
resolve :: Rev -> IOE Hash
resolve (RevHash hex) = return $ Hash (fromHex hex)
resolve (RevParent nth rev) = do
  hash <- resolve rev
  -- TODO: obey the "nth".
  obj <- getObject hash
  case obj of
    Commit headers message ->
      case lookup "parent" headers of
        Just hex -> return $ Hash (fromHex hex)
        Nothing -> throwError "commit has no parent"
    _ -> throwError "object is not a commit"
resolve (RevSymRef name) = do
  (_,hash) <- resolveRef name
  return hash

-- |Resolve a string like "origin/master~3" into a Hash.
resolveRev :: String -> IOE Hash
resolveRev input = returnE (parseRev input) >>= resolve
