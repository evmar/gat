module RevParse (
  Rev(..), parseRev
) where

import Control.Monad.Error
import Text.ParserCombinators.Parsec

data Rev = RevHash String
         | RevParent Int Rev
         | RevSymRef String
         deriving (Eq, Show)

parseRev :: String -> Either String Rev
parseRev input =
  case parse p_ref "" input of
    Left error -> throwError $ "error parsing revision: " ++ show error
    Right parse -> return parse

p_ref :: Parser Rev
p_ref = do rev <- (p_sha1 <|> p_symref)
           modrev <- option rev (p_parent rev)
           eof; return modrev
  where
    p_sha1 = count 40 hexDigit >>= return . RevHash
    -- XXX what chars are allowed in a symref?
    p_symref = many1 (alphaNum <|> char '/') >>= return . RevSymRef
    -- Mods alter a Rev.
    p_parent rev = do char '^'; return $ RevParent 1 rev
