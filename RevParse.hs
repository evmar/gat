module RevParse (
  Rev(..), parseRev
) where

import Control.Monad.Error
import Text.ParserCombinators.Parsec

data Rev = RevHash String
         | RevSymRef String
         deriving (Eq, Show)

parseRev :: String -> Either String Rev
parseRev input =
  case parse p_ref "" input of
    Left error -> throwError $ "error parsing revision: " ++ show error
    Right parse -> return parse

p_ref :: Parser Rev
p_ref = do rev <- p_sha1; eof; return rev
  where
    p_sha1 = count 40 hexDigit >>= return . RevHash
