module Object where

import qualified Data.ByteString.Lazy as BL
import Control.Monad.Error
import Shared

data ObjectType = TypeCommit | TypeTree | TypeBlob | TypeTag
                  deriving (Eq, Show)
type RawObject = (ObjectType, BL.ByteString)  -- Type, data.
data Object = Blob BL.ByteString
            | Commit [(String,String)] String
            | Tree [(String, FilePath, Hash)]
            deriving Show

-- |Convert a String to an ObjectType.
-- These string types appear in "loose" objects.
objectTypeFromString :: String -> Either String ObjectType
objectTypeFromString "blob"   = return TypeBlob
objectTypeFromString "tree"   = return TypeTree
objectTypeFromString "commit" = return TypeCommit
objectTypeFromString other    =
  throwError $ "unknown object type: " ++ show other
