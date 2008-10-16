-- | A git \"object\" is a hash-addressed structure stored in the
-- object store.

module Object (
    ObjectType(..)
  , RawObject
  , Object(..)
  , Tree(..)
  , objectTypeFromString
) where

import qualified Data.ByteString.Lazy as BL
import Control.Monad.Error

import FileMode
import Shared

-- | ObjectType represents the types of objects available.
data ObjectType = TypeCommit  -- ^ tree and metadata about the tree
                | TypeTree    -- ^ list of hashes and filenames
                | TypeBlob    -- ^ collection of bytes (a file)
                | TypeTag
                deriving (Eq, Show)

-- | RawObject is an object type and unparsed bytes of the object.
type RawObject = (ObjectType, BL.ByteString)  -- Type, data.

-- | Object represents a parsed object.
data Object = Blob BL.ByteString  -- ^ blob of bytes (a file's contents)
            | Commit [(String,String)] String
              -- ^ (key,value) metadata and commit message
            | ObTree Tree
            deriving Show

data Tree = Tree [(GitFileMode, FilePath, Hash)]
            deriving Show

-- | Convert a String to an ObjectType.
-- These string names appear in "loose" objects.
objectTypeFromString :: String -> Either String ObjectType
objectTypeFromString "blob"   = return TypeBlob
objectTypeFromString "tree"   = return TypeTree
objectTypeFromString "commit" = return TypeCommit
objectTypeFromString other    =
  throwError $ "unknown object type: " ++ show other
