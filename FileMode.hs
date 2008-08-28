module FileMode where

import System.Posix.Files
import System.Posix.Types
import Data.Bits
import Data.Word
import Text.Printf

s_IFREG = 0o10000
s_IFDIR = 0o40000

-- |GitFileMode tracks Git's representation of file modes, which is simpler
-- than the full Unix user/group/etc. distinctions.  Haskell insulates us
-- from the Unix values of file modes so we need to write out their values
-- here.
data GitFileType = GitFileRegular | GitFileDir deriving Show
data GitFileMode = GitFileMode {
  fm_executable :: Bool,
  fm_fileType :: GitFileType,
  fm_origInt :: Int
}
instance Show GitFileMode where
  show (GitFileMode exe typ int) =
    "(GitFileMode " ++ show exe ++ " " ++ show typ ++ " " ++
    printf "%o" int ++ ")"

testMode :: FileStatus -> FileMode -> Bool
testMode stat mode =
  intersectFileModes (fileMode stat) mode /= nullFileMode

-- cache.h:802, canon_mode()
modeFromStat :: FileStatus -> GitFileMode
modeFromStat stat | isDirectory stat =
  GitFileMode False GitFileDir (-1)
modeFromStat stat | isRegularFile stat =
  let executable = stat `testMode` ownerExecuteMode
  in GitFileMode executable GitFileRegular (-1)

modeFromInt :: Int -> GitFileMode
modeFromInt int | int .&. s_IFDIR /= 0 =
  GitFileMode False GitFileDir int
modeFromInt int =
  GitFileMode False GitFileRegular int

modeToString :: GitFileMode -> String
modeToString fm | fm_executable fm = "100755"
modeToString fm | not (fm_executable fm) = "100644"
