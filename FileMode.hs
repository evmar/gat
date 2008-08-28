module FileMode where

import System.Posix.Files
import System.Posix.Types
import Data.Bits
import Data.Word
import Text.Printf

-- man 2 stat
s_IFREG = 0o10000
s_IFDIR = 0o40000
s_IFLNK = 0o120000

-- |GitFileMode tracks Git's representation of file modes, which is simpler
-- than the full Unix user/group/etc. distinctions.  Haskell insulates us
-- from the Unix values of file modes so we need to write out their values
-- here.
data GitFileMode = GitFileRegular Bool
                 | GitFileDirectory
                 | GitFileSymlink
                 deriving (Eq, Show)

testMode :: FileStatus -> FileMode -> Bool
testMode stat mode =
  intersectFileModes (fileMode stat) mode /= nullFileMode

-- cache.h:802, canon_mode()
modeFromStat :: FileStatus -> GitFileMode
modeFromStat stat | isDirectory stat = GitFileDirectory
modeFromStat stat | isRegularFile stat =
  let executable = stat `testMode` ownerExecuteMode
  in GitFileRegular executable

modeFromInt :: Int -> GitFileMode
modeFromInt int | int .&. s_IFDIR == s_IFDIR = GitFileDirectory
modeFromInt int | int .&. s_IFLNK == s_IFLNK = GitFileSymlink
modeFromInt int = GitFileRegular (int .&. 1 /= 0)

modeToString :: GitFileMode -> String
modeToString (GitFileRegular True)  = "100755"
modeToString (GitFileRegular False) = "100644"
modeToString GitFileDirectory = "40000"
modeToString GitFileSymlink = "120000"
