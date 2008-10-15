-- | Git uses a representation of file modes that is simpler than the full Unix
-- per-user\/group\/etc. distinctions.  This module maps between various
-- formats and a datatype representing a git file mode.
module FileMode (
    GitFileMode(..)
  , modeFromStat
  , modeFromPath
  , modeFromInt
  , modeToString
) where

import System.Posix.Files
import System.Posix.Types
import Data.Bits

-- man 2 stat
s_IFREG = 0o10000
s_IFDIR = 0o40000
s_IFLNK = 0o120000

-- |GitFileMode represents git's representation of file modes, which is simpler
-- than the full Unix user\/group\/etc. file mode.
data GitFileMode = GitFileRegular Bool
                 | GitFileDirectory
                 | GitFileSymlink
                 deriving (Eq, Show)

-- @testMode stat mode@ tests whether @stat@ has @mode@ set.
testMode :: FileStatus -> FileMode -> Bool
testMode stat mode =
  intersectFileModes (fileMode stat) mode /= nullFileMode

-- cache.h:802, canon_mode()
-- | Convert stat() output to a GitFileMode.
modeFromStat :: FileStatus -> GitFileMode
modeFromStat stat | isDirectory stat = GitFileDirectory
modeFromStat stat | isRegularFile stat =
  let executable = stat `testMode` ownerExecuteMode
  in GitFileRegular executable

-- | stat() a path and construct a GitFileMode.
modeFromPath :: FilePath -> IO GitFileMode
modeFromPath path = do
  stat <- getFileStatus path
  return $ modeFromStat stat

-- | Convert the Unix-style integer file mode (found in git data structures) to
-- a GitFileMode.
modeFromInt :: Int -> GitFileMode
modeFromInt int | int .&. s_IFDIR == s_IFDIR = GitFileDirectory
modeFromInt int | int .&. s_IFLNK == s_IFLNK = GitFileSymlink
modeFromInt int = GitFileRegular (int .&. 1 /= 0)

-- | Convert a GitFileMode to a String of the form found in e.g. diff output.
modeToString :: GitFileMode -> String
modeToString (GitFileRegular True)  = "100755"
modeToString (GitFileRegular False) = "100644"
modeToString GitFileDirectory = "40000"
modeToString GitFileSymlink = "120000"
