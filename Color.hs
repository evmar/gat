-- | Basic ANSI color handling.
module Color (
    Color(..)
  , coloredLine
) where

-- | Just the colors that are actually used by Gat.
data Color = Bold | Green | Red | Cyan | Yellow

-- | Map a Color to the ANSI color code used to generate it.
ansiColorCode :: Color -> String
ansiColorCode Bold   = "1"
ansiColorCode Red    = "31"
ansiColorCode Green  = "32"
ansiColorCode Yellow = "33"
ansiColorCode Cyan   = "36"

ansiEscape :: String -> String
ansiEscape code = "\x1b[" ++ code ++ "m"

-- | Wrap a string with a color + color reset pair.
coloredLine :: Color -> String -> String
coloredLine color str =
  ansiEscape (ansiColorCode color) ++ str ++ ansiEscape ""
