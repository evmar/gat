module Color where

data Color = Bold | Green | Red | Cyan | Yellow

ansiColorCode :: Color -> String
ansiColorCode Bold   = "1"
ansiColorCode Red    = "31"
ansiColorCode Green  = "32"
ansiColorCode Yellow = "33"
ansiColorCode Cyan   = "36"

ansiEscape :: String -> String
ansiEscape code = "\x1b[" ++ code ++ "m"

coloredLine :: Color -> String -> String
coloredLine color str =
  ansiEscape (ansiColorCode color) ++ str ++ ansiEscape ""
