import qualified Data.ByteString as B
import Data.Binary.Strict.Get
import Data.Word
import Test.HUnit

import Pack

assertParse :: (Show a, Eq a) => Get a -> [Word8] -> a -> Assertion
assertParse parser bytes exp =
  case runGet parser (B.pack bytes) of
    (Right out, rest) | B.null rest -> assertEqual "" exp out
    (Left err, rest) -> assertFailure $ "parse failure: " ++ show err
    (Right out, rest) -> assertFailure $ "parse failure: " ++ show out ++ " leftover " ++ show rest

tests = TestList [
      TestCase $ assertEqual "splitMSB" (True, 0x7F) (splitMSB 0xFF)
    , TestCase $ assertEqual "splitMSB" (False, 0x43) (splitMSB 0x43)
    , TestLabel "readDeltaOffset" $ TestList [
          TestCase $ assertParse readDeltaOffset [0x43]       0x43
        , TestCase $ assertParse readDeltaOffset [0x80, 0x43] (0x80 + 0x43)
        , TestCase $ assertParse readDeltaOffset [0x81, 0x43] (0x100 + 0x43)
        , TestCase $ assertParse readDeltaOffset [0x82, 0x43] (0x180 + 0x43)
        -- Found in a real pack file.
        , TestCase $ assertParse readDeltaOffset [0x83, 0x6f] 623
      ]
  ]

main = runTestTT tests
