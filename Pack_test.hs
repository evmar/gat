import qualified Data.ByteString as B
import Control.Monad.Identity
import Data.Binary.Strict.Get
import Data.Maybe
import Data.Word
import Test.HUnit

import Pack
import Shared

testSearch = test $ do
  let l1 = [1,4,6,7,8,13,100]
  -- Try searching the list for each item in the list.
  let found = catMaybes $ map (searchList l1) l1
  assertEqual "all entries found" [0..(length l1-1)] found
  -- Try searching the list for some items not in the list
  let found = catMaybes $ map (searchList l1) [0, 3, 9, 54, 1001]
  assertEqual "no non-entries found" [] found
  where
    searchList list target = runIdentity $
      binarySearch (0, length list) (\x -> Identity (list !! x)) target

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
    , TestLabel "binarySearch" $ testSearch
  ]

main = runTestTT tests
