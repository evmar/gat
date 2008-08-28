import Test.HUnit

import FileMode

testRoundTrip :: String -> String -> Int -> Test
testRoundTrip msg exp inp = test $
  assertEqual ("round-trip " ++ msg) exp (modeToString (modeFromInt inp))

testRoundTrips = test $ [
    testRoundTrip "regular"    "100644" 0o100644
  , testRoundTrip "executable" "100755" 0o100755
  , testRoundTrip "directory"  "40000"  0o40000
  , testRoundTrip "symlink"    "120000" 0o120000
  ]

main = runTestTT testRoundTrips
