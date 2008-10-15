import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Strict.Get
import Test.HUnit

import Delta

-- A basic test of the deta code.  Reads testdata/delta.base, applies
-- testdata/delta as a delta, and verifies the output size is as expected.
testApply :: Test
testApply = test $ do
  base <- B.readFile "testdata/delta.base"
  rawdelta <- B.readFile "testdata/delta"
  delta <-
    case runGet readDelta rawdelta of
      (Right delta, rest) -> do
        assert (B.null rest)
        return delta
      other -> do assertFailure $ show other; undefined
  assertEqual "base size" (fromIntegral $ d_origSize delta) (B.length base)
  let patched = applyDelta base delta
  assertEqual "patched size" (fromIntegral $ d_resultSize delta) (BL.length patched)

main = runTestTT testApply
