import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Strict.Get
import Control.Monad.Error
import Test.HUnit

import Object
import ObjectStore

testLoose :: Test
testLoose = test $ do
  -- Load an object and assert a few of its properties.
  (Just (objtype, raw)) <- getLooseObject "testdata/loose"
  assertEqual "parsed as blob" TypeBlob objtype
  assertEqual "content length" 2794 (BL.length raw)

main = runTestTT testLoose
