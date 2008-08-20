
import Test.HUnit
import RevParse

assertParse :: Rev -> String -> Assertion
assertParse exp inp =
  case parseRev inp of
    Left error -> assertFailure "parse failure"
    Right rev -> assertEqual "" exp rev
testParse label exp inp = TestLabel label $ TestCase (assertParse exp inp)

tests = TestList [
    sha1, symref, parent, grandparent, grandparent2, grandparent3
  ]
  where
    hEAD = RevSymRef "HEAD"
    sha1 = testParse "sha1"
      (RevHash "39cfa1b3e586a092b04cfd81ad9f58844448e845")
      "39cfa1b3e586a092b04cfd81ad9f58844448e845"
    symref = testParse "symref" (RevSymRef "HEAD") "HEAD"
    parent = testParse "parent" (RevParent 1 hEAD) "HEAD^"
    grandparent = testParse "grandparent"
      (RevParent 1 (RevParent 1 hEAD)) "HEAD^^"
    grandparent2 = testParse "grandparent2" (RevAncestor 2 hEAD) "HEAD~2"
    grandparent3 = testParse "grandparent3"
      (RevParent 1 (RevAncestor 2 hEAD)) "HEAD~2^"

main = runTestTT tests
