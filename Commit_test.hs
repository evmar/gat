import Test.HUnit

import Commit

testParse :: Test
testParse = test $ do
  text <- readFile "testdata/commit"
  let Right commit = parseCommit text
  assertEqual "tree parsed"
    "6cab39a126bb985be8ff6e3907f648d55c2a5c57" (commit_tree commit)
  assertEqual "parent parsed"
    ["e6fe5236a1b7fe63f3481cd60cda17a76e433c65"] (commit_parents commit)
  assertEqual "author parsed"
    "Evan Martin <martine@danga.com> 1224448950 -0700" (commit_author commit)
  assertEqual "committer parsed"
    "Evan Martin <martine@danga.com> 1224448950 -0700" (commit_committer commit)
  assertEqual "message parsed"
    "generate docs in makefile\n" (commit_message commit)

main = runTestTT testParse
