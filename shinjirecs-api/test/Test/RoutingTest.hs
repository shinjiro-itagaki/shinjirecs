module Test.RoutingTest where
import Test.HUnit
import Routing(getMaybeRawPathParamsFromPatternAndPath, pathToPieces)
import Class.String(toStrings,toByteString)

pathToPiecesTest :: Test
pathToPiecesTest =
  "Test of Routing.pathToPieces"
  ~: ["hoge","foo","baa"]
  ~=? (toStrings $ pathToPieces $ toByteString "//hoge///foo/baa")

getMaybeRawPathParamsFromPatternAndPathTest :: Test
getMaybeRawPathParamsFromPatternAndPathTest =
  TestLabel "Test of Routing.getMaybeRawPathParamsFromPatternAndPath" $ TestList $ [
  f (toByteString "/hoge/:id/foo") (toByteString "/hoge/1/foo") ~?= Just [("id","1")],
  f (toByteString "/hoge/foo")     (toByteString "/hoge/foo")   ~?= Just []
  ]
  where
    f = getMaybeRawPathParamsFromPatternAndPath

tests :: Test
tests = TestList [
  pathToPiecesTest
  ,getMaybeRawPathParamsFromPatternAndPathTest  
  ]
