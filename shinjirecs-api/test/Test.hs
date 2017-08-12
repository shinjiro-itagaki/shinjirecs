import Test.HUnit
import Routing(getMaybeRawPathParamsFromPatternAndPath, pathToPieces)
--import Data.ByteString
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

tests = TestList [
  pathToPiecesTest
  ,getMaybeRawPathParamsFromPatternAndPathTest  
  ]

main :: IO ()
main = runTestTT tests >>= print
