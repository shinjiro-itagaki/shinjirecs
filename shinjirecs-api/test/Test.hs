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
  "Test of Routing.getMaybeRawPathParamsFromPatternAndPath"
  ~: Just [("id","1")] --expected
  ~=? getMaybeRawPathParamsFromPatternAndPath (toByteString "/hoge/:id/foo") (toByteString "/hoge/1/foo") -- result

tests = TestList [
  pathToPiecesTest
  ,getMaybeRawPathParamsFromPatternAndPathTest  
  ]

main :: IO ()
main = runTestTT tests >>= print
