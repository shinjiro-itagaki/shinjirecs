import Test.HUnit
import Routing(getMaybeRawPathParamsFromPatternAndPath)
--import Data.ByteString
import Class.String(StringClass(..))

getMaybeRawPathParamsFromPatternAndPathTest :: Test
getMaybeRawPathParamsFromPatternAndPathTest =
  "Test of Routing.getMaybeRawPathParamsFromPatternAndPath"
  ~: getMaybeRawPathParamsFromPatternAndPath (toByteString "/hoge/:id/foo") (toByteString "/hoge/1/foo")
  ~=? Just [("id","1")]

main :: IO ()
main = runTestTT getMaybeRawPathParamsFromPatternAndPathTest >>= print
