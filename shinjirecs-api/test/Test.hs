import Test.HUnit
import qualified Test.RoutingTest as Routing
import qualified Test.HelperTest  as Helper

allTests = TestList [
  Routing.tests
  ,Helper.tests
  ]
main :: IO ()
main = runTestTT allTests >>= print
