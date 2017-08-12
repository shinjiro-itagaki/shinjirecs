module Test.RoutingTest where
import Test.HUnit
import Routing(getMaybeRawPathParamsFromPatternAndPath, pathToPieces, findAction)
import Routing.Class(RawPathParams,Path,toPath)
import Class.String(toStrings,toByteString)
import Network.HTTP.Types.Method(StdMethod(GET,POST,HEAD,PUT,DELETE,TRACE,CONNECT,OPTIONS,PATCH), parseMethod)

pathToPiecesTest :: Test
pathToPiecesTest = TestLabel "Test of Routing.pathToPieces"
                   $ (toStrings $ pathToPieces $ toPath "//hoge///foo/baa") ~?= ["hoge","foo","baa"]

getMaybeRawPathParamsFromPatternAndPathTest :: Test
getMaybeRawPathParamsFromPatternAndPathTest =
  TestLabel "Test of Routing.getMaybeRawPathParamsFromPatternAndPath"
  $ TestList
  $ [f (toByteString "/hoge/:id/foo") (toByteString "/hoge/1/foo") ~?= Just [("id","1")],
     f (toByteString "/hoge/foo")     (toByteString "/hoge/foo")   ~?= Just []
    ]
  where
    f = getMaybeRawPathParamsFromPatternAndPath

findActionTest :: Test
findActionTest =
  TestLabel "Test of Routing.findAction"
  $ TestList
  $ Prelude.map (\(methods, path,res_params) -> TestList $ toTests' methods (toPath path) res_params)
  $ [([GET] ,     "/channels/list"     ,[])
    ,([GET],      "/channels/1"        ,[("id","1")])
    ,([PATCH,PUT],"/channels/2"        ,[("id","2")])
    ,([POST],     "/channels"          ,[])
    ,([DELETE],   "/channels/3"        ,[("id","3")])
    ,([GET],      "/install/index"     ,[])
    ,([GET],      "/install/channels"  ,[])
    ,([GET],      "/install/step1"     ,[])
    ,([GET],      "/install/step2"     ,[])
    ,([GET],      "/install/step3"     ,[])
    ,([GET],      "/programs/list"     ,[]) 
    ,([GET],      "/programs/4"        ,[("id","4")])
    ,([PATCH,PUT],"/programs/5"        ,[("id","5")])
    ,([POST],     "/programs"          ,[])
    ,([DELETE],   "/programs/6"        ,[("id","6")])
    ,([GET],      "/reservations/list" ,[]) 
    ,([GET],      "/reservations/7"    ,[("id","7")]) 
    ,([PATCH,PUT],"/reservations/8"    ,[("id","8")]) 
    ,([POST],     "/reservations"      ,[]) 
    ,([DELETE],   "/reservations/9"    ,[("id","9")])
    ]
  where
    toTest' :: StdMethod -> Routing.Class.Path -> RawPathParams -> Test
    toTest' method path res_params = (fmap (\(_, x) -> x) $ findAction method (toByteString path)) ~?= Right res_params
    toTests' :: [StdMethod] -> Routing.Class.Path -> RawPathParams -> [Test]
    toTests' methods path res_params = Prelude.map (\method -> toTest' method path res_params) methods

tests :: Test
tests = TestList [
  pathToPiecesTest
  ,getMaybeRawPathParamsFromPatternAndPathTest  
  ]
