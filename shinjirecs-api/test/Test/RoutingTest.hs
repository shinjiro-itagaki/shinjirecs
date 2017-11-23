module Test.RoutingTest where
import Test.HUnit
import Routing(routingMap, getMaybeRawPathParamsFromPatternAndPath, pathToPieces, findRoute, matchStdMethods,findPathMatchedRoutes)
import Routing.Class(RawPathParams,Path,toPath)
import Class.String(toStrings,toByteString)
import Network.HTTP.Types.Method(StdMethod(GET,POST,HEAD,PUT,DELETE,TRACE,CONNECT,OPTIONS,PATCH), parseMethod)
import Data.Maybe(fromJust)
import Helper.ListHelper((\\))
import Routing.Class(Route(..),RouteNotFoundError(PathNotFound,PathFoundButMethodUnmatch,UnknownMethod))
import Controller.Types(ActionWrapper(..))

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

findRouteTest :: Test
findRouteTest =
  TestLabel "Test of Routing.findRoute"
  $ TestList
  $ Prelude.map (\(methods, path, res_params) ->
                   TestList $ [
                    TestList  $ toTests'  (toPath path) res_params methods
                    ,TestList $ toTests2' (toPath path) res_params $ ([minBound .. maxBound] \\ methods)
                    ]
                )
  $ [([GET],      "/channels/list"     ,[])
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
    -- Either RouteNotFound (ActionWrapper, RawPathParams)
    fromLeft' :: Routing.Class.Path -> StdMethod -> Either RouteNotFoundError (Route, RawPathParams) -> RouteNotFoundError
    fromLeft' path method (Left x)  = x
    fromLeft' path method (Right ((MkRoute methods ptn _ ), params))  = error $ "expect is RouteNotFoundError but actual is that [" ++ (show method) ++ "] " ++ (show path) ++ " matches at [" ++ (show methods) ++ "] " ++ (show ptn)
    fromRight' :: Routing.Class.Path -> StdMethod -> Either RouteNotFoundError (Route, RawPathParams) -> (Route, RawPathParams)
    fromRight' path method (Left _)  = error $ "expect is some route but actual is RouteNotFoundError by [" ++ (show method) ++ "] " ++ (show path)
    fromRight' path method (Right x) = x
    toTest',toTest2' :: Routing.Class.Path -> RawPathParams -> StdMethod -> Test
    toTest'  path res_params method = (snd . fromRight' path method $ findRoute method (toByteString path)) ~?= res_params
    toTest2' path res_params method = (fromLeft'  path method $ findRoute method (toByteString path)) ~?= (PathFoundButMethodUnmatch "")
    toTests',toTests2' :: Routing.Class.Path -> RawPathParams -> [StdMethod] -> [Test]
    toTests'  path params methods = Prelude.map (toTest'  path params) methods
    toTests2' path params methods = Prelude.map (toTest2' path params) methods

matchStdMethodsTest :: Test
matchStdMethodsTest = TestLabel "Test of Routing.matchStdMethodsTest"
                      $ (matchStdMethods (routingMap !! 3) POST) ~?= True

findPathMatchedRoutesTest :: Test
findPathMatchedRoutesTest =
  TestLabel "Test of Routing.findPathMatchedRoutes"
  $ (getMethods' $ fst $ (!! 0) $ findPathMatchedRoutes (toByteString "/channels") routingMap)
  ~?= [POST]
  where
    getMethods' :: Route -> [StdMethod]
    getMethods' (MkRoute methods _ _) = methods 

tests :: Test
tests = TestList [
  pathToPiecesTest
  ,getMaybeRawPathParamsFromPatternAndPathTest
  ,findRouteTest
  ,matchStdMethodsTest
  ,findPathMatchedRoutesTest
  ]
