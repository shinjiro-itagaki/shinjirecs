{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Routing where
import Network.Wai (Request(..))
import Network.HTTP.Types.Method(StdMethod, parseMethod, Method, renderStdMethod)
import Data.List(find)
import Data.ByteString(ByteString)
import Data.ByteString.Char8(split,null)

import Routing.Class(Route(MkRoute),Path,PathPattern,RawPathParam,RawPathParams,toParamGivenAction, RouteNotFoundError(PathNotFound,PathFoundButMethodUnmatch,UnknownMethod), RawPathParamsError(BadRouteDefinition, BadParamTypes),RoutingError(RouteNotFound,BadPathParams))
import Class.String(toString, toByteString)
import Data.Maybe(fromJust,isJust)
import Data.Word(Word8)
import Data.Text(Text)
import System.IO(putStrLn)

import qualified Controllers.ChannelsController     as ChannelsC
import qualified Controllers.HomeController         as HomeC
import qualified Controllers.InstallController      as InstallC
import qualified Controllers.ProgramsController     as ProgramsC
import qualified Controllers.ReservationsController as ReservationsC
import Routing.Class(Route(MkRoute),Path,PathPattern,PathParamList(..), toActionWrapper)
import Routing.Types(Resource(listAction ,getAction ,modifyAction ,createAction ,destroyAction))

import Network.HTTP.Types.Method(StdMethod(GET,POST,HEAD,PUT,DELETE,TRACE,CONNECT,OPTIONS,PATCH))
import Controller.Types(Action,status,ControllerResponse)
import Class.String((+++))
import Controller(defaultControllerResponse, responseBadRequest, doIfValidInputJSON,ToBody(toBody), responsePathNotFound, responseNotAllowedMethod, responseUnsupportedMethod, response500)
import Network.HTTP.Types (status200, status201, status400, status404)
import Data.Aeson(Value,(.=),object)
import Controller.Types(ParamGivenAction,responseToValue,body)
import Network.Wai (Request(..))
--import Routing.Class(RoutingError(RouteNotFound,BadPathParams))
import Network.HTTP.Types.Method(StdMethod, parseMethod)
import Routing.Class(Route(MkRoute),Path,PathPattern,RawPathParam,RawPathParams,toParamGivenAction, RouteNotFoundError(PathNotFound,PathFoundButMethodUnmatch,UnknownMethod))

matchStdMethods :: Route -> StdMethod -> Bool
matchStdMethods (MkRoute methods _ _ ) method = elem method methods

matchPathElements :: [String] -> [String] -> Maybe RawPathParams -> Maybe RawPathParams
matchPathElements _      _      Nothing = Nothing -- protect error by fromJust
matchPathElements []     (y:ys) _       = Nothing -- mismatch count of elements
matchPathElements (x:xs) []     _       = Nothing -- mismatch count of elements
matchPathElements ptn@(x@(':':sym):xs) pathelems@(y:ys) params = matchPathElements xs ys $ Just $ (fromJust params) ++ [(sym, y)]
matchPathElements ptn@(x:xs) pathelems@(y:ys) params =
  if x == y
  then matchPathElements xs ys params -- match and do next
  else Nothing -- unmatch
matchPathElements [] [] x = x -- finished
-- matchPathElements _ _ _ = Nothing -- error

pathToPieces :: Path -> [String]
pathToPieces path = Prelude.map toString $ filter (not . Data.ByteString.Char8.null) $ Data.ByteString.Char8.split '/' $ toByteString path

getMaybeRawPathParamsFromPatternAndPath :: PathPattern -> Path -> Maybe RawPathParams
getMaybeRawPathParamsFromPatternAndPath ptn path =
  matchPathElements ptn_pieces' path_pieces' (Just [])
  where
    ptn_pieces'  = pathToPieces ptn
    path_pieces' = pathToPieces path

-- Nothing means unmatch path
getMaybeRawPathParams :: Route -> Path -> Maybe RawPathParams
getMaybeRawPathParams (MkRoute _ ptn _ ) path = getMaybeRawPathParamsFromPatternAndPath ptn path

matchPath :: Path -> Route -> Bool
matchPath p r = isJust $ getMaybeRawPathParams r p
    
getRawPathParams :: Route -> Path -> RawPathParams
getRawPathParams r p = case getMaybeRawPathParams r p of
  Just params -> params
  Nothing     -> []

findPathMatchedRoutes :: Path -> [Route] -> [(Route, RawPathParams)]
findPathMatchedRoutes path [] = []
findPathMatchedRoutes path (r:rs) = case getMaybeRawPathParams r path of
  Just p  -> [(r, p)] ++ findPathMatchedRoutes path rs
  Nothing -> findPathMatchedRoutes path rs

findMethodMatchedRoute :: StdMethod -> [(Route, RawPathParams)] -> Maybe (Route, RawPathParams)
findMethodMatchedRoute _ [] = Nothing
findMethodMatchedRoute stdmethod ((route@(MkRoute methods ptn actionWrapper),params):xs) =
  if matchStdMethods route stdmethod
  then Just (route, params)
  else findMethodMatchedRoute stdmethod xs

findRoute :: StdMethod -> Path -> Either RouteNotFoundError (Route, RawPathParams)
findRoute stdmethod path =
  case findPathMatchedRoutes path routingMap of
    [] -> Left PathNotFound
    xs -> case find (\(route, params) -> matchStdMethods route stdmethod) xs of
      Just x  -> Right x
      Nothing ->
        Left $ PathFoundButMethodUnmatch $ Prelude.concat $ Prelude.map (showRoute' . fst) xs
  where
    showRoute' :: Route -> String
    showRoute' (MkRoute methods ptn _ ) = Prelude.concat [show methods, " ", toString ptn]

notFound, badRequest :: Action ()
notFound conn method req _ = return $ defaultControllerResponse {
  status = status404
  }

badRequest conn method req _ = return $ defaultControllerResponse {
  status = status400
  }

_GET_ = [GET]
_POST_ = [POST]
_PATCH_ = [PATCH,PUT]
_DELETE_ = [DELETE]
_ALL_ = [minBound .. maxBound] :: [StdMethod]

(@>>) :: (PathParamList a) => ([StdMethod], PathPattern) -> Action a -> Route
(@>>) (stdmethods, pathpattern) action = MkRoute stdmethods pathpattern $ toActionWrapper action

readResource :: PathPattern -> Resource -> [Route]
readResource p rs = [
   ( _GET_ ,   p +++ "/list"     ) @>> listAction    rs
  ,( _GET_,    p +++ "/:id"      ) @>> getAction     rs
  ,( _PATCH_,  p +++ "/:id"      ) @>> modifyAction  rs
  ,( _POST_,   p +++ ""          ) @>> createAction  rs
  ,( _DELETE_, p +++ "/:id"      ) @>> destroyAction rs
  ]

routingMap :: [Route]
routingMap =
  (readResource "/channels"     ChannelsC.resource     ) ++
  (readResource "/programs"     ProgramsC.resource     ) ++
  (readResource "/reservations" ReservationsC.resource ) ++  
  [
  ( _GET_,    "/install/index"     ) @>> notFound -- InstallC.index
  ,( _GET_,    "/install/channels"  ) @>> notFound -- InstallC.resultDetectChannels
  ,( _GET_,    "/install/step1"     ) @>> notFound -- (InstallC.step 1)
  ,( _GET_,    "/install/step2"     ) @>> notFound -- (InstallC.step 2)
  ,( _GET_,    "/install/step3"     ) @>> notFound -- (InstallC.step 3)
  ,( _GET_,    "/multi"             ) @>> multi
  ,( _ALL_,    "*"                  ) @>> notFound -- not found error 
  ]

multi :: Action ()
multi _ method conn req = doIfValidInputJSON req $ impl' []
  where
    path' = rawPathInfo req :: ByteString
    impl' :: [IO (Text, ControllerResponse)] -> [Text] -> IO ControllerResponse
    impl' ys []     = do
      xs <- sequence ys
      return $ defaultControllerResponse {
        body = toBody $ object $ Prelude.map (\(k,v) -> k .= (responseToValue v)) xs
        }
    impl' ys (x:xs) = do
      case runImpl method (toByteString x) of
        Right (_, action, _) -> do
          cres <- action method conn req
          impl' (ys ++ [return (x,cres)]) xs
        Left err -> return err

routingErrorToControllerResponse :: RoutingError -> Path -> Method -> ControllerResponse
routingErrorToControllerResponse (RouteNotFound PathNotFound)                    path method = responsePathNotFound      path
routingErrorToControllerResponse (RouteNotFound (PathFoundButMethodUnmatch msg)) path method = responseNotAllowedMethod  path (show method)
routingErrorToControllerResponse (RouteNotFound UnknownMethod)                   path method = responseUnsupportedMethod path
routingErrorToControllerResponse (BadPathParams (BadParamTypes keys))            path method = responseBadRequest $ (show keys) ++ " are bad type"
routingErrorToControllerResponse (BadPathParams BadRouteDefinition)              path method = response500 "BadRouteDefinition!"

run :: Request -> Either ControllerResponse (StdMethod, ParamGivenAction, Route)
run req =
  case parseMethod $ requestMethod req of
    Left method -> Left $ routingErrorToControllerResponse (RouteNotFound UnknownMethod) path' method
    Right stdmethod -> runImpl stdmethod path'
  where
    path' = rawPathInfo req

runImpl :: StdMethod -> ByteString -> Either ControllerResponse (StdMethod, ParamGivenAction, Route)
runImpl stdmethod path =
  case findRoute stdmethod path of
    Right (route@(MkRoute methods ptn actionWrapper), rawPathParams) ->
      case toParamGivenAction actionWrapper rawPathParams of
        Left x -> Left $ routingErrorToControllerResponse (BadPathParams x) path (renderStdMethod stdmethod)
        Right action -> Right (stdmethod, action, route)
    Left x -> Left $ routingErrorToControllerResponse (RouteNotFound x) path (renderStdMethod stdmethod)
