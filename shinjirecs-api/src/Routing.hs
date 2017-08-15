{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Routing where
import Network.Wai (Request(..))
import qualified Controllers.ChannelsController     as ChannelsC
import qualified Controllers.InstallController      as InstallC
import qualified Controllers.ProgramsController     as ProgramsC
import qualified Controllers.ReservationsController as ReservationsC
import DB(Connection)

import Network.HTTP.Types (status200, status201, status400, status404)
import Network.HTTP.Types.Method(StdMethod(GET,POST,HEAD,PUT,DELETE,TRACE,CONNECT,OPTIONS,PATCH), parseMethod)
import Data.List(find)
import Data.ByteString(ByteString)
import Data.ByteString.Char8(split,null)

import Controller.Types(ActionWrapper(..), Action, ControllerResponse(..) ,ParamGivenAction)
import Controller(defaultControllerResponse)

import Routing.Class(Route(MkRoute),Path,PathPattern,RawPathParamKey,RawPathParamVal,RawPathParam,RawPathParams,PathParamList(..),toParamGivenAction, toActionWrapper,RouteNotFoundError(PathNotFound,PathFoundButMethodUnmatch,UnknownMethod), RawPathParamsError(BadParamTypes,BadRouteDefinition), RoutingError(RouteNotFound,BadPathParams))
import Class.String(StringClass(..))
import Data.List.Split
import Data.Maybe(fromJust,isJust)
import Data.Word(Word8)

notFound, badRequest :: Action ()
notFound conn method req _ = return $ defaultControllerResponse {
  status = status404
  }

badRequest conn method req _ = return $ defaultControllerResponse {
  status = status400
  }

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

(@>>) :: (PathParamList a) => ([StdMethod], PathPattern) -> Action a -> Route
(@>>) (stdmethods, pathpattern) action = MkRoute stdmethods pathpattern $ toActionWrapper action

_GET_ = [GET]
_POST_ = [POST]
_PATCH_ = [PATCH,PUT]
_DELETE_ = [DELETE]
_ALL_ = [minBound .. maxBound] :: [StdMethod]

routingMap :: [Route]
routingMap = [
   ( _GET_ ,   "/channels/list"     ) @>> ChannelsC.list
  ,( _GET_,    "/channels/:id"      ) @>> ChannelsC.get
  ,( _PATCH_,  "/channels/:id"      ) @>> ChannelsC.modify
  ,( _POST_,   "/channels"          ) @>> ChannelsC.create
  ,( _DELETE_, "/channels/:id"      ) @>> ChannelsC.destroy
    
  ,( _GET_,    "/install/index"     ) @>> notFound -- InstallC.index
  ,( _GET_,    "/install/channels"  ) @>> notFound -- InstallC.resultDetectChannels
  ,( _GET_,    "/install/step1"     ) @>> notFound -- (InstallC.step 1)
  ,( _GET_,    "/install/step2"     ) @>> notFound -- (InstallC.step 2)
  ,( _GET_,    "/install/step3"     ) @>> notFound -- (InstallC.step 3)
    
  ,( _GET_,    "/programs/list"     ) @>> notFound -- ProgramsC.list
  ,( _GET_,    "/programs/:id"      ) @>> notFound -- ProgramsC.get
  ,( _PATCH_,  "/programs/:id"      ) @>> notFound -- ProgramsC.modify
  ,( _POST_,   "/programs"          ) @>> notFound -- ProgramsC.create
  ,( _DELETE_, "/programs/:id"      ) @>> notFound -- ProgramsC.destroy
    
  ,( _GET_,    "/reservations/list" ) @>> notFound -- ReservationsC.list
  ,( _GET_,    "/reservations/:id"  ) @>> notFound -- ReservationsC.get
  ,( _PATCH_,  "/reservations/:id"  ) @>> notFound -- ReservationsC.modify
  ,( _POST_,   "/reservations"      ) @>> notFound -- ReservationsC.create
  ,( _DELETE_, "/reservations/:id"  ) @>> notFound -- ReservationsC.destroy
  ,( _ALL_,    "*"                  ) @>> notFound -- not found error 
  ]

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

run :: Request -> Either RoutingError (StdMethod, ParamGivenAction)
run req =
  case parseMethod $ requestMethod req of
    Left _ -> Left $ RouteNotFound UnknownMethod
    Right stdmethod -> case findRoute stdmethod (rawPathInfo req) of
      Right ((MkRoute methods ptn actionWrapper), rawPathParams) ->
        case toParamGivenAction actionWrapper rawPathParams of
          Left x -> Left $ BadPathParams x
          Right action -> Right (stdmethod, action)
      Left x -> Left $ RouteNotFound x
