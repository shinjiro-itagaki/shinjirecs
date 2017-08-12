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

import Routing.Class(Path,PathPattern,RawPathParamKey,RawPathParamVal,RawPathParam,RawPathParams,PathParamList(..),toParamGivenAction, toActionWrapper,RouteNotFound(PathNotFound,PathFoundButMethodUnmatch,UnknownMethod))
import Class.String(StringClass(..))
import Data.List.Split
import Data.Maybe(fromJust,isJust)
import Data.Word(Word8)

notFound, badRequest :: Action ()
notFound conn req _ = return $ defaultControllerResponse {
  status = status404
  }

badRequest conn req _ = return $ defaultControllerResponse {
  status = status400
  }

data Route = MkRoute [StdMethod] PathPattern ActionWrapper

matchStdMethods :: Route -> StdMethod -> Bool
matchStdMethods (MkRoute ys _ _ ) x = elem x ys

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
  ,( _POST_,   "/channels"          ) @>> notFound -- ChannelsC.create
  ,( _DELETE_, "/channels/:id"      ) @>> notFound -- ChannelsC.destroy
    
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

findPathMatchedRoute :: Path -> [Route] -> Maybe (Route, RawPathParams)
findPathMatchedRoute path [] = Nothing
findPathMatchedRoute path (r:rs) = case getMaybeRawPathParams r path of
  Just p  -> Just (r, p)
  Nothing -> findPathMatchedRoute path rs
  
findAction :: StdMethod -> Path -> Either RouteNotFound (ActionWrapper, RawPathParams)
findAction stdmethod path =
  case findPathMatchedRoute path routingMap of
    Just (route@(MkRoute methods ptn actionWrapper), rawpathparams) ->
      if matchStdMethods route stdmethod
      then Right (actionWrapper, rawpathparams)
      else Left PathFoundButMethodUnmatch
    Nothing -> Left PathNotFound

run :: Request -> Either RouteNotFound ParamGivenAction
run req =
  case parseMethod $ requestMethod req of
    Left _ -> Left UnknownMethod
    Right stdmethod -> case findAction stdmethod (rawPathInfo req) of
      Right (actionWrapper, rawPathParams) -> Right $ toParamGivenAction actionWrapper rawPathParams
      Left x -> Left x
