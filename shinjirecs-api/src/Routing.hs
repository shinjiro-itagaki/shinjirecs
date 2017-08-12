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

import Controller.Types(Action(..), ActionType, ControllerResponse(..) ,ParamGivenActionType)
import Controller(defaultControllerResponse)

import Routing.Class(Path,PathPattern,RawPathParamKey,RawPathParamVal,RawPathParam,RawPathParams,PathParamList(..),toParamGivenActionType)
import Class.String(StringClass(..))
import Data.List.Split
import Data.Maybe(fromJust,isJust)
import Data.Word(Word8)

notFound, badRequest :: ActionType ()
notFound conn req _ = return $ defaultControllerResponse {
  status = status404
  }

badRequest conn req _ = return $ defaultControllerResponse {
  status = status400
  }

data Route = MkRoute [StdMethod] PathPattern Action

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

pathToPieces :: Path -> [String]
pathToPieces path = Prelude.map toString $ filter (not . Data.ByteString.Char8.null) $ Data.ByteString.Char8.split '/' $ toByteString path

getMaybeRawPathParamsFromPatternAndPath :: PathPattern -> Path -> Maybe RawPathParams
getMaybeRawPathParamsFromPatternAndPath ptn path =
  matchPathElements ptn_pieces' path_pieces' (Just [])
  where
    ptn_pieces'  = pathToPieces ptn
    path_pieces' = pathToPieces path

getMaybeRawPathParams :: Route -> Path -> Maybe RawPathParams
getMaybeRawPathParams (MkRoute _ ptn _ ) path = getMaybeRawPathParamsFromPatternAndPath ptn path

matchPath :: Route -> Path -> Bool
matchPath r p = isJust $ getMaybeRawPathParams r p
    
getRawPathParams :: Route -> Path -> RawPathParams
getRawPathParams r p = case getMaybeRawPathParams r p of
  Just params -> params
  Nothing     -> []

routingMap :: [Route]
routingMap = map (\(x,y,z) -> MkRoute x y z) [
   ( [GET],    "/channels/list",                  toAction ChannelsC.list )
  ,( [GET],    "/channels/:id",                   toAction ChannelsC.get  ) -- ChannelsC.get
  ,( [PATCH],  "/channels/:id",                   toAction notFound ) -- ChannelsC.modify
  ,( [POST],   "/channels",                       toAction notFound ) -- ChannelsC.create
  ,( [DELETE], "/channels/:id",                   toAction notFound ) -- ChannelsC.destroy
    
  ,( [GET],    "/install/index",                  toAction notFound ) -- InstallC.index
  ,( [GET],    "/install/result_detect_channels", toAction notFound ) -- InstallC.resultDetectChannels
  ,( [GET],    "/install/step1",                  toAction notFound ) -- (InstallC.step 1)
  ,( [GET],    "/install/step2",                  toAction notFound ) -- (InstallC.step 2)
  ,( [GET],    "/install/step3",                  toAction notFound ) -- (InstallC.step 3)
    
  ,( [GET],    "/programs/list",                  toAction notFound ) -- ProgramsC.list
  ,( [GET],    "/programs/:id",                   toAction notFound ) -- ProgramsC.get
  ,( [PATCH],  "/programs/:id",                   toAction notFound ) -- ProgramsC.modify
  ,( [POST],   "/programs",                       toAction notFound ) -- ProgramsC.create
  ,( [DELETE], "/programs/:id",                   toAction notFound ) -- ProgramsC.destroy
    
  ,( [GET],    "/reservations/list",              toAction notFound ) -- ReservationsC.list
  ,( [GET],    "/reservations/:id",               toAction notFound ) -- ReservationsC.get
  ,( [PATCH],  "/reservations/:id",               toAction notFound ) -- ReservationsC.modify
  ,( [POST],   "/reservations",                   toAction notFound ) -- ReservationsC.create
  ,( [DELETE], "/reservations/:id",               toAction notFound ) -- ReservationsC.destroy  
  ]
  
findAction :: StdMethod -> Path -> Maybe Action
findAction stdmethod path = case res of
  Just (MkRoute _ _ action) -> Just action
  Nothing                   -> Nothing
  where
    res = find (\route -> (matchStdMethods route stdmethod) && (matchPath route path)) routingMap

run :: Request -> Maybe ParamGivenActionType
run req = do
  case parseMethod $ requestMethod req of
    Left _ -> Nothing
    Right stdmethod -> case findAction stdmethod (rawPathInfo req) of
      Just action -> Just $ toParamGivenActionType action []
      Nothing -> Nothing
