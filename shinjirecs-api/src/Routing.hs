{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Controller(ContentType ,Body ,ControllerResponse(..) ,Symbol ,Action ,defaultControllerResponse)

notFoundFunc :: Connection -> Request -> IO ControllerResponse
notFoundFunc conn req = return $ defaultControllerResponse {
  status = status404
  }

notFound = notFoundFunc

type Path = ByteString
type PathPattern = ByteString
type Route = ([StdMethod], PathPattern, Action)

routingMap :: [Route]
routingMap = [
  (  [GET],    "/channels/list",                  ChannelsC.list ) -- ChannelsC.list
  ,( [GET],    "/channels/:id" ,                  notFound ) -- ChannelsC.get
  ,( [PATCH],  "/channels/:id",                   notFound ) -- ChannelsC.modify
  ,( [POST],   "/channels",                       notFound ) -- ChannelsC.create
  ,( [DELETE], "/channels/:id",                   notFound ) -- ChannelsC.destroy
    
  ,( [GET],    "/install/index",                  notFound ) -- InstallC.index
  ,( [GET],    "/install/result_detect_channels", notFound ) -- InstallC.resultDetectChannels
  ,( [GET],    "/install/step1",                  notFound ) -- (InstallC.step 1)
  ,( [GET],    "/install/step2",                  notFound ) -- (InstallC.step 2)
  ,( [GET],    "/install/step3",                  notFound ) -- (InstallC.step 3)
    
  ,( [GET],    "/programs/list",                  notFound ) -- ProgramsC.list
  ,( [GET],    "/programs/:id",                   notFound ) -- ProgramsC.get
  ,( [PATCH],  "/programs/:id",                   notFound ) -- ProgramsC.modify
  ,( [POST],   "/programs",                       notFound ) -- ProgramsC.create
  ,( [DELETE], "/programs/:id",                   notFound ) -- ProgramsC.destroy
    
  ,( [GET],    "/reservations/list",              notFound ) -- ReservationsC.list
  ,( [GET],    "/reservations/:id",               notFound ) -- ReservationsC.get
  ,( [PATCH],  "/reservations/:id",               notFound ) -- ReservationsC.modify
  ,( [POST],   "/reservations",                   notFound ) -- ReservationsC.create
  ,( [DELETE], "/reservations/:id",               notFound ) -- ReservationsC.destroy
  ]

findRoute :: StdMethod -> Path -> Maybe Action
findRoute stdmethod path =
  case res of
    Just (_ ,_ ,action) -> Just action
    Nothing             -> Nothing
  where
    res = find (\(stdmethods, path', action') -> elem stdmethod stdmethods && path == path') routingMap

run :: Request -> Maybe Action
run req = do
  case parseMethod $ requestMethod req of
    Left _ -> Nothing
    Right stdmethod -> findRoute stdmethod (rawPathInfo req)
