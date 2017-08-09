{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Routing where
-- import Server (ServerM, status, get, patch, delete, post, options, ActionM, RoutePattern)
import Network.Wai (Request(..))
import qualified Controller
-- import Controller(ControllerAction(..), Controller(..), ActionSymbol(..))
-- import Controller(Controller(..), ActionSymbol(..))
import Controller(Controller(..))
import qualified Controllers.ChannelsController     as ChannelsC
import qualified Controllers.InstallController      as InstallC
import qualified Controllers.ProgramsController     as ProgramsC
import qualified Controllers.ReservationsController as ReservationsC

-- import Database.Persist.Sql(ConnectionPool) --persistent
import DB(Connection)

import Network.HTTP.Types (status200, status201, status400, status404)
import Network.HTTP.Types.Method(StdMethod(GET,POST,HEAD,PUT,DELETE,TRACE,CONNECT,OPTIONS,PATCH), parseMethod)
import Data.List(find)
import Data.ByteString(ByteString)
import Controller(ContentType ,Body ,ControllerResponse ,Action)

{-

defRoute :: (Controller c) =>
  (RoutePattern -> ActionM () -> ServerM ())
  -> Connection
  -> RoutePattern
  -> ControllerAction c
  -> ServerM ()
defRoute func conn pat act = do
  options pat (status status200) -- add OPTIONS
  func pat $ Controller.run conn act

-}

notFound :: Connection -> Request -> ControllerResponse
notFound conn req = ("application/json", "")

type Path = ByteString
type PathPattern = ByteString
type Route = ([StdMethod], PathPattern, Action)

routingMap :: [Route]
routingMap = [
  (  [GET],    "/channels/list",                  notFound ) -- ChannelsC.list
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

run :: Request -> Action
run req = do
  case parseMethod $ requestMethod req of
    Left _ -> notFound
    Right stdmethod
      -> case findRoute stdmethod (rawPathInfo req) of
           Just action -> action
           Nothing     -> notFound
    
  -- return ()
  {- 
  _GET    "/channels/list" ChannelsC.list
  _GET    "/channels/:id"  ChannelsC.get
  _PATCH  "/channels/:id"  ChannelsC.modify
  _POST   "/channels"      ChannelsC.create
  _DELETE "/channels/:id"  ChannelsC.destroy
    
  _GET    "/install/index" InstallC.index
  _GET    "/install/result_detect_channels" InstallC.resultDetectChannels
  _GET    "/install/step1" (InstallC.step 1)
  _GET    "/install/step2" (InstallC.step 2)
  _GET    "/install/step3" (InstallC.step 3)
    
  _GET    "/programs/list" ProgramsC.list
  _GET    "/programs/:id"  ProgramsC.get
  _PATCH  "/programs/:id"  ProgramsC.modify
  _POST   "/programs"      ProgramsC.create
  _DELETE "/programs/:id"  ProgramsC.destroy
    
  _GET    "/reservations/list" ReservationsC.list
  _GET    "/reservations/:id"  ReservationsC.get
  _PATCH  "/reservations/:id"  ReservationsC.modify
  _POST   "/reservations"      ReservationsC.create
  _DELETE "/reservations/:id"  ReservationsC.destroy
-}
  where
    {-
    _GET, _PATCH, _POST, _DELETE :: (Controller c) => RoutePattern -> ControllerAction c -> ServerM ()
    _GET    = defRoute get    conn
    _PATCH  = defRoute patch  conn
    _POST   = defRoute post   conn
    _DELETE = defRoute delete conn
-}
    
    
