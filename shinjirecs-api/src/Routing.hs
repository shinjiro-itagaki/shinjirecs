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
import Controller(Action(..), ActionType, ControllerResponse(..) ,defaultControllerResponse)

import Routing.Class

notFound, badRequest :: ActionType ()
notFound conn req _ = return $ defaultControllerResponse {
  status = status404
  }

badRequest conn req _ = return $ defaultControllerResponse {
  status = status400
  }
notImplemented = error "not implemented"

ptn2params :: PathPattern -> ByteString -> RawPathParams
ptn2params ptn path = notImplemented

data Route = MkRoute [StdMethod] PathPattern Action

matchStdMethods :: Route -> StdMethod -> Bool
matchStdMethods (MkRoute ys _ _ ) x = elem x ys

matchPath :: Route -> Path -> Bool
matchPath (MkRoute _ ptn _ ) path =
  notImplemented
  where
--    ptndiv = 

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

run :: Request -> Maybe Action
run req = do
  case parseMethod $ requestMethod req of
    Left _ -> Nothing
    Right stdmethod -> findAction stdmethod (rawPathInfo req)
