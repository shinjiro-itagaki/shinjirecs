{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Routing.Map(
  routingMap
  ) where
import qualified Controllers.ChannelsController     as ChannelsC
import qualified Controllers.HomeController         as HomeC
import qualified Controllers.InstallController      as InstallC
import qualified Controllers.ProgramsController     as ProgramsC
import qualified Controllers.ReservationsController as ReservationsC
import Routing.Class(Route(MkRoute),Path,PathPattern,PathParamList(..), toActionWrapper)
import Routing.Types(Resource(listAction ,getAction ,modifyAction ,createAction ,destroyAction))

import Network.HTTP.Types.Method(StdMethod(GET,POST,HEAD,PUT,DELETE,TRACE,CONNECT,OPTIONS,PATCH))
import Controller.Types(Action,status)
import Class.String((+++))
import Controller(defaultControllerResponse, responseBadRequest)
import Network.HTTP.Types (status200, status201, status400, status404)

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
multi _ method conn req = do
  
  return $ responseBadRequest ("urls" :: String)
