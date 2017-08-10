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
import Controller(ContentType ,Body ,ControllerResponse(..) ,Symbol ,Action, defaultControllerResponse)
import Data.Map(Map(..), empty, fromList)

notFound, badRequest :: Connection -> Request -> IO ControllerResponse
notFound conn req = return $ defaultControllerResponse {
  status = status404
  }

badRequest conn req = return $ defaultControllerResponse {
  status = status400
  }

type Path = ByteString
type PathPattern = ByteString
type Route = ([StdMethod], PathPattern, Action)

-- type Action = (Connection -> Request -> IO ControllerResponse)

type RawPathParamKey = String
type RawPathParamVal = String
type RawPathParam    = (RawPathParamKey, RawPathParamVal)
type RawPathParams   = [RawPathParam]

notImplemented = error "not implemented"

ptn2params :: PathPattern -> ByteString -> RawPathParams
ptn2params ptn path = notImplemented

data Route2 = MkRoute_S    [StdMethod] PathPattern (Connection -> Request -> String                 -> IO ControllerResponse)
            | MkRoute_I    [StdMethod] PathPattern (Connection -> Request -> Int                    -> IO ControllerResponse)
            | MkRoute_SMap [StdMethod] PathPattern (Connection -> Request -> Map String String      -> IO ControllerResponse)
            | MkRoute_IMap [StdMethod] PathPattern (Connection -> Request -> Map String Int         -> IO ControllerResponse)
            | MkRoute_SS   [StdMethod] PathPattern (Connection -> Request -> (String,String)        -> IO ControllerResponse)
            | MkRoute_II   [StdMethod] PathPattern (Connection -> Request -> (Int,Int)              -> IO ControllerResponse)
            | MkRoute_IS   [StdMethod] PathPattern (Connection -> Request -> (Int,String)           -> IO ControllerResponse)
            | MkRoute_SI   [StdMethod] PathPattern (Connection -> Request -> (String,Int)           -> IO ControllerResponse)
            | MkRoute_III  [StdMethod] PathPattern (Connection -> Request -> (Int,Int,Int)          -> IO ControllerResponse)
            | MkRoute_SII  [StdMethod] PathPattern (Connection -> Request -> (String,Int,Int)       -> IO ControllerResponse)
            | MkRoute_ISI  [StdMethod] PathPattern (Connection -> Request -> (Int,String,Int)       -> IO ControllerResponse)
            | MkRoute_IIS  [StdMethod] PathPattern (Connection -> Request -> (Int,Int,String)       -> IO ControllerResponse)
            | MkRoute_SSI  [StdMethod] PathPattern (Connection -> Request -> (String,String,Int)    -> IO ControllerResponse)
            | MkRoute_ISS  [StdMethod] PathPattern (Connection -> Request -> (Int,String,String)    -> IO ControllerResponse)
            | MkRoute_SIS  [StdMethod] PathPattern (Connection -> Request -> (String,Int,String)    -> IO ControllerResponse)
            | MkRoute_SSS  [StdMethod] PathPattern (Connection -> Request -> (String,String,String) -> IO ControllerResponse)

class PathParamList a where
  path2args :: RawPathParams -> Either RawPathParams a
  path2args others = Left others  
  mkRoute :: [StdMethod] -> PathPattern -> (Connection -> Request -> a ->  IO ControllerResponse) -> Route2
  
-- for MkRoute_S
instance PathParamList String where
  path2args (x:yy) = Right $ read $ snd x
  mkRoute = MkRoute_S

-- for MkRoute_I
instance PathParamList Int where
  path2args (x:yy) = Right $ read $ snd x
  mkRoute = MkRoute_I

-- for MkRoute_SMap
instance PathParamList (Map String String) where
  path2args = Right . fromList
  mkRoute = MkRoute_SMap

-- for MkRoute_IMap
instance PathParamList (Map String Int) where
  path2args = Right . fromList . map (\(k,v) -> (k, read v))
  mkRoute = MkRoute_IMap

-- for MkRoute_SS
instance PathParamList (String ,String) where
  path2args (x0:x1:xx) = Right (read $ snd x0, read $ snd x1)
  mkRoute = MkRoute_SS
  
-- for MkRoute_II
instance PathParamList (Int ,Int) where
  path2args (x0:x1:xx) = Right (read $ snd x0, read $ snd x1)
  mkRoute = MkRoute_II
  
-- for MkRoute_IS
instance PathParamList (Int ,String) where
  path2args (x0:x1:xx) = Right (read $ snd x0, read $ snd x1)
  mkRoute = MkRoute_IS
  
-- for MkRoute_SI
instance PathParamList (String , Int) where
  path2args (x0:x1:xx) = Right (read $ snd x0, read $ snd x1)
  mkRoute = MkRoute_SI
  
-- for MkRoute_III
instance PathParamList (Int , Int , Int) where
  path2args (x0:x1:x2:xx) = Right (read $ snd x0, read $ snd x1, read $ snd x2)
  mkRoute = MkRoute_III
  
-- for MkRoute_SII
instance PathParamList (String , Int , Int) where
  path2args (x0:x1:x2:xx) = Right (read $ snd x0, read $ snd x1, read $ snd x2)
  mkRoute = MkRoute_SII
  
-- for MkRoute_ISI
instance PathParamList (Int,String,Int) where
  path2args (x0:x1:x2:xx) = Right (read $ snd x0, read $ snd x1, read $ snd x2)
  mkRoute = MkRoute_ISI
  
-- for MkRoute_IIS
instance PathParamList (Int,Int,String) where
  path2args (x0:x1:x2:xx) = Right (read $ snd x0, read $ snd x1, read $ snd x2)
  mkRoute = MkRoute_IIS
  
-- for MkRoute_SSI
instance PathParamList (String,String,Int) where
  path2args (x0:x1:x2:xx) = Right (read $ snd x0, read $ snd x1, read $ snd x2)
  mkRoute = MkRoute_SSI  
  
-- for MkRoute_ISS
instance PathParamList (Int,String,String) where
  path2args (x0:x1:x2:xx) = Right (read $ snd x0, read $ snd x1, read $ snd x2)
  mkRoute = MkRoute_ISS  
  
-- for MkRoute_SIS
instance PathParamList (String,Int,String) where
  path2args (x0:x1:x2:xx) = Right (read $ snd x0, read $ snd x1, read $ snd x2)
  mkRoute = MkRoute_SIS
  
-- for MkRoute_SSS
instance PathParamList (String,String,String) where
  path2args (x0:x1:x2:xx) = Right (read $ snd x0, read $ snd x1, read $ snd x2)
  mkRoute = MkRoute_SSS  

  
routingMap2 :: [Route2]
routingMap2 = [
  mkRoute [GET] "/channels/list" ChannelsC.list2
  ]

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

findAction :: StdMethod -> Path -> Maybe Action
findAction stdmethod path =
  case res of
    Just (_ ,ptn ,action) -> Just action
    Nothing               -> Nothing
  where
    res = find (\(stdmethods, path', action') -> elem stdmethod stdmethods && path == path') routingMap

run :: Request -> Maybe Action
run req = do
  case parseMethod $ requestMethod req of
    Left _ -> Nothing
    Right stdmethod -> findAction stdmethod (rawPathInfo req)
