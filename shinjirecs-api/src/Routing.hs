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
import Controller(Action(..), ActionType, ContentType ,Body ,ControllerResponse(..) ,Symbol ,defaultControllerResponse)
import Data.Map(Map(..), empty, fromList)

notFound, badRequest :: ActionType ()
notFound conn req _ = return $ defaultControllerResponse {
  status = status404
  }

badRequest conn req _ = return $ defaultControllerResponse {
  status = status400
  }

type Path = ByteString
type PathPattern = ByteString
type RawPathParamKey = String
type RawPathParamVal = String
type RawPathParam    = (RawPathParamKey, RawPathParamVal)
type RawPathParams   = [RawPathParam]

notImplemented = error "not implemented"

ptn2params :: PathPattern -> ByteString -> RawPathParams
ptn2params ptn path = notImplemented

data Route = MkRoute [StdMethod] PathPattern Action

matchStdMethods :: Route -> StdMethod -> Bool
matchStdMethods (MkRoute ys _ _ ) x = elem x ys

matchPath :: Route -> Path -> Bool
matchPath (MkRoute _ ptn _ ) path = notImplemented

class PathParamList a where
  path2args :: RawPathParams -> Either RawPathParams a
  path2args others = Left others
  toAction :: ActionType a -> Action

path2arg' :: (Read a) => RawPathParams -> Either RawPathParams a
path2arg' (x:yy) = Right $ read $ snd x
path2arg' others = Left others

path2argsXX' :: (Read a, Read b) => RawPathParams -> Either RawPathParams (a,b)
path2argsXX' (x0:x1:xx) = Right (read $ snd x0, read $ snd x1)
path2argsXX' others = Left others

path2argsXXX' :: (Read a, Read b, Read c) => RawPathParams -> Either RawPathParams (a,b,c)
path2argsXXX' (x0:x1:x2:xx) = Right (read $ snd x0, read $ snd x1, read $ snd $ x2)
path2argsXXX' others = Left others

instance PathParamList () where
  path2args = path2arg'
  toAction = Action_N
  
instance PathParamList String where
  path2args = path2arg'
  toAction = Action_S

instance PathParamList Int where
  path2args = path2arg'
  toAction = Action_I

instance PathParamList (Map String String) where
  path2args = Right . fromList
  toAction = Action_SMap

instance PathParamList (Map String Int) where
  path2args = Right . fromList . map (\(k,v) -> (k, read v))
  toAction = Action_IMap

instance PathParamList (String ,String) where
  path2args = path2argsXX'
  toAction = Action_SS
  
instance PathParamList (Int ,Int) where
  path2args = path2argsXX'
  toAction = Action_II
  
instance PathParamList (Int ,String) where
  path2args = path2argsXX'
  toAction = Action_IS
  
instance PathParamList (String , Int) where
  path2args = path2argsXX'
  toAction = Action_SI
  
instance PathParamList (Int , Int , Int) where
  path2args = path2argsXXX'
  toAction = Action_III
  
instance PathParamList (String , Int , Int) where
  path2args = path2argsXXX'
  toAction = Action_SII
  
instance PathParamList (Int,String,Int) where
  path2args = path2argsXXX'
  toAction = Action_ISI
  
instance PathParamList (Int,Int,String) where
  path2args = path2argsXXX'
  toAction = Action_IIS
  
instance PathParamList (String,String,Int) where
  path2args = path2argsXXX'
  toAction = Action_SSI  
  
instance PathParamList (Int,String,String) where
  path2args = path2argsXXX'
  toAction = Action_ISS  
  
instance PathParamList (String,Int,String) where
  path2args = path2argsXXX'
  toAction = Action_SIS
  
instance PathParamList (String,String,String) where
  path2args = path2argsXXX'
  toAction = Action_SSS  

routingMap :: [Route]
routingMap = map (\(x,y,z) -> MkRoute x y z) [
   ( [GET],    "/channels/list",                  toAction ChannelsC.list )
  ,( [GET],    "/channels/:id",                   toAction notFound ) -- ChannelsC.get
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
