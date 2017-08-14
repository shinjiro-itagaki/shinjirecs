{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module App (
  App.migrate
  ,listen
  )where
import Data.Maybe(maybe)
import Network.Wai (Application,Request(..),Response,ResponseReceived,responseLBS,Middleware)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types
import Network.HTTP.Types.Status(Status,status200,status405,status405)
import Network.HTTP.Types.Method(StdMethod(..), parseMethod)
--import Network.Wai.Middleware.RequestLogger(logStdoutDev) -- wai-extra
-- import Network.Wai.Middleware.AddHeaders(addHeaders) -- wai-extra
import qualified Config -- (Config, ConfigFilePaths(ConfigFilePaths), db, dbpath, load, Env(..))
import Config.Env(Env(..))
import DB
import Controller.Types(Action,ControllerResponse(..),ParamGivenAction)
import Routing.Class(RouteNotFound(PathNotFound, PathFoundButMethodUnmatch, UnknownMethod))
import Routing(run)
import qualified Data.ByteString.Lazy as L
import Class.String(toByteStringL)

{-
setCommonHeaders :: Middleware
setCommonHeaders = addHeaders [
  ("Access-Control-Allow-Origin","*")
  ]

startServer :: Int -> ConnectionPool -> IO ()
startServer port conn = do
  runThread conn
  server port $ do
    middleware setCommonHeaders
    middleware logStdoutDev
    Routing.run conn
-}

toResponse :: ControllerResponse -> Response
toResponse res = responseLBS
  (status res)
  [("Content-Type", contentType res)]
  (body res)


mkResponse :: Status -> L.ByteString -> Response
mkResponse status msg = responseLBS status [("Content-Type", "text/plain")] msg

response500 :: L.ByteString -> Response
response500 msg = mkResponse status500 msg

response404 :: L.ByteString -> Response
response404 path = mkResponse status404 $ L.concat [path, " is not found"]

response_PathNotFound :: L.ByteString -> Response
response_PathNotFound path = mkResponse status404 $ L.concat ["match route ",path," is not found"]

response_UnsupportedMethod :: L.ByteString -> Response
response_UnsupportedMethod method = mkResponse status405 $ L.concat [method , " is unsupported"]

response_NotAllowedMethod :: L.ByteString -> L.ByteString -> Response
response_NotAllowedMethod method path = mkResponse status405 $ L.concat [method, " is not allowed on " , path]

app :: Env -> Application
app env req respond = do
  maybeConf <- Config.loadDefault env
  case maybeConf of
    Just conf -> do
      conn <- (DB.connect $ Config.db conf)
      case Routing.run req of
        Right action -> action conn req >>= respond . toResponse
        Left x -> respond $ case x of
          PathNotFound                  -> response404                path'
          PathFoundButMethodUnmatch msg -> response_NotAllowedMethod  method' path'
          UnknownMethod                 -> response_UnsupportedMethod method'
    Nothing -> respond $ response500 "load config error!"
  where
    method' = toByteStringL $ requestMethod req
    path'   = toByteStringL $ rawPathInfo req
    

listen :: Int -> Env -> IO ()
listen port env = Network.Wai.Handler.Warp.run port $ app env

migrate :: Env -> IO ()
migrate env = Config.loadDefault env >>= maybe
  (fail "read config error") -- if Nothing
  (\config' -> DB.migrate $ Config.db config')

