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
import Network.HTTP.Types.Status(status200)
import Network.HTTP.Types.Method(StdMethod(..), parseMethod)
--import Network.Wai.Middleware.RequestLogger(logStdoutDev) -- wai-extra
-- import Network.Wai.Middleware.AddHeaders(addHeaders) -- wai-extra
import qualified Config -- (Config, ConfigFilePaths(ConfigFilePaths), db, dbpath, load, Env(..))
import Config.Env(Env(..))
import DB
import Controller(ControllerResponse(..))
import Routing(run,ActionSet)

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

fireActionSet :: Connection -> Request -> ActionSet -> ControllerResponse
fireActionSet conn req (action, sym) =
  action conn req sym

toResponse :: ControllerResponse -> Response
toResponse res = responseLBS
  (status res)
  [("Content-Type", contentType res)]
  (body res)

app :: Env -> Application
app env req respond = do
  maybeConf <- Config.loadDefault env
  case maybeConf of
    Just conf -> do
      conn <- (DB.connect $ Config.db conf)
      respond $ case Routing.run req of
        Just actionset -> toResponse $ fireActionSet conn req actionset
        Nothing -> responseLBS
          status404
          [("Content-Type", "text/plain")]
          "Hello, Web!"          
    Nothing -> respond $ responseLBS
      status500
      [("Content-Type", "text/plain")]
      "Connect database failed!"

listen :: Int -> Env -> IO ()
listen port env = Network.Wai.Handler.Warp.run port $ app env

migrate :: Env -> IO ()
migrate env = Config.loadDefault env >>= maybe
  (fail "read config error") -- if Nothing
  (\config' -> DB.migrate $ Config.db config')

