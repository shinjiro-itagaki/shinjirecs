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
import Controller.Types(Action,ControllerResponse(..),ParamGivenAction)
import Routing(run)

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

fireAction :: Connection -> Request -> ParamGivenAction -> IO ControllerResponse
fireAction conn req action = action conn req

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
      response <- case Routing.run req of
        Just action -> fmap toResponse $ fireAction conn req action
        Nothing -> return $ responseLBS
          status404
          [("Content-Type", "text/plain")]
          "Hello, Web!"
      respond response
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

