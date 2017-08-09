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
import qualified DB
--import qualified DB.Persist
--import Database.Persist.Sql(ConnectionPool, runMigration) --persistent
--import Control.Monad.Logger(MonadLogger, monadLoggerLog, NoLoggingT, runNoLoggingT) -- monad-logger
import Routing(run)
-- import Server(server, middleware)

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

-- type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app :: Application
app request respond = do
  respond $ responseLBS
    status200
    [("Content-Type", "text/plain")]
    "Hello, Web!"

listen :: Int -> Env -> IO ()
-- listen port = runAction $ startServer port
-- listen port env = return ()
listen port env = Network.Wai.Handler.Warp.run port app

{-
runAction :: (ConnectionPool -> IO ()) -> Env -> IO ()
runAction func env = Config.loadDefault env >>= maybe
  (fail "read config error") -- if Nothing
  (\config' -> (runNoLoggingT $ DB.Persist.createPool $ Config.db config') >>= func)

runThread :: ConnectionPool -> IO ()
runThread conn = do
  putStr ""
  where
--    db' = DB.run conn 
-}
migrate :: Env -> IO ()
migrate env = Config.loadDefault env >>= maybe
  (fail "read config error") -- if Nothing
  (\config' -> DB.migrate $ Config.db config')

