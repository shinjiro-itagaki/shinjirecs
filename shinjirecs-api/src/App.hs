{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module App where
import Data.Maybe(maybe)
import Network.Wai (Application,Request,Response,ResponseReceived,responseLBS,Middleware)
import Network.Wai.Middleware.RequestLogger(logStdoutDev) -- wai-extra
import Network.Wai.Middleware.AddHeaders(addHeaders) -- wai-extra
import qualified Config -- (Config, ConfigFilePaths(ConfigFilePaths), db, dbpath, load, Env(..))
import Config(Env(..))
import qualified DB
import qualified DB.Persist
import Database.Persist.Sql(ConnectionPool, runMigration) --persistent
import Control.Monad.Logger(MonadLogger, monadLoggerLog, NoLoggingT, runNoLoggingT) -- monad-logger
import Routing
import Server(server, middleware)

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
    
listen :: Int -> Env -> IO ()
listen port = runAction $ startServer port

runAction :: (ConnectionPool -> IO ()) -> Env -> IO ()
runAction func env = config env >>= maybe
  (fail "read config error") -- if Nothing
  (\config' -> (runNoLoggingT $ DB.Persist.createPool $ Config.db config') >>= func)

config :: Env -> IO (Maybe Config.Config)
config = Config.load Config.defaultConfigFilePaths

migrate :: Env -> IO ()
migrate = runAction (\pool -> DB.Persist.run pool $ runMigration DB.migrateAll)

runThread :: ConnectionPool -> IO ()
runThread conn = do
  putStr ""
  where
--    db' = DB.run conn 
