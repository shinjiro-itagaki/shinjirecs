{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module App where
import Data.Maybe(maybe)
import Web.Scotty (scotty, middleware)
import Network.Wai (Application,Request,Response,ResponseReceived,responseLBS,Middleware)
import Network.Wai.Middleware.RequestLogger(logStdoutDev) -- wai-extra
import Network.Wai.Middleware.AddHeaders(addHeaders) -- wai-extra
import qualified Config -- (Config, ConfigFilePaths(ConfigFilePaths), db, dbpath, load, Env(..))
import qualified DB
import Database.Persist.Sql(ConnectionPool, runMigration) --persistent
import Control.Monad.Logger(MonadLogger, monadLoggerLog, NoLoggingT, runNoLoggingT) -- monad-logger
import Routing

server = scotty

setCommonHeaders :: Middleware
setCommonHeaders = addHeaders [
  ("Access-Control-Allow-Origin","*")
  ]

appImpl :: Int -> ConnectionPool -> IO ()
appImpl port conn = do
  server port $ do
    middleware setCommonHeaders
    middleware logStdoutDev
    Routing.run conn
-- arg1 : port number
app :: Int -> IO ()
app = act . appImpl

act :: (ConnectionPool -> IO ()) -> IO ()
act func = config >>= maybe
  (fail "read config error") -- if Nothing
  (\config' -> (runNoLoggingT $ DB.createPool $ Config.db config') >>= func)

config :: IO (Maybe Config.Config)
config = Config.load (Config.ConfigFilePaths { Config.dbpath = "config/database.yml" }) Config.Development

migrate :: IO ()
migrate = act (\pool -> DB.run pool $ runMigration DB.migrateAll)
