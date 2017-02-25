{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module App where
import Data.Maybe(maybe)
import Control.Applicative((<$>))
import Control.Exception.Base(bracket_)
import Web.Scotty (ScottyM, scotty, status, json, get, patch, delete , post, options, ActionM, param, jsonData, addroute, setHeader,middleware, RoutePattern)
import Network.HTTP.Types (status200, status201, status400, status404, StdMethod(..))
import Network.Wai (Application,Request,Response,ResponseReceived,responseLBS,Middleware)
import Network.Wai.Middleware.RequestLogger(logStdoutDev) -- wai-extra
import Network.Wai.Middleware.AddHeaders(addHeaders) -- wai-extra
import qualified Data.Text.Lazy as L
import Data.Int (Int64)
-- import           Data.Yaml (decodeFile)
import qualified Config -- (Config, ConfigFilePaths(ConfigFilePaths), db, dbpath, load, Env(..))
import qualified DB
import qualified Database.Persist as P --persistent
import qualified Database.Persist.Sql as Sql --persistent
-- import qualified Database.Persist.Store as PC --persistent
import Database.Persist.Sqlite (createSqlitePool) -- persistent-sqlite
import Control.Monad.IO.Class(liftIO,MonadIO) -- base
import Control.Monad.Logger(MonadLogger, monadLoggerLog, NoLoggingT, runNoLoggingT) -- monad-logger
import qualified Control.Monad.Trans.Class as Trans -- transformers
import Control.Monad.Trans.Resource (ResourceT) -- resourcet
import qualified Database.Persist.Class as PS
import Control.Monad.Reader (ReaderT) -- mtl
import Database.Persist.Sql.Types.Internal (SqlBackend)
import Routing

server = scotty

setCommonHeaders :: Middleware
setCommonHeaders = addHeaders [
  ("Access-Control-Allow-Origin","*")
  ]

appImpl :: Int -> Sql.ConnectionPool -> IO ()
appImpl port conn = do
  server port $ do
    middleware setCommonHeaders
    middleware logStdoutDev
    Routing.run conn
-- arg1 : port number
app :: Int -> IO ()
app = act . appImpl

act :: (Sql.ConnectionPool -> IO ()) -> IO ()
act func = config >>= maybe
  (fail "read config error") -- if Nothing
  (\config' -> (runNoLoggingT $ DB.createPool $ Config.db config') >>= func)

config :: IO (Maybe Config.Config)
config = Config.load (Config.ConfigFilePaths { Config.dbpath = "config/database.yml" }) Config.Development

migrate :: IO ()
migrate = act (\pool -> DB.run pool $ Sql.runMigration DB.migrateAll)
