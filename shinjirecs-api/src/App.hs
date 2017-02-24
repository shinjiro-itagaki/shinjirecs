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

import Controller(ControllerAction(..), Controller(..), ActionSymbol(..))

import qualified Controllers.Channels     as ChannelsC
import qualified Controllers.Install      as InstallC
import qualified Controllers.Programs     as ProgramsC
import qualified Controllers.Reservations as ReservationsC

server = scotty

setCommonHeaders :: Middleware
setCommonHeaders = addHeaders [
  ("Access-Control-Allow-Origin","*")
  ]

_COMMON :: (Controller c) =>
  (RoutePattern -> ActionM () -> ScottyM ())
  -> Sql.ConnectionPool
  -> RoutePattern
  -> ControllerAction c
  -> ScottyM ()
_COMMON func conn pat act = do
  options pat (status status200)
  func pat (impl' act)
  where
    impl' :: (Controller c) => ControllerAction c -> ActionM ()
    impl' (sym, main) = do
      (res, c) <- beforeAction sym $ new conn
      case res of
        True -> do
          afterAction sym =<< main c
        False -> do
          return () -- do nothing

-- runDB :: MonadIO m => Sql.ConnectionPool -> Sql.SqlPersistT IO a -> m a
appImpl :: Int -> Sql.ConnectionPool -> IO ()
appImpl port pool = do
  server port $ do
    middleware setCommonHeaders
    middleware logStdoutDev
    _GET    "/channels/list" ChannelsC.list
    _GET    "/channels/:id"  ChannelsC.get
    _PATCH  "/channels/:id"  ChannelsC.modify
    _POST   "/channels"      ChannelsC.create
    _DELETE "/channels/:id"  ChannelsC.destroy
    
    _GET    "/install/index" InstallC.index
    _GET    "/install/result_detect_channels" InstallC.resultDetectChannels
    _GET    "/install/step1" (InstallC.step 1)
    _GET    "/install/step2" (InstallC.step 2)
    _GET    "/install/step3" (InstallC.step 3)
    
    _GET    "/programs/list" ProgramsC.list
    _GET    "/programs/:id"  ProgramsC.get
    _PATCH  "/programs/:id"  ProgramsC.modify
    _POST   "/programs"      ProgramsC.create
    _DELETE "/programs/:id"  ProgramsC.destroy
    
    _GET    "/reservations/list" ReservationsC.list
    _GET    "/reservations/:id"  ReservationsC.get
    _PATCH  "/reservations/:id"  ReservationsC.modify
    _POST   "/reservations"      ReservationsC.create
    _DELETE "/reservations/:id"  ReservationsC.destroy
  where
    _GET, _PATCH, _POST, _DELETE :: (Controller c) => RoutePattern -> ControllerAction c -> ScottyM ()
    _GET    = _COMMON get    pool
    _PATCH  = _COMMON patch  pool
    _POST   = _COMMON post   pool
    _DELETE = _COMMON delete pool

-- arg1 : port number
app :: Int -> IO ()
app = act . appImpl

act :: (Sql.ConnectionPool -> IO ()) -> IO ()
act func = config >>= maybe
  (fail "read config error") -- if Nothing
  (\config' -> (runNoLoggingT $ DB.createPool $ Config.db config') >>= func)

config :: IO (Maybe Config.Config)
config = Config.load (Config.ConfigFilePaths { Config.dbpath = "config/database.yml" }) Config.Development

runDB :: MonadIO m => Sql.ConnectionPool -> Sql.SqlPersistT IO a -> m a
runDB p action = liftIO $ Sql.runSqlPool action p

migrate :: IO ()
migrate = act (\pool -> runDB pool $ Sql.runMigration DB.migrateAll)
