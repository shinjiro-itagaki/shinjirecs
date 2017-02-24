{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module App where
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

toKey :: PS.ToBackendKey Sql.SqlBackend record => L.Text -> ActionM (PS.Key record)
toKey keyname = Sql.toSqlKey <$> (param keyname) -- :: ActionM (PS.Key record)

findRecord :: PS.ToBackendKey Sql.SqlBackend record => (Sql.SqlPersistT IO (Maybe record) -> ActionM (Maybe record)) -> L.Text -> ActionM (Maybe record)
findRecord db keyname = do
  key <- toKey keyname -- :: ActionM (PS.Key record)  
  DB.findRecord db key

setCommonHeaders :: Middleware
setCommonHeaders = addHeaders [
  ("Access-Control-Allow-Origin","*")
  ]

--setDefaultStatus :: Middleware
--setDefaultStatus = 

routeOPTIONS :: RoutePattern -> ScottyM ()
routeOPTIONS pat = do
  options pat $ do
    status status200

-- data BeforeAction = ScottyM ()

beforeActions :: RoutePattern -> ActionM Bool
beforeActions pat = do
  status status200
  return True

afterActions :: RoutePattern -> ActionM ()
afterActions pat = do
  return ();

_COMMON :: (RoutePattern -> ActionM () -> ScottyM ())
  -> RoutePattern
--  -> ControllerAction c
  -> ActionM ()
  -> ScottyM ()
_COMMON  func pat act = do
  routeOPTIONS pat
  func pat $ do
    res <- beforeActions pat
    impl' res
  where
--    impl' True (ControllerAction sym f) = do
    impl' True = do
      -- f new
      afterActions pat
    impl' False = do
      json False
  
_GET    = _COMMON get
_PATCH  = _COMMON patch
_POST   = _COMMON post
_DELETE = _COMMON delete

_COMMON2 :: (Controller sym c, ActionSymbol sym) =>
  (RoutePattern -> ActionM () -> ScottyM ())
  -> Sql.ConnectionPool
  -> RoutePattern
  -> ControllerAction c sym
  -> ScottyM ()
_COMMON2 func conn pat act = do
  routeOPTIONS pat
  func pat $ do
    impl' act
  where
    impl' :: (Controller sym c, ActionSymbol sym) => ControllerAction c sym -> ActionM ()
    impl' (sym, main) = do
      (res, c) <- beforeAction sym $ new sym conn
      case res of
        True -> do
          afterAction sym =<< main c
        False -> do
          return () -- do nothing
-- MonadIO m => Sql.SqlPersistT IO a -> m a

_GET2, _PATCH2, _POST2, _DELETE2 :: (Controller sym c, ActionSymbol sym) => Sql.ConnectionPool -> RoutePattern -> ControllerAction c sym -> ScottyM ()
_GET2    = _COMMON2 get
_PATCH2  = _COMMON2 patch
_POST2   = _COMMON2 post
_DELETE2 = _COMMON2 delete

beforeStep :: ActionM ()
beforeStep = do
  json True
  status status201

-- runDB :: MonadIO m => Sql.ConnectionPool -> Sql.SqlPersistT IO a -> m a
appImpl :: Int -> Sql.ConnectionPool -> IO ()
appImpl port pool = do
  let db = runDB pool -- MonadIO m => Sql.SqlPersistT IO a -> m a
  server port $ do
    middleware setCommonHeaders
    middleware logStdoutDev
    _GET2    pool "/channels/list" ChannelsC.list
    _GET2    pool "/channels/:id"  ChannelsC.get
    _PATCH2  pool "/channels/:id"  ChannelsC.modify
    _POST2   pool "/channels"      ChannelsC.create
    _DELETE2 pool "/channels/:id"  ChannelsC.destroy
    
    _GET2    pool "/install/index" InstallC.index
    _GET2    pool "/install/result_detect_channels" InstallC.resultDetectChannels
    _GET2    pool "/install/step1" InstallC.step1
    _GET2    pool "/install/step2" InstallC.step2
    _GET2    pool "/install/step3" InstallC.step3
    
    _GET2    pool "/programs/list" ProgramsC.list
    _GET2    pool "/programs/:id"  ProgramsC.get
    _PATCH2  pool "/programs/:id"  ProgramsC.modify
    _POST2   pool "/programs"      ProgramsC.create
    _DELETE2 pool "/programs/:id"  ProgramsC.destroy
    
    _GET2    pool "/reservations/list" ReservationsC.list
    _GET2    pool "/reservations/:id"  ReservationsC.get
    _PATCH2  pool "/reservations/:id"  ReservationsC.modify
    _POST2   pool "/reservations"      ReservationsC.create
    _DELETE2 pool "/reservations/:id"  ReservationsC.destroy    

-- arg1 : port number
app :: Int -> IO ()
app = act . appImpl

act :: (Sql.ConnectionPool -> IO ()) -> IO ()
act func = do
  maybe_config <- config
  case maybe_config of
    Just config -> do
      pool <- runNoLoggingT $ DB.createPool (Config.db config)
      func pool
    _           -> fail "read config error"  
  
config :: IO (Maybe Config.Config)
config = Config.load Config.ConfigFilePaths {
  Config.dbpath = "config/database.yml"
} Config.Development

-- class (PS.ToBackendKey SqlBackend record) => Table record where
--   db   :: (Monad m) => (Sql.SqlPersistT IO record -> m record)
--   find :: (Monad m) => PS.Key record -> m (Maybe a)
  
runDB :: MonadIO m => Sql.ConnectionPool -> Sql.SqlPersistT IO a -> m a
runDB p action = liftIO $ Sql.runSqlPool action p

migrate :: IO ()
migrate = act migrate'
  where
    migrate' :: Sql.ConnectionPool -> IO ()
    migrate' pool = runDB pool $ Sql.runMigration DB.migrateAll
