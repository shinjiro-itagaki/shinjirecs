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

import Controller(ControllerAction(..), Controller(..))
import Controllers.Channels(ChannelsController, list)
import Database.Persist.Sql.Types.Internal (SqlBackend)

-- import Database.Persist.Types (PersistValue(PersistInt64)) 

server = scotty

-- toSqlKey :: ToBackendKey SqlBackend record => Int64 -> Key record
toKey :: PS.ToBackendKey Sql.SqlBackend record => L.Text -> ActionM (PS.Key record)
toKey keyname = Sql.toSqlKey <$> (param keyname) -- :: ActionM (PS.Key record)

-- MonadIO m => Sql.ConnectionPool -> Sql.SqlPersistT IO a -> m a
-- Sql.SqlPersistT IO a -> m a
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

_COMMON2 :: (Controller c) =>
  (RoutePattern -> ActionM () -> ScottyM ())
  -> Sql.ConnectionPool
  -> RoutePattern
  -> ControllerAction c
  -> ScottyM ()
_COMMON2 func conn pat act = do
  routeOPTIONS pat
  func pat $ do
    impl' act
  where
    impl' :: (Controller c) => ControllerAction c -> ActionM ()
    impl' (sym, main) = do
      (res, c) <- beforeAction sym $ new conn -- $ DBTables { channels = db }
      case res of
        True -> do
          afterAction sym =<< main c
        False -> do
          return () -- do nothing
-- MonadIO m => Sql.SqlPersistT IO a -> m a
_GET2 :: (Controller c) => Sql.ConnectionPool -> RoutePattern -> ControllerAction c -> ScottyM ()
_GET2 conn =  _COMMON2 get conn

-- _PATCH2  :: (Controller c) => RoutePattern -> ControllerAction c -> ScottyM ()
-- _PATCH2  = _COMMON2 patch
-- _POST2   :: (Controller c) => RoutePattern -> ControllerAction c -> ScottyM ()
-- _POST2   = _COMMON2 post
-- _DELETE2 :: (Controller c) => RoutePattern -> ControllerAction c -> ScottyM ()
-- _DELETE2 = _COMMON2 delete

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
--    _GET2 "/" $ Direct $ do
--      return ()
    _GET2 pool "/channels/list" Controllers.Channels.list
    _GET "/channels/list" $ do
      -- selectList :: (MonadIO m, PersistQueryRead backend, PersistRecordBackend record backend) => [Filter record] -> [SelectOpt record] -> ReaderT backend m [Entity record]
      let filter = [] :: [P.Filter DB.Channel]
          opt = [] :: [P.SelectOpt DB.Channel]
      records <- db $ map P.entityVal <$> P.selectList filter opt
      json records
      status status200
    _GET "/channels/:id" $ do
      m_record <- (findRecord db "id" :: ActionM (Maybe DB.Channel))
      status status200
      case m_record of
        Just record -> json record
        _           -> status status404
    _PATCH "/channels/:id" $ do
      key <- (toKey "id" :: ActionM (PS.Key DB.Channel))
      m_record <- (DB.findRecord db key :: ActionM (Maybe DB.Channel))
      newrec <- (jsonData :: ActionM DB.Channel)
      case m_record of
        Just record -> do
          res <- db $ P.replace key newrec
          json =<< DB.findRecord db key
          status status201
        _            -> status status404
    _POST "/channels" $ do
      newrec <- jsonData :: ActionM DB.Channel 
      key    <- db $ P.insert newrec
      json =<< DB.findRecord db key
      status status201
    _DELETE "/channels/:id" $ do
      key <- (toKey "id" :: ActionM (PS.Key DB.Channel))
      m_record <- (DB.findRecord db key :: ActionM (Maybe DB.Channel))
      case m_record of
        Just record -> do
          db $ P.delete key
          m_record2 <- DB.findRecord db key
          case m_record2 of
            Just d_record -> status status201
            _             -> status status400
        _            -> status status404      

    _GET "/install/index" $ do
      beforeStep
    _GET "/install/result_detect_channels" $ do
      beforeStep
    _GET "/install/step/1" $ do
      beforeStep
    _GET "/install/step/2" $ do
      beforeStep
    _GET "/install/step/3" $ do
      beforeStep
      
    _GET "/programs/list" $ do
      -- selectList :: (MonadIO m, PersistQueryRead backend, PersistRecordBackend record backend) => [Filter record] -> [SelectOpt record] -> ReaderT backend m [Entity record]
      let filter = [] :: [P.Filter DB.Program]
          opt = [] :: [P.SelectOpt DB.Program]
      records <- db $ map P.entityVal <$> P.selectList filter opt
      json records
      status status200
    _GET "/programs/:id" $ do
      m_record <- (findRecord db "id" :: ActionM (Maybe DB.Program))
      status status200
      case m_record of
        Just record -> json record
        _           -> status status404
    _PATCH "/programs/:id" $ do
      key <- (toKey "id" :: ActionM (PS.Key DB.Program))
      m_record <- (DB.findRecord db key :: ActionM (Maybe DB.Program))
      newrec <- (jsonData :: ActionM DB.Program)
      case m_record of
        Just record -> do
          res <- db $ P.replace key newrec
          json =<< DB.findRecord db key
          status status201
        _            -> status status404
    _POST "/programs" $ do
      newrec <- jsonData :: ActionM DB.Program
      key    <- db $ P.insert newrec
      json =<< DB.findRecord db key
      status status201
      
    _DELETE "/programs/:id" $ do
      key <- (toKey "id" :: ActionM (PS.Key DB.Program))
      m_record <- (DB.findRecord db key :: ActionM (Maybe DB.Program))
      case m_record of
        Just record -> do
          db $ P.delete key
          m_record2 <- DB.findRecord db key
          case m_record2 of
            Just d_program -> status status201
            _              -> status status400
        _            -> status status404      
      
    _GET "/reservations/list" $ do
      -- selectList :: (MonadIO m, PersistQueryRead backend, PersistRecordBackend record backend) => [Filter record] -> [SelectOpt record] -> ReaderT backend m [Entity record]
      let filter = [] :: [P.Filter DB.Reservation]
          opt = [] :: [P.SelectOpt DB.Reservation]
      records <- db $ map P.entityVal <$> P.selectList filter opt
      json records
      status status200      
    _GET "/reservations/:id" $ do
      m_record <- (findRecord db "id" :: ActionM (Maybe DB.Reservation))
      status status200
      case m_record of
        Just record -> json record
        _           -> status status404
    _PATCH "/reservations/:id" $ do
      key <- (toKey "id" :: ActionM (PS.Key DB.Reservation))
      m_record <- (DB.findRecord db key :: ActionM (Maybe DB.Reservation))
      newrec <- (jsonData :: ActionM DB.Reservation)
      case m_record of
        Just record -> do
          res <- db $ P.replace key newrec
          json =<< DB.findRecord db key
          status status201
        _            -> status status404
    _POST "/reservations" $ do
      newrec <- jsonData :: ActionM DB.Reservation
      key    <- db $ P.insert newrec
      json =<< DB.findRecord db key
      status status201
      
    _DELETE "/reservations/:id" $ do
      key <- (toKey "id" :: ActionM (PS.Key DB.Reservation))
      m_record <- (DB.findRecord db key :: ActionM (Maybe DB.Reservation))
      case m_record of
        Just record -> do
          db $ P.delete key
          m_record2 <- DB.findRecord db key
          case m_record2 of
            Just d_program -> status status201
            _              -> status status400
        _            -> status status404      


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
