{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module App where
import Control.Applicative((<$>))
import Web.Scotty (ScottyM, scotty, status, json, get, patch, delete , post, ActionM, param, jsonData)
import Network.HTTP.Types (status200, status201, status400, status404)
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

-- runDB :: MonadIO m => Sql.ConnectionPool -> Sql.SqlPersistT IO a -> m a
appImpl :: Int -> Sql.ConnectionPool -> IO ()
appImpl port pool = do
  let db = runDB pool
  server port $ do
    get "/" $ do
      json True
      status status200
    get "/channels/list" $ do
      -- selectList :: (MonadIO m, PersistQueryRead backend, PersistRecordBackend record backend) => [Filter record] -> [SelectOpt record] -> ReaderT backend m [Entity record]
      let filter = [] :: [P.Filter DB.Channel]
          opt = [] :: [P.SelectOpt DB.Channel]
      records <- db $ map P.entityVal <$> P.selectList filter opt
      json records
      status status200
    get "/channels/:id" $ do
      m_record <- (findRecord db "id" :: ActionM (Maybe DB.Channel))
      status status200
      case m_record of
        Just record -> json record
        _           -> status status404
    patch "/channels/:id" $ do
      key <- (toKey "id" :: ActionM (PS.Key DB.Channel))
      m_record <- (DB.findRecord db key :: ActionM (Maybe DB.Channel))
      newrec <- (jsonData :: ActionM DB.Channel)
      case m_record of
        Just record -> do
          res <- db $ P.replace key newrec
          json =<< DB.findRecord db key
          status status201
        _            -> status status404
    post "/channels" $ do
      newrec <- jsonData :: ActionM DB.Channel 
      key    <- db $ P.insert newrec
      json =<< DB.findRecord db key
      status status201
      
    delete "/channels/:id" $ do
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
      
    get "/install/index" $ do
      json True
      status status201
    get "/install/result_detect_channels" $ do
      json True
      status status201
    get "/install/step1" $ do
      json True
      status status201
    get "/install/step2" $ do
      json True
      status status201
    get "/install/step3" $ do
      json True
      status status201
    get "/install/step4" $ do
      json True
      status status201
    get "/install/step5" $ do
      json True
      status status201
      
    get "/programs/list" $ do
      -- selectList :: (MonadIO m, PersistQueryRead backend, PersistRecordBackend record backend) => [Filter record] -> [SelectOpt record] -> ReaderT backend m [Entity record]
      let filter = [] :: [P.Filter DB.Program]
          opt = [] :: [P.SelectOpt DB.Program]
      records <- db $ map P.entityVal <$> P.selectList filter opt
      json records
      status status200      
    get "/programs/:id" $ do
      m_record <- (findRecord db "id" :: ActionM (Maybe DB.Program))
      status status200
      case m_record of
        Just record -> json record
        _           -> status status404
    patch "/programs/:id" $ do
      key <- (toKey "id" :: ActionM (PS.Key DB.Program))
      m_record <- (DB.findRecord db key :: ActionM (Maybe DB.Program))
      newrec <- (jsonData :: ActionM DB.Program)
      case m_record of
        Just record -> do
          res <- db $ P.replace key newrec
          json =<< DB.findRecord db key
          status status201
        _            -> status status404
    post "/programs" $ do
      newrec <- jsonData :: ActionM DB.Program
      key    <- db $ P.insert newrec
      json =<< DB.findRecord db key
      status status201
      
    delete "/programs/:id" $ do
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
      

    get "/reservations/list" $ do
      -- selectList :: (MonadIO m, PersistQueryRead backend, PersistRecordBackend record backend) => [Filter record] -> [SelectOpt record] -> ReaderT backend m [Entity record]
      let filter = [] :: [P.Filter DB.Reservation]
          opt = [] :: [P.SelectOpt DB.Reservation]
      records <- db $ map P.entityVal <$> P.selectList filter opt
      json records
      status status200      
    get "/reservations/:id" $ do
      m_record <- (findRecord db "id" :: ActionM (Maybe DB.Reservation))
      status status200
      case m_record of
        Just record -> json record
        _           -> status status404
    patch "/reservations/:id" $ do
      key <- (toKey "id" :: ActionM (PS.Key DB.Reservation))
      m_record <- (DB.findRecord db key :: ActionM (Maybe DB.Reservation))
      newrec <- (jsonData :: ActionM DB.Reservation)
      case m_record of
        Just record -> do
          res <- db $ P.replace key newrec
          json =<< DB.findRecord db key
          status status201
        _            -> status status404
    post "/reservations" $ do
      newrec <- jsonData :: ActionM DB.Reservation
      key    <- db $ P.insert newrec
      json =<< DB.findRecord db key
      status status201
      
    delete "/reservations/:id" $ do
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

runDB :: MonadIO m => Sql.ConnectionPool -> Sql.SqlPersistT IO a -> m a
runDB p action = liftIO $ Sql.runSqlPool action p

migrate :: IO ()
migrate = act migrate'
  where
    migrate' :: Sql.ConnectionPool -> IO ()
    migrate' pool = runDB pool $ Sql.runMigration DB.migrateAll
