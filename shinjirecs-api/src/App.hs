{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module App where
import Web.Scotty (ScottyM, scotty, status, json, get, ActionM, param)
import Network.HTTP.Types (status404, status201)
import Data.Text.Lazy (Text)
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

-- toKey :: String -> Key backend entity
-- toKey x = Key $ P.toPersistValue $ toPathPiece x

-- appImpl :: Int -> (DB.Sql a -> IO a) -> IO ()
-- db :: MonadIO m => (Sql.SqlPersistT IO a -> m a)
-- appImpl :: MonadIO m => Int -> (Sql.SqlPersistT IO a -> m a) -> IO ()
-- appImpl port db = do
-- MonadIO m => ((Sql.SqlPersistT IO a -> m a)

-- MonadIO m => Sql.ConnectionPool -> Sql.SqlPersistT IO a -> m a

appImpl :: Int -> Sql.ConnectionPool -> IO ()
appImpl port pool = do
  let db = runDB pool
  server port $ do
    get "/" $ do
      json True
      status status201
    get "/channels/:id/read" $ do
      idval <- param "id" :: ActionM Int64
      -- toSqlKey :: ToBackendKey SqlBackend record => Int64 -> Key record
      let key = Sql.toSqlKey idval :: DB.ChannelId
          query = PS.get key :: Sql.SqlPersistT IO (Maybe DB.Channel)
      m_channel <- (db query :: ActionM (Maybe DB.Channel))
      status status201
      case m_channel of
        Just channel -> json channel
        _            -> status status404
    get "/channels/:id/edit" $ do
      json True
      status status201      
    get "/channels/list" $ do
      json True
      status status201
    get "/channels/add" $ do
      json True
      status status201      
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
      json True
      status status201
    get "/reservations/:id/edit" $ do
      json True
      status status201
    get "/reservations/list" $ do
      json True
      status status201
    get "/reservations/add" $ do
      json True
      status status201

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
