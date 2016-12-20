{-# LANGUAGE OverloadedStrings #-}
module App where
import Web.Scotty (ScottyM, scotty, status, json, get)
import Network.HTTP.Types (status404, status201)
-- import           Data.Yaml (decodeFile)
import qualified Config -- (Config, ConfigFilePaths(ConfigFilePaths), db, dbpath, load, Env(..))
import qualified DB
import qualified Database.Persist.Sql as Sql --persistent
import Database.Persist.Sqlite (createSqlitePool) -- persistent-sqlite
import Control.Monad.IO.Class(liftIO,MonadIO) -- base
import Control.Monad.Logger(MonadLogger, monadLoggerLog, NoLoggingT) -- monad-logger
import qualified Control.Monad.Trans.Class as Trans -- transformers
import Control.Monad.Trans.Resource (ResourceT) -- resourcet

server = scotty

appImpl :: Int -> (DB.Sql a -> IO a) -> IO ()
appImpl port db = do
  server port $ do
    get "/" $ do
      json True
      status status201
    get "/channels/edit" $ do
      json True
      status status201
    get "/channels/list" $ do
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
    get "/programs/index" $ do
      json True
      status status201
    get "/programs/list" $ do
      json True
      status status201
    get "/reservations/edit" $ do
      json True
      status status201
    get "/reservations/index" $ do
      json True
      status status201      
    get "/reservations/list" $ do
      json True
      status status201
    get "/reservations/new" $ do
      json True
      status status201

app :: Int -> IO ()
app port = do
  appImpl port =<< db


config :: IO (Maybe Config.Config)
config = Config.load Config.ConfigFilePaths {
  Config.dbpath = "config/database.yml"
} Config.Development

db :: IO (DB.Sql a -> IO a)
db = do
  maybe_config <- config
  case maybe_config of
    Just config -> do
      DB.connect (Config.db config)
    _           -> fail "read config error"

migrate :: IO ()
migrate = do
  db_ <- db
  db_ (Sql.runMigration DB.migrateAll)
