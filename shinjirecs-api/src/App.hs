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

appImpl :: Int -> (Sql.SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a) -> IO ()
appImpl port db = do
  server port $ do
    get "/" $ do
      json True
      status status201

app :: Int -> IO ()
app port = do
  maybe_config <- Config.load Config.ConfigFilePaths {
    Config.dbpath = "config/database.yml"
  } Config.Development
  case maybe_config of
    Just config -> do
      db <- (DB.connect (Config.db config))
      appImpl port db
    _           -> fail "read config error"
