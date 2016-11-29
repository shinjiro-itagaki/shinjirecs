{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}

module DB where
import Control.Monad.IO.Class(liftIO,MonadIO) -- base
import Control.Monad.Logger (MonadLogger)
import Data.Text (Text) -- text
import qualified Data.Text as Text --text
import Data.Time -- time
import Data.ByteString -- bytestring
import Data.Word -- base
import Database.Persist -- persistent
import Database.Persist.Types -- persistent
import Database.Persist.Quasi (lowerCaseSettings, PersistSettings) -- persistent
import Database.Persist.Sql (Connection, ConnectionPool ,runSqlConn , runSqlPool, SqlPersistT, IsSqlBackend, runSqlPersistMPool) -- persistent
import Database.Persist.TH (share, mkMigrate, mkPersist, sqlSettings, persistFileWith) -- persistent-template

import Database.Persist.MySQL (withMySQLConn) -- persistent-mysql
import Database.Persist.Sqlite (withSqliteConn) -- persistent-sqlite
import Database.Persist.Postgresql (withPostgresqlConn) -- persistent-postgresql
import qualified Database.PostgreSQL.Simple as PgSQL -- postgresql-simple
import qualified Database.MySQL.Simple      as MySQL -- mysql-simple
import Control.Monad.Trans.Resource (runResourceT, ResourceT, MonadBaseControl) -- resourcet
import Control.Monad.Logger (runNoLoggingT, NoLoggingT) -- monad-logger
import Control.Monad.Reader (ReaderT) -- mtl
import Database.Persist.Sql.Types.Internal (SqlBackend)
import Database.Persist.Class (BaseBackend, IsPersistBackend) -- persistent

data AdapterType = MySQL | PostgreSQL | SQLite3 | Unsupported deriving Show
data Config = Config {
  -- host :: Host,
  -- port :: Int,
  -- user :: Text,
  -- password :: Text
  adapter  :: AdapterType,
  database :: Text, --FilePath,
  pool     :: Int,
  timeout  :: Int
  } deriving Show

configToMySQLConnectInfo :: Config -> MySQL.ConnectInfo
configToMySQLConnectInfo config = MySQL.defaultConnectInfo

configToPgSQLConnectInfo :: Config -> PgSQL.ConnectInfo
configToPgSQLConnectInfo config =  PgSQL.ConnectInfo
                                  { PgSQL.connectHost     = "host"
                                  , PgSQL.connectPort     = 5432
                                  , PgSQL.connectUser     = "user"
                                  , PgSQL.connectPassword = "password"
                                  , PgSQL.connectDatabase = Text.unpack $ database config
                                  }


configToPgSQLConnectionString :: Config -> ByteString
configToPgSQLConnectionString = PgSQL.postgreSQLConnectionString . configToPgSQLConnectInfo 

stringToAdapterType :: String -> AdapterType
stringToAdapterType str =
  case str of
    "mysql"      -> MySQL
    "postgresql" -> PostgreSQL
    "sqlite3"    -> SQLite3
    _            -> Unsupported

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
  $(persistFileWith lowerCaseSettings "config/models")

run pool = runSqlPersistMPool' pool
  where
    runSqlPersistMPool' pool' action' = runSqlPersistMPool action' pool'


getSQLActionRunner' :: (BaseBackend backend ~ SqlBackend, IsPersistBackend backend, MonadBaseControl IO m1, MonadBaseControl IO m) =>
  ((backend -> m1 a1) -> ResourceT (NoLoggingT m) a)
  -> ReaderT backend m1 a1
  -> m a
getSQLActionRunner' func = runNoLoggingT . runResourceT . func . runSqlConn

connect :: Config -> IO (SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a)
connect config = 
  case adapter' of
    MySQL      -> return $ getSQLActionRunner' (withMySQLConn      (configToMySQLConnectInfo config))
    PostgreSQL -> return $ getSQLActionRunner' (withPostgresqlConn (configToPgSQLConnectionString config))
    SQLite3    -> return $ getSQLActionRunner' (withSqliteConn     path')
    _          -> fail $ "invalid db adapter: " ++ (show 'adapter)
  where
    path'    = database config :: Text
    pool'    = pool     config :: Int
    adapter' = adapter  config :: AdapterType
    
