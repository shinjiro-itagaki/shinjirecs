{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
-- {-# LANGUAGE ScopedTypeVariables #-}

module DB where
import Control.Monad.IO.Class(liftIO,MonadIO) -- base
import Control.Monad.Logger (MonadLogger)
import Data.Text (Text,pack) -- text
import qualified Data.Text as Text --text
import Data.Time -- time
import Data.ByteString -- bytestring
import Data.Word -- base
import Data.Pool(Pool) -- base
import Database.Persist -- persistent
import Database.Persist.Types -- persistent
import Database.Persist.Quasi (lowerCaseSettings, PersistSettings) -- persistent
import Database.Persist.Sql (Connection, ConnectionPool ,runSqlConn , runSqlPool, SqlPersistT, IsSqlBackend, runSqlPersistMPool) -- persistent
import Database.Persist.TH (share, mkMigrate, mkPersist, sqlSettings, persistFileWith, derivePersistField) -- persistent-template

import Database.Persist.MySQL (withMySQLConn, createMySQLPool) -- persistent-mysql
import Database.Persist.Sqlite (withSqliteConn, createSqlitePool) -- persistent-sqlite
import Database.Persist.Postgresql (withPostgresqlConn, createPostgresqlPool) -- persistent-postgresql
import qualified Database.PostgreSQL.Simple as PgSQL -- postgresql-simple
import qualified Database.MySQL.Simple      as MySQL -- mysql-simple
import Control.Monad.Trans.Resource (runResourceT, ResourceT, MonadBaseControl) -- resourcet
import Control.Monad.Logger (runNoLoggingT, NoLoggingT) -- monad-logger
import Control.Monad.Reader (ReaderT) -- mtl
import Database.Persist.Sql.Types.Internal (SqlBackend)
import Database.Persist.Class (BaseBackend, IsPersistBackend) -- persistent
import qualified Database.Persist.Class as PS
import DB.Status(ReservationState(..))

type Sql = SqlPersistT (ResourceT (NoLoggingT IO))

-- make following datas
-- Channel
-- Reservation
-- Program
-- ... and others
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
  $(persistFileWith lowerCaseSettings "config/models")

data AdapterType = MySQL | PostgreSQL | SQLite3 deriving Show
data Config = Config {
  host     :: Maybe String,
  socket   :: Maybe String,
  port     :: Maybe Integer,
  user     :: Maybe String,
  password :: Maybe String,
  adapter  :: AdapterType,
  database :: String, --FilePath,
  pool     :: Int,
  timeout  :: Int
  } deriving Show

(.||.) :: Maybe a -> a -> a
(Just x) .||. y = x
Nothing  .||. y = y

configToMySQLConnectInfo :: Config -> MySQL.ConnectInfo
configToMySQLConnectInfo config = MySQL.defaultConnectInfo
                                  { MySQL.connectHost     = (host config)               .||. "localhost"
                                  , MySQL.connectPort     = fromInteger $ (port config) .||. 5432
                                  , MySQL.connectUser     = (user config)               .||. "root" 
                                  , MySQL.connectPassword = (password config)           .||. ""
                                  , MySQL.connectPath     = (socket config)             .||. ""
                                  , MySQL.connectDatabase = database config
                                  }

configToPgSQLConnectInfo :: Config -> PgSQL.ConnectInfo
configToPgSQLConnectInfo config =  PgSQL.ConnectInfo
                                  { PgSQL.connectHost     = (host config)               .||. "localhost"
                                  , PgSQL.connectPort     = fromInteger $ (port config) .||. 5432
                                  , PgSQL.connectUser     = (user config)               .||. "root" 
                                  , PgSQL.connectPassword = (password config)           .||. ""
                                  , PgSQL.connectDatabase = database config
                                  }

configToPgSQLConnectionString :: Config -> ByteString
configToPgSQLConnectionString = PgSQL.postgreSQLConnectionString . configToPgSQLConnectInfo 

stringToAdapterType :: String -> Maybe AdapterType
stringToAdapterType str =
  case str of
    "mysql"      -> return MySQL
    "postgresql" -> return PostgreSQL
    "sqlite3"    -> return SQLite3
    _            -> Nothing

getSQLActionRunner' :: (BaseBackend backend ~ SqlBackend, IsPersistBackend backend, MonadBaseControl IO m1, MonadBaseControl IO m) =>
  ((backend -> m1 a1) -> ResourceT (NoLoggingT m) a)
  -> ReaderT backend m1 a1
  -> m a
getSQLActionRunner' func = runNoLoggingT . runResourceT . func . runSqlConn

createPool :: (MonadIO m, MonadBaseControl IO m, MonadLogger m) => Config -> m ConnectionPool
createPool config = 
  case adapter config of
    MySQL      -> createMySQLPool      (configToMySQLConnectInfo      config) pool'
    PostgreSQL -> createPostgresqlPool (configToPgSQLConnectionString config) pool'
    SQLite3    -> createSqlitePool     (Data.Text.pack $ database     config) pool'
  where
    pool' = pool config :: Int

connect :: Config -> IO (Sql a -> IO a)
connect config = 
  return $ getSQLActionRunner' $
  case adapter config of
    MySQL      -> mysql'
    PostgreSQL -> pgsql'
    SQLite3    -> sqlit'
  where
    mysql' = withMySQLConn      $ configToMySQLConnectInfo      config
    pgsql' = withPostgresqlConn $ configToPgSQLConnectionString config
    sqlit' = withSqliteConn     $ Data.Text.pack $ database     config
    
run :: MonadIO m => ConnectionPool -> SqlPersistT IO a -> m a
run p action = liftIO $ runSqlPool action p
