{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
-- {-# LANGUAGE ScopedTypeVariables #-}

module DB.Persist where
import qualified Data.Text as Text --text
import Data.Text (Text,pack) -- text
import Database.Persist -- persistent
import Database.Persist.Types -- persistent
import Database.Persist.Sql (Connection, ConnectionPool ,runSqlConn , runSqlPool, SqlPersistT, IsSqlBackend, runSqlPersistMPool) -- persistent
import Database.Persist.MySQL (withMySQLConn, createMySQLPool) -- persistent-mysql
import Database.Persist.Sqlite (withSqliteConn, createSqlitePool) -- persistent-sqlite
import Database.Persist.Postgresql (withPostgresqlConn, createPostgresqlPool) -- persistent-postgresql
import Database.Persist.Sql.Types.Internal (SqlBackend)
import Database.Persist.Class (BaseBackend, IsPersistBackend) -- persistent
import Control.Monad.Trans.Resource (runResourceT, ResourceT, MonadBaseControl) -- resourcet
import Control.Monad.Logger (runNoLoggingT, NoLoggingT) -- monad-logger
import Control.Monad.IO.Class(liftIO,MonadIO) -- base
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (ReaderT) -- mtl
import DB.Types(AdapterType(..))
import DB.Config(Config(..),configToMySQLConnectInfo,configToPgSQLConnectionString)

type Sql = SqlPersistT (ResourceT (NoLoggingT IO))

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
    MySQL      -> withMySQLConn      $ configToMySQLConnectInfo      config
    PostgreSQL -> withPostgresqlConn $ configToPgSQLConnectionString config
    SQLite3    -> withSqliteConn     $ Data.Text.pack $ database     config
    
run :: MonadIO m => ConnectionPool -> SqlPersistT IO a -> m a
run p action = liftIO $ runSqlPool action p


type Connection__ = ConnectionPool

