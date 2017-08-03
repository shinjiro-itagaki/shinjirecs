{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
module DB.HDBC (
  Query
  ,connect
  ,Connection__  
  ,P.migrate
  ,P.Reservation(..)
  ,P.Channel(..)
  ,P.Program(..)
--  ,Record
  ) where
import DB.Config(Config(..), MySQLConnectInfo)
import qualified Database.HDBC.Sqlite3 as Sqlite3
import Database.HDBC.Sqlite3 (connectSqlite3)
import qualified Database.HDBC.MySQL as MySQL
import Database.HDBC.MySQL(connectMySQL,defaultMySQLConnectInfo)
import qualified Database.HDBC.PostgreSQL as PostgreSQL
import Database.HDBC.PostgreSQL(connectPostgreSQL)
import Database.HDBC(Statement(..))
import Database.HDBC.Types(IConnection)
import qualified DB.Persist as P
import DB.Config(Config(..),configToMySQLConnectInfo,configToPgSQLConnectionString)
import Config.Env(Env(..))
import qualified DB.Types as T
import Data.Text
import qualified Database.MySQL.Simple as SimpleMySQL -- mysql-simple
import Data.ByteString.Char8
import Database.Persist(PersistEntity) -- persistent

data Connection = MkMySQLConn MySQL.Connection | MkPostgreSQLConn PostgreSQL.Connection | MkSqlite3Conn Sqlite3.Connection
type Connection__ = Connection

castMySQLConnectInfo :: MySQLConnectInfo -> MySQL.MySQLConnectInfo
castMySQLConnectInfo from =
  MySQL.MySQLConnectInfo
  {
    MySQL.mysqlHost = SimpleMySQL.connectHost from,
    MySQL.mysqlPort = fromInteger $ toInteger $ SimpleMySQL.connectPort from,
    MySQL.mysqlUser = SimpleMySQL.connectUser from,
    MySQL.mysqlPassword = SimpleMySQL.connectPassword from,
    MySQL.mysqlUnixSocket = SimpleMySQL.connectPath from,
    MySQL.mysqlDatabase = SimpleMySQL.connectDatabase from,
    MySQL.mysqlGroup = Nothing
  }

connect :: Config -> IO Connection
connect config = 
  case adapter config of
    T.MySQL      -> do
      conn <- connectMySQL $ castMySQLConnectInfo $ configToMySQLConnectInfo config
      return $ MkMySQLConn conn
    T.PostgreSQL -> do
      conn <- connectPostgreSQL $ Data.ByteString.Char8.unpack $ configToPgSQLConnectionString config
      return $ MkPostgreSQLConn conn
    T.SQLite3    -> do
      conn <- connectSqlite3 $ database config
      return $ MkSqlite3Conn conn
    
type Query = Statement

-- insert :: PersistEntity val => val -> m (Key val)
-- update :: PersistEntity val => Key val -> [Update val] -> m ()
-- updateWhere :: PersistEntity val => [Filter val] -> [Update val] -> m ()
-- delete :: PersistEntity val => Key val -> m ()
-- deleteBy :: PersistEntity val => Unique val -> m ()
-- deleteWhere :: PersistEntity val => [Filter val] -> m ()
-- get :: PersistEntity val => Key val -> m (Maybe val)
-- getBy :: PersistEntity val => Unique val -> m (Maybe (Key val, val))
-- selectSource :: PersistEntity val => [Filter val] -> [Order val]
--   -> Int --  limit
--   -> Int --  offset
--   -> Enumerator (Key val, val) m a
-- selectKeys :: PersistEntity val => [Filter val] -> Enumerator (Key val) m a
-- count :: PersistEntity val => [Filter val] -> m Int


-- data (PersistEntity e) => Record e = MkRecord e
