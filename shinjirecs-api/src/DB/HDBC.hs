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
--  Query
  connect
  ,Connection__  
  ,P.migrate
  ,P.Reservation(..)
  ,P.Channel(..)
  ,P.Program(..)
  ,insert
  ,update
  ,updateWhere
  ,delete
  ,deleteBy
  ,deleteWhere
  ,get
  ,getBy
  ,select
  ,selectKeys
  ,DB.HDBC.count
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
import DB.Class(Key,Record(..),Filter,Order)
import Data.Enumerator(Enumerator)

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
    
-- type Query = Statement

notImplemented = error "not implemented"

insert :: (Record val, Monad m) => val -> m (Key val)
insert val = notImplemented -- return $ toRecordKey 0

update :: (Record val, Monad m) => Key val -> [Update val] -> m ()
update key updates = notImplemented

updateWhere :: (Record val, Monad m) => [Filter val] -> [Update val] -> m ()
updateWhere filters updates = notImplemented

delete :: (Record val, Monad m) => Key val -> m ()
delete key = notImplemented

deleteBy :: (Record val, Monad m) => Unique val -> m ()
deleteBy unique = notImplemented

deleteWhere :: (Record val, Monad m) => [Filter val] -> m ()
deleteWhere filters = notImplemented

get :: Record val => Key val -> m (Maybe val)
get key = notImplemented

getBy :: (Record val, Monad m) => Unique val -> m (Maybe (Key val, val))
getBy val = notImplemented

select :: (Record val, Monad m) =>
  [Filter val]
  -> [Order val]
  -> Int --  limit
  -> Int --  offset
  -> Enumerator (Key val, val) m a
select filters orders limit offset = notImplemented

selectKeys :: (Record val, Monad m) => [Filter val] -> Enumerator (Key val) m a
selectKeys filters = notImplemented

count :: (Record val, Monad m) => [Filter val] -> m Int
count filters = notImplemented
