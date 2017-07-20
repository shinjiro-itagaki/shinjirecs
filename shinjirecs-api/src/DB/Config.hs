{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
module DB.Config where
import Data.ByteString
import qualified Database.PostgreSQL.Simple as PgSQL -- postgresql-simple
import qualified Database.MySQL.Simple      as MySQL -- mysql-simple
import DB.Types(AdapterType(..))
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

type MySQLConnectInfo = MySQL.ConnectInfo
configToMySQLConnectInfo :: Config -> MySQLConnectInfo
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

migrationFilePath = "config/models"
