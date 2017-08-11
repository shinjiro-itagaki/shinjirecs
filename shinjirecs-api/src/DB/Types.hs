{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric   #-}
module DB.Types where
import Database.Persist.TH
import qualified Data.Aeson as A
import qualified Data.Text as T
import GHC.Generics
import Database.Persist.Sql(PersistFieldSql(sqlType))
import Database.Persist(SqlType(SqlInt32))

data ChannelType =  GR | BS | CS deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)
derivePersistField "ChannelType"

instance A.ToJSON ChannelType
instance A.FromJSON ChannelType

data AdapterType = MySQL | PostgreSQL | SQLite3 deriving Show
stringToAdapterType :: String -> Maybe AdapterType
stringToAdapterType str =
  case str of
    "mysql"      -> return MySQL
    "postgresql" -> return PostgreSQL
    "sqlite3"    -> return SQLite3
    _            -> Nothing
