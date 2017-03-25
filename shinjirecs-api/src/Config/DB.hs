{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Config.DB where
import qualified DB -- (AdapterType(..), Config(Config), adapter, database, pool, timeout) as
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import Config.Class(ConfigClass(..), Env,readInclude, lookupInt, lookupInteger, lookupText, lookupString)
import Control.Applicative((<|>))
import Data.List.Extra (lower) -- extra
import qualified Data.Yaml as Y (Object, Value(..))
import Data.HashMap.Strict as M
import Class.Castable(Castable(from))
import Helper((|||))

instance ConfigClass DB.Config where
  defaultConfig env = return $
    DB.Config {
    DB.adapter  = DB.SQLite3,
    DB.database = "db/" ++ from env ++ ".sqlite3",
    DB.pool     = 5,
    DB.timeout  = 3000,
    DB.host     = Nothing,
    DB.port     = Nothing,
    DB.user     = Nothing,
    DB.password = Nothing,
    DB.socket   = Nothing
    }
  
  objectToConfig obj dflt =
    DB.Config {
    DB.adapter  = DB.adapter  `or'`  readAdapter   "adapter"  ,
    DB.database = DB.database `or'`  lookupString  "database" ,
    DB.pool     = DB.pool     `or'`  lookupInt     "pool"     ,
    DB.timeout  = DB.timeout  `or'`  lookupInt     "timeout"  ,
    DB.host     = DB.host     `mor'` lookupString  "host"     ,
    DB.port     = DB.port     `mor'` lookupInteger "port"     ,
    DB.user     = DB.user     `mor'` lookupString  "user"     ,
    DB.password = DB.password `mor'` lookupString  "password" ,
    DB.socket   = DB.socket   `mor'` lookupString  "socket"   
    }
    where
      mor' = mor dflt obj
      or'  = Config.Class.or  dflt obj

readAdapter :: String -> Y.Object -> Maybe (DB.AdapterType)
readAdapter key config =
  let k' = Data.Text.pack key
  in case M.lookup k' config of
    Just (Y.String t) -> DB.stringToAdapterType $ Data.Text.unpack t
    Nothing           -> Nothing
    _                 -> fail "Invalid type (not string) for: adapter"
