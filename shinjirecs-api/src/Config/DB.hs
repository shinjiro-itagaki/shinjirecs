{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Config.DB where
import qualified DB -- (AdapterType(..), Config(Config), adapter, database, pool, timeout) as
import Data.Text (Text, pack, unpack)
import Config.Class(Env,(|||), readInclude, lookupInt, lookupInteger, lookupText, lookupString)
import Control.Applicative((<|>))
import Data.List.Extra (lower) -- extra
import qualified Data.Yaml as Y (Object, Value(..))
import Data.HashMap.Strict as M
  
data PreDBConfig = PreDBConfig {
  include  :: Maybe String,
  host     :: Maybe String,
  socket   :: Maybe String,
  port     :: Maybe Integer,
  user     :: Maybe String,
  password :: Maybe String,
  adapter  :: Maybe DB.AdapterType,
  database :: Maybe String,
  pool     :: Maybe Int,
  timeout  :: Maybe Int
  }

objectToPreDBConfig :: Y.Object -> PreDBConfig
objectToPreDBConfig configs =
  PreDBConfig
  {
    include  = readInclude'              configs,
    adapter  = readAdapter'   "adapter"  configs,
    database = lookupString'  "database" configs,
    pool     = lookupInt'     "pool"     configs,
    timeout  = lookupInt'     "timeout"  configs,
    host     = lookupString'  "host"     configs,
    port     = lookupInteger' "port"     configs,
    user     = lookupString'  "user"     configs,
    password = lookupString'  "password" configs,
    socket   = lookupString'  "socket"   configs    
  }
  where
    readInclude'   = readInclude
    readAdapter'   = readAdapter 
    lookupInt'     = lookupInt
    lookupInteger' = lookupInteger
    lookupText'    = lookupText
    lookupString'  = lookupString

    
(<<<) :: PreDBConfig -> PreDBConfig -> PreDBConfig
(>>>) :: PreDBConfig -> PreDBConfig -> PreDBConfig
(<<<) l r = PreDBConfig {
  include  = or' include,
  host     = or' host,
  socket   = or' socket,
  port     = or' port,
  user     = or' user,
  password = or' password,
  adapter  = or' adapter,
  database = or' database,
  pool     = or' pool,
  timeout  = or' timeout
  }
  where
    or' f = (f l) <|> (f r)

(>>>) l r = r <<< l

preDBConfig2DBConfig :: Env -> PreDBConfig -> DB.Config
preDBConfig2DBConfig env_ pre =
  DB.Config
  {
    DB.adapter  = (adapter  pre) ||| DB.SQLite3,
    DB.database = (database pre) ||| ("db/" ++ envstr' ++ ".sqlite3"),
    DB.pool     = (pool     pre) ||| (5 :: Int),
    DB.timeout  = (timeout  pre) ||| (3000 :: Int),
    DB.host     = (host     pre),
    DB.port     = (port     pre),
    DB.user     = (user     pre),
    DB.password = (password pre),
    DB.socket   = (socket   pre)
  }
  where
    envstr' = lower $ show env_

readAdapter :: String -> Y.Object -> Maybe (DB.AdapterType)
readAdapter key config =
  let k' = Data.Text.pack key
  in case M.lookup k' config of
    Just (Y.String t) -> DB.stringToAdapterType $ Data.Text.unpack t
    Nothing           -> Nothing
    _                 -> fail "Invalid type (not string) for: adapter"
