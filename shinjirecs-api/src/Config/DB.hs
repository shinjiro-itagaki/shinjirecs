{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Config.DB where
import qualified DB -- (AdapterType(..), Config(Config), adapter, database, pool, timeout) as
import qualified Data.Yaml as Y (decodeFile, FromJSON, Object, Value(Object, String, Number))
import Data.HashMap.Strict as M
import Data.Text (Text, pack, unpack)
import Config.Class(Env,(|||))
import Data.Scientific (Scientific(..), coefficient)
import Control.Applicative((<|>))
import Data.List.Extra (lower) -- extra
  
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
    readInclude' :: Y.Object -> Maybe String
    readInclude' config =
      case M.lookup "<<" configs of
        Just (Y.String s) -> Just $ Data.Text.unpack s
        Nothing           -> Nothing
        _                 -> fail "Invalid type for: <<"
    
    readAdapter' :: String -> Y.Object -> Maybe (DB.AdapterType)
    readAdapter' key config =
      let k' = Data.Text.pack key
      in case M.lookup k' config of
        Just (Y.String t) -> DB.stringToAdapterType $ Data.Text.unpack t
        Nothing           -> Nothing
        _                 -> fail "Invalid type (not string) for: adapter"

    lookupInt' :: String -> Y.Object -> Maybe (Int)
    lookupInt' key config = lookupInteger' (Data.Text.pack key) config >>= return . fromInteger

    lookupInteger' :: Text -> Y.Object -> Maybe Integer
    lookupInteger' k config =
      case M.lookup k config of
        Just (Y.Number t) -> Just (coefficient t)
        Nothing           -> Nothing
        _                 -> fail $ "Invalid type (not integer) for: " ++ (Data.Text.unpack k)

    lookupText' :: Text -> Y.Object -> Maybe Text
    lookupText' k config =
      case M.lookup k config of
        Just (Y.String t) -> Just t
        Nothing           -> Nothing
        _                 -> fail $ "Invalid type (not string) for: " ++ (Data.Text.unpack k)

    lookupString' :: Text -> Y.Object -> Maybe String
    lookupString' k config = lookupText' k config >>= return . Data.Text.unpack

    
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
