{-# LANGUAGE OverloadedStrings #-}

module Config
    ( load
    , Config
    ) where
import qualified Data.Yaml as Y (decodeFile, FromJSON, Object, Value(Object, String, Number))
import Data.Scientific (Scientific(..), coefficient)
import Data.Text (Text, pack, unpack)
import Data.HashMap.Strict as M

data Env = Production | Development | Test deriving Show
data DatabaseAdapter = MySQL | PostgreSQL | SQLite3 | UnknownDB deriving Show
data Database = Database {
  adapter  :: DatabaseAdapter,
  database :: FilePath,
  pool     :: Integer,
  timeout  :: Integer
  } deriving Show

data Config = Config {
  env :: Env,
  db  :: Database
  } deriving Show -- (Data, Typeable)

envToText :: Env -> Data.Text.Text
envToText = Data.Text.pack . show

stringToDatabaseAdapter :: String -> DatabaseAdapter
stringToDatabaseAdapter str =
  case str of
    "mysql"      -> MySQL
    "postgresql" -> PostgreSQL
    "sqlite3"    -> SQLite3
    _            -> UnknownDB


load :: FilePath -> Env -> IO Config
load path env = do
  configs <- toConfigs' =<< toAllConfigs' =<< loadFile'
  adapter  <- readAdapter' configs
  database <- readDBPath'  configs
  pool     <- readInteger' "pool"    configs 5
  timeout  <- readInteger' "timeout" configs 3000
  return Config {
    env = env, 
    db = Database {
      adapter  = adapter,
      database = database,
      pool     = pool,
      timeout  = timeout
    }
  }

  where
    loadFile' :: IO (Maybe Y.Value)
    loadFile' = Y.decodeFile path
    
    toAllConfigs' :: Maybe Y.Value -> IO (Y.Value)
    toAllConfigs' = maybe (fail "Invalid YAML file" :: IO (Y.Value)) return

    toConfigs' :: Y.Value -> IO (Y.Object)
    toConfigs' = getObject $ Data.Text.pack $ show env

    typeInvalid' :: Text -> IO (a)
    typeInvalid' k = fail $ "Invalid type for: " ++ show k

    valueInvalid' :: Text -> Text -> IO (a)
    valueInvalid' k t = fail $ (show t) ++ " is invalid value for: " ++ (show k)

    notFound' :: Text -> IO (a)
    notFound' k = fail $ "Not found: " ++ show k

    readAdapter' :: Y.Object -> IO (DatabaseAdapter)
    readAdapter' config =
      let k' = Data.Text.pack "adapter"
      in case M.lookup k' config of
        Just (Y.String t) ->
          let adapter = stringToDatabaseAdapter $ Data.Text.unpack t
          in case adapter of
            UnknownDB -> valueInvalid' k' t
            _         -> return adapter
        Just _        -> typeInvalid' k'
        Nothing       -> notFound' k'

    readDBPath' :: Y.Object -> IO (FilePath)
    readDBPath' config = do
      path <- lookupText' (Data.Text.pack "database") config
      return $ Data.Text.unpack path

    readInteger' :: String -> Y.Object -> Integer -> IO (Integer)
    readInteger' key config ifnotfound = do
      v <- lookupInteger' (Data.Text.pack key) config ifnotfound
      return $ fromIntegral v

    lookupInteger' :: Text -> Y.Object -> Integer -> IO (Integer)
    lookupInteger' k config ifnotfound =
      case M.lookup k config of
        Just (Y.Number t) -> return $ coefficient t
        Just _            -> typeInvalid' k    -- maybe not integer
        Nothing           -> return ifnotfound -- use default value

    lookupText' :: Text -> Y.Object -> IO (Text)
    lookupText' k config =
      case M.lookup k config of
        Just (Y.String t) -> return t
        Just _            -> typeInvalid' k -- maybe not string
        Nothing           -> notFound' k    -- key is not found
      
    getObject :: Text -> Y.Value -> IO (Y.Object)
    getObject env v = do
      envs <- fromObject v
      case M.lookup env envs of
        Just (Y.Object o) -> return o
        _                 -> fail $ "Invalid type for: " ++ (show env)

    fromObject :: Y.Value -> IO (Y.Object)
    fromObject m = do
      case m of
        Y.Object o -> return o
        _          -> fail "Invalid JSON format"

    
