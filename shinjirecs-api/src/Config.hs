{-# LANGUAGE OverloadedStrings #-}

module Config
where
import qualified Data.Yaml as Y (decodeFile, FromJSON, Object, Value(Object, String, Number))
import Data.Scientific (Scientific(..), coefficient)
import Data.Text (Text, pack, unpack)
import Data.HashMap.Strict as M
import qualified DB -- (AdapterType(..), Config(Config), adapter, database, pool, timeout) as

data Env = Production | Development | Test deriving Show

data Config = Config {
  env :: Env,
  db  :: DB.Config
  } deriving Show -- (Data, Typeable)

data ConfigFilePaths = ConfigFilePaths {
  dbpath :: FilePath
  } deriving Show

envToText :: Env -> Data.Text.Text
envToText = Data.Text.pack . show

load :: ConfigFilePaths -> Env -> IO Config
load paths env = do
  configs <- toConfigs' =<< toAllConfigs' =<< loadDbFile'
  adapter  <- readAdapter' configs
  database <- readDBPath'  configs
  pool     <- readInt' "pool"    configs 5
  timeout  <- readInt' "timeout" configs 3000
  return Config {
    env = env, 
    db = DB.Config {
      DB.adapter  = adapter,
      DB.database = database,
      DB.pool     = pool,
      DB.timeout  = timeout
    }
  }

  where
    loadDbFile' :: IO (Maybe Y.Value)
    loadDbFile' = Y.decodeFile $ dbpath paths
    
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

    readAdapter' :: Y.Object -> IO (DB.AdapterType)
    readAdapter' config =
      let k' = Data.Text.pack "adapter"
      in case M.lookup k' config of
        Just (Y.String t) ->
          let adapter = DB.stringToAdapterType $ Data.Text.unpack t
          in case adapter of
            DB.Unsupported -> valueInvalid' k' t
            _         -> return adapter
        Just _        -> typeInvalid' k'
        Nothing       -> notFound' k'

    readDBPath' :: Y.Object -> IO (Text)
    readDBPath' config = do
      return =<< lookupText' (Data.Text.pack "database") config

    readInt' :: String -> Y.Object -> Integer -> IO (Int)
    readInt' key config ifnotfound = do
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

    
