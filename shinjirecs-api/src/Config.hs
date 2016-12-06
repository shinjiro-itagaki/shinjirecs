{-# LANGUAGE OverloadedStrings #-}

module Config
where
import qualified Data.Yaml as Y (decodeFile, FromJSON, Object, Value(Object, String, Number))
import Data.Scientific (Scientific(..), coefficient)
import Data.Text (Text, pack, unpack)
import Data.List.Extra (lower) -- extra
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
envToText = Data.Text.pack . lower . show

ifnotfound :: Maybe a -> a -> a
ifnotfound (Just x) y = x
ifnotfound _        y = y


load :: ConfigFilePaths -> Env -> IO (Maybe Config)
load paths env = do
  maybe_configs <- config'
  case maybe_configs of
    Just configs -> return $ Just Config { env = env, db = configsToDBConfig' configs }
    _            -> return Nothing
  where
    config' :: IO (Maybe Y.Object)
    config' = do
      maybe_allconfigs <- loadDbFile'
      case maybe_allconfigs of
        Just allconfigs -> return $ getObject (Data.Text.pack envstr') allconfigs
        _               -> return Nothing
      
    envstr' = lower $ show env
    configsToDBConfig' :: Y.Object -> DB.Config
    configsToDBConfig' configs =
      DB.Config
      {
        DB.adapter  = (readAdapter'   "adapter"  configs) `ifnotfound` DB.SQLite3,
        DB.database = (lookupString'  "database" configs) `ifnotfound` ("db/" ++ envstr' ++ ".sqlite3"),
        DB.pool     = (lookupInt'     "pool"     configs) `ifnotfound` (5 :: Int),
        DB.timeout  = (lookupInt'     "timeout"  configs) `ifnotfound` (3000 :: Int),
        DB.host     = lookupString'  "host"     configs,
        DB.port     = lookupInteger' "port"     configs,
        DB.user     = lookupString'  "user"     configs,
        DB.password = lookupString'  "password" configs,
        DB.socket   = lookupString'  "socket"   configs
      }
    
    loadDbFile' :: IO (Maybe Y.Value)
    loadDbFile' = Y.decodeFile $ dbpath paths
    
    -- toAllConfigs' :: Maybe Y.Value -> IO (Maybe Y.Value)
    -- toAllConfigs' = maybe (fail "Invalid YAML file" :: Maybe (Y.Value)) return

    -- toConfigs' :: Maybe Y.Value -> Maybe Y.Object
    -- toConfigs' = getObject $ Data.Text.pack $ lower $ show env

    importOtherConfig' :: Y.Object -> Y.Value -> [String] -> Maybe(Y.Object)
    importOtherConfig' config allconfig imported =
      let i = M.lookup "<<" config
      in case i of
        Just (Y.String str) -> do
          if' (exists' imported (Data.Text.unpack str))
            (fail $ "value of << is duplicated : " ++ (Data.Text.unpack str))
            (do
                iconf <- getObject str allconfig
                importOtherConfig' iconf allconfig (imported ++ [(Data.Text.unpack str)])                
            )

        Nothing  -> return config
        _        -> fail "Invalid type for: <<"
      where
        exists' :: (Eq e) => [e] -> e -> Bool
        exists' [] elem = True
        exists' (e:ex) elem = e == elem || exists' ex elem
        if' :: Bool -> a -> a -> a
        if' True  x _ = x
        if' False _ y = y
        

    typeInvalid' :: Text -> IO (a)
    typeInvalid' k = fail $ "Invalid type for: " ++ show k

    valueInvalid' :: Text -> Text -> IO (a)
    valueInvalid' k t = fail $ (show t) ++ " is invalid value for: " ++ (show k)

    notFound' :: Text -> IO (a)
    notFound' k = fail $ "Not found: " ++ show k

    readAdapter' :: String -> Y.Object -> Maybe (DB.AdapterType)
    readAdapter' key config =
      let k' = Data.Text.pack key
      in case M.lookup k' config of
        Just (Y.String t) -> -- 型がstringの場合
          let adapter = DB.stringToAdapterType $ Data.Text.unpack t
          in case adapter of
            DB.Unsupported -> Nothing
            _         -> Just adapter
        _             -> Nothing

    lookupInt' :: String -> Y.Object -> Maybe (Int)
    lookupInt' key config = do
      case lookupInteger' (Data.Text.pack key) config of
        Just v -> Just (fromIntegral v)
        _      -> Nothing

    lookupInteger' :: Text -> Y.Object -> Maybe (Integer)
    lookupInteger' k config =
      case M.lookup k config of
        Just (Y.Number t) -> Just (coefficient t)
        _                 -> Nothing

    lookupText' :: Text -> Y.Object -> Maybe (Text)
    lookupText' k config =
      case M.lookup k config of
        Just (Y.String t) -> Just t
        _                 -> Nothing

    lookupString' :: Text -> Y.Object -> Maybe (String)
    lookupString' k config =
      case lookupText' k config of
        Just t -> Just (Data.Text.unpack t)
        _      -> Nothing

    -- 引数で指定したキーを持つオブジェクトを返す
    getObject :: Text -> Y.Value -> Maybe (Y.Object)
    getObject env v = do
      envs <- fromObject v
      case M.lookup env envs of
        Just (Y.Object o) -> Just o
        _                 -> Nothing

    fromObject :: Y.Value -> Maybe (Y.Object)
    fromObject m = do
      case m of
        Y.Object o -> Just o
        _          -> Nothing

    
