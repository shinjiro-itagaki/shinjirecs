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
    

(<||>) :: Maybe a -> Maybe a -> Maybe a
x@(Just _) <||> y = x
Nothing    <||> y = y

(<<<) :: PreDBConfig -> PreDBConfig -> PreDBConfig
(>>>) :: PreDBConfig -> PreDBConfig -> PreDBConfig
(<<<) l r = PreDBConfig {
  include  = (include  l) <||> (include  r),
  host     = (host     l) <||> (host     r),
  socket   = (socket   l) <||> (socket   r),
  port     = (port     l) <||> (port     r),
  user     = (user     l) <||> (user     r),
  password = (password l) <||> (password r),
  adapter  = (adapter  l) <||> (adapter  r),
  database = (database l) <||> (database r),
  pool     = (pool     l) <||> (pool     r),
  timeout  = (timeout  l) <||> (timeout  r)
  }

(>>>) l r = r <<< l

preDBConfig2DBConfig :: Env -> PreDBConfig -> DB.Config
preDBConfig2DBConfig env_ pre =
  DB.Config
  {
    DB.adapter  = (adapter  pre) `ifnotfound` DB.SQLite3,
    DB.database = (database pre) `ifnotfound` ("db/" ++ envstr' ++ ".sqlite3"),
    DB.pool     = (pool     pre) `ifnotfound` (5 :: Int),
    DB.timeout  = (timeout  pre) `ifnotfound` (3000 :: Int),
    DB.host     = (host     pre),
    DB.port     = (port     pre),
    DB.user     = (user     pre),
    DB.password = (password pre),
    DB.socket   = (socket   pre)
  }
  where
    envstr' = lower $ show env_
  
envToText :: Env -> Data.Text.Text
envToText = Data.Text.pack . lower . show

ifnotfound :: Maybe a -> a -> a
ifnotfound (Just x) y = x
ifnotfound _        y = y


load :: ConfigFilePaths -> Env -> IO (Maybe Config)
load paths env = do
  maybe_allconfigs <- loadDbFile'
  case maybe_allconfigs of
    Just allconfigs ->
      let maybe_config = getObject (Data.Text.pack envstr') allconfigs
      in
        case maybe_config of
          Just config -> return $ Just Config { env = env, db = configsToDBConfig' allconfigs config }
          _           -> return Nothing
    _                 -> return Nothing
    
  where
    envstr' = lower $ show env
    configsToDBConfig' :: Y.Value -> Y.Object -> DB.Config
    configsToDBConfig' allconfigs config = preDBConfig2DBConfig env $ importOtherConfig' allconfigs [] $ objectToPreDBConfig config
    
    loadDbFile' :: IO (Maybe Y.Value)
    loadDbFile' = Y.decodeFile $ dbpath paths
    
    importOtherConfig' :: Y.Value -> [String] -> PreDBConfig -> PreDBConfig
    importOtherConfig' allconfigs imported config =
      case include config of
        Just str ->
          if' (exists' imported str)
            (config)
            (
              let maybe_conf_obj = getObject (Data.Text.pack str) allconfigs -- :: Maybe (Y.Object)
              in case maybe_conf_obj of
                Just conf_obj -> config <<< importOtherConfig' allconfigs (imported ++ [str]) (objectToPreDBConfig conf_obj)
                _             -> config
            )
        _  -> config
      where
        exists' :: (Eq e) => [e] -> e -> Bool
        exists' [] elem = False
        exists' (e:ex) elem = (e == elem) || exists' ex elem
        if' :: Bool -> a -> a -> a
        if' True  x _ = x
        if' False _ y = y
        

    typeInvalid' :: Text -> IO (a)
    typeInvalid' k = fail $ "Invalid type for: " ++ show k

    valueInvalid' :: Text -> Text -> IO (a)
    valueInvalid' k t = fail $ (show t) ++ " is invalid value for: " ++ (show k)

    notFound' :: Text -> IO (a)
    notFound' k = fail $ "Not found: " ++ show k


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

    
