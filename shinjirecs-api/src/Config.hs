{-# LANGUAGE OverloadedStrings #-}

module Config
where
import Data.Maybe (maybe)
import Data.Bool (bool)
import qualified Data.List as L
import qualified Data.Yaml as Y (decodeFile, FromJSON, Object, Value(Object, String, Number))
import Data.Scientific (Scientific(..), coefficient)
import Data.Text (Text, pack, unpack)
import Data.List.Extra (lower) -- extra
import Data.HashMap.Strict as M
import qualified DB -- (AdapterType(..), Config(Config), adapter, database, pool, timeout) as

data Env = Production | Development | Test deriving Show

data PathsConfig = PathsConfig {
  privateDir :: FilePath,
  commandDir :: FilePath
  } deriving(Show)

defaultPathsConfig :: PathsConfig
defaultPathsConfig = PathsConfig{
  privateDir = "private",
  commandDir = "private/commands"
  }

data Config = Config {
  env :: Env,
  db  :: DB.Config,
  paths :: PathsConfig
  } deriving Show -- (Data, Typeable)


data ConfigFilePaths = ConfigFilePaths {
  dbpath :: FilePath
  ,pathsPath  :: FilePath
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
    lookupInt' key config = lookupInteger' (Data.Text.pack key) config >>= return . fromInteger

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
    lookupString' k config = lookupText' k config >>= return . Data.Text.unpack
    

(<||>) :: Maybe a -> Maybe a -> Maybe a
x@(Just _) <||> _ = x
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
  
envToText :: Env -> Data.Text.Text
envToText = Data.Text.pack . lower . show

(|||) :: Maybe a -> a -> a
(|||) (Just x) _ = x
(|||) Nothing  x = x
infixl 1 |||

load :: ConfigFilePaths -> Env -> IO (Maybe Config)
load paths env =
  (Y.decodeFile $ dbpath paths)
  >>= return . (>>= (\allconfigs ->
                        getObject (Data.Text.pack $ lower $ show env) allconfigs
                        >>= (\config ->
                                Just Config { env = env,
                                              db = preDBConfig2DBConfig env $ importOtherConfig' allconfigs [] $ objectToPreDBConfig config,
                                              paths = defaultPathsConfig })))
  where
    importOtherConfig' :: Y.Value -> [String] -> PreDBConfig -> PreDBConfig
    importOtherConfig' allconfigs imported config =
      (include config
       >>= (\str -> bool (Just str) Nothing (L.elem str imported))
       >>= (\str ->
             getObject (Data.Text.pack str) allconfigs
             >>= return . (config <<<) . importOtherConfig' allconfigs (imported ++ [str]) . objectToPreDBConfig)) ||| config

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

    
