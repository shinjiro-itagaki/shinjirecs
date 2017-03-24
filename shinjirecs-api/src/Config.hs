{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Config where
import Control.Applicative((<|>))
import Data.Maybe (maybe)
import Data.Bool (bool)
import qualified Data.List as L
import qualified Data.Yaml as Y (decodeFile, FromJSON, Object, Value(Object, String, Number))
import Data.Scientific (Scientific(..), coefficient)
import Data.Text (Text, pack, unpack)
import Data.List.Extra (lower) -- extra
import Data.HashMap.Strict as M
import qualified DB -- (AdapterType(..), Config(Config), adapter, database, pool, timeout) as
import Data.Word (Word)
import System.Directory(getCurrentDirectory)
import System.Argv0(getArgv0)
import System.FilePath.Posix((</>),takeDirectory) -- filepath
import Class.Castable(Castable(from))

data Env = Production | Development | Test deriving Show

instance Castable String Env where
  from "production" = Production
  from "test"       = Test
  from _            = Development

data PathsConfig = PathsConfig {
  baseDir    :: FilePath,
  privateDir :: FilePath,
  commandDir :: FilePath,
  videoFilesDir :: FilePath
  } deriving(Show)

defaultPathsConfig :: IO PathsConfig
defaultPathsConfig = do
  curr' <- getCurrentDirectory
  baseDir' <- return . (curr' </>) . takeDirectory . show =<< getArgv0
  return PathsConfig {
    baseDir    = baseDir',
    privateDir = "private",
    commandDir = "private/commands",
    videoFilesDir = "private/videofiles"
  }

data ReservationCommandArg = ArgDevice | ArgChannel | ArgDurationSec | ArgDestFilePath deriving (Show)

data ReservationConfig = ReservationConfig {
  marginStart :: Word,
  script :: String,
  args :: [ReservationCommandArg]
  } deriving (Show)

defaultReservationConfig :: ReservationConfig
defaultReservationConfig = ReservationConfig {
    marginStart = 3,
    script = "recpt1.sh",
    args = [ArgDevice,ArgChannel,ArgDurationSec,ArgDestFilePath]
    }

data Config = Config {
  env :: Env,
  db  :: DB.Config,
  paths :: PathsConfig,
  reservation :: ReservationConfig
  } deriving Show -- (Data, Typeable)


data ConfigFilePaths = ConfigFilePaths {
  dbpath :: FilePath
  ,pathsPath  :: FilePath
  } deriving Show

defaultConfigFilePaths =
  ConfigFilePaths
  {
    dbpath = "config/database.yml",
    pathsPath = "config/paths.yml"
  }

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
  
envToText :: Env -> Data.Text.Text
envToText = Data.Text.pack . lower . show

(|||) :: Maybe a -> a -> a
(|||) (Just x) _ = x
(|||) Nothing  x = x
infixl 1 |||

load :: ConfigFilePaths -> Env -> IO (Maybe Config)
load paths env = do
  pconf <- defaultPathsConfig
  (Y.decodeFile $ dbpath paths)
    >>= return . (>>= (\allconfigs -> getObject (Data.Text.pack $ lower $ show env) allconfigs
                        >>= (\config ->
                                Just Config { env = env,
                                              db = preDBConfig2DBConfig env $ importOtherConfig' allconfigs [] $ objectToPreDBConfig config,
                                              paths = pconf ,
                                              reservation = defaultReservationConfig})))
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

    
