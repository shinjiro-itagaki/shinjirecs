{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Config
  ( module Config,
    module Config.Class,
    module Config.Paths,
    module Config.Reservation,
  ) where
import Data.Maybe (maybe)
import Data.Bool (bool)
import qualified Data.List as L
import qualified Data.Yaml as Y (decodeFile, FromJSON, Object, Value(Object, String, Number))
import Data.Text (Text, pack, unpack)
import Data.List.Extra (lower) -- extra
import Data.HashMap.Strict as M
import Data.Word (Word)
import Config.Class(ConfigClass,Env,(|||))
import qualified DB
import Config.DB
import Config.Paths(PathsConfig(..),defaultPathsConfig)
import Config.Reservation(ReservationConfig(..),defaultReservationConfig,ReservationCommandArg(..))

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

    
