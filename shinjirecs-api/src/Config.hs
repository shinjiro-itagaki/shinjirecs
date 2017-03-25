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
import Config.Class(ConfigClass(..),Env)
import qualified DB
import Config.DB
import Config.Paths(PathsConfig(..))
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
  mdbconf <- readYaml (dbpath defaultConfigFilePaths) env
  pconf <- defaultConfig env :: IO PathsConfig
  rconf <- defaultConfig env :: IO ReservationConfig
  return $ mdbconf >>= return . (\dbconf -> Config {
    env = env,
    db = dbconf,
    paths = pconf,
    reservation = rconf
    })
