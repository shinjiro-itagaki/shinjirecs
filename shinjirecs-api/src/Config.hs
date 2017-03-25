{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Config
  ( module Config,
    module Config.Class,
    module Config.Paths,
    module Config.Reservation,
  ) where
import Data.Maybe (maybe,fromJust)
import Data.Bool (bool)
import qualified Data.List as L
import qualified Data.Yaml as Y (decodeFile, FromJSON, Object, Value(Object, String, Number))
import Data.Text (Text, pack, unpack)
import Data.List.Extra (lower) -- extra
import Data.HashMap.Strict as M
import Data.Word (Word)
import Config.Class(ConfigClass(..),Env(..))
import qualified DB
import Config.DB
import Config.Paths(PathsConfig(..))
import Config.Reservation(ReservationConfig(..),defaultReservationConfig,ReservationCommandArg(..),scriptArgs)
import Class.Castable(Castable(from))

data Config = Config {
  env :: Env,
  db  :: DB.Config,
  paths :: PathsConfig,
  reservation :: ReservationConfig
  } deriving Show -- (Data, Typeable)

instance Castable (Env, Maybe DB.Config,Maybe PathsConfig,Maybe ReservationConfig) (Maybe Config) where
  from (env', Just db', Just paths', Just rsv') =
    Just $ Config { env = env', db = db', paths = paths', reservation = rsv'}
  from _ = Nothing

data ConfigFilePaths = ConfigFilePaths {
  dbPath :: FilePath,
  pathsPath  :: FilePath,
  reservationPath  :: FilePath
  } deriving Show

defaultConfigFilePaths =
  ConfigFilePaths
  {
    dbPath = "config/database.yml",
    pathsPath = "config/paths.yml",
    reservationPath = "config/reservation.yml"
  }

load :: ConfigFilePaths -> Env -> IO (Maybe Config)
load paths env' = do
  --putStrLn "=1==========="
  mdbconf <- readYaml (dbPath          paths) env'
  --putStrLn $ show $ fromJust mdbconf
  --putStrLn "=2==========="
  mpconf  <- readYaml (pathsPath       paths) env'
  --putStrLn $ show $ fromJust mpconf  
  --putStrLn "=3==========="
  mrconf  <- readYaml (reservationPath paths) env'
  --putStrLn $ show $ fromJust mrconf
  return $ from (env',
                 mdbconf :: Maybe DB.Config,
                 mpconf  :: Maybe PathsConfig,
                 mrconf  :: Maybe ReservationConfig)
