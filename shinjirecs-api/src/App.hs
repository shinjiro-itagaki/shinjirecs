{-# LANGUAGE OverloadedStrings #-}
module App
    ( app
    ) where
import           Web.Scotty (scotty, status, json, get)
import           Network.HTTP.Types (status404, status201)
-- import           Data.Yaml (decodeFile)
import Config (Config, ConfigFilePaths(ConfigFilePaths), dbpath, load, Env(..))

app :: Int -> IO ()
app port = do
  config <- Config.load ConfigFilePaths {
    dbpath = "config/database.yml"
  } Development
  
  scotty port $ do
    get "/" $ do
      json True
      status status201
