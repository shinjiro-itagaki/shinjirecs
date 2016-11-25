{-# LANGUAGE OverloadedStrings #-}
module App
    ( app
    ) where
import           Web.Scotty (scotty, status, json, get)
import           Network.HTTP.Types (status404, status201)
-- import           Data.Yaml (decodeFile)
import Config

app :: Int -> IO ()
app port = do
  scotty port $ do
    get "/" $ do
      json True
      status status201
