{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Controller where
import Data.Aeson(ToJSON(..))
import Data.ByteString as B
import Data.ByteString.Lazy as L
import Network.Wai (Request(..))
import Network.HTTP.Types (Status, status200, status201, status400, status404, StdMethod(..))
import DB(Connection)
import Class.Castable(Castable(..))

type ContentType = B.ByteString
type Body = L.ByteString

data ControllerResponse = MkControllerResponse {
  contentType :: ContentType
  ,body       :: Body
  ,status     :: Status
  }
  
defaultControllerResponse :: ControllerResponse
defaultControllerResponse = MkControllerResponse {
  contentType = "application/json"
  ,body       = ""
  ,status     = status200
  }
  
type Symbol = String
type Action = (Connection -> Request -> IO ControllerResponse)
type Action2 a = (Connection -> Request -> a -> IO ControllerResponse)


