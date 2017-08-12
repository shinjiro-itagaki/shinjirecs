{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Controller where
import Data.Aeson(ToJSON(..))
import Network.Wai (Request(..))
import Network.HTTP.Types (Status, status200, status201, status400, status404, StdMethod(..))
import DB(Connection)
import Class.Castable(Castable(..))
import Class.String(StringClass(..))
import Routing.Class(RawPathParams, PathParamList(..))
import Controller.Types(ControllerResponse(..), ActionWrapper(..), Action, Body)

defaultControllerResponse :: ControllerResponse
defaultControllerResponse = MkControllerResponse {
  contentType = "application/json"
  ,body       = ""
  ,status     = status200
  }

toBody :: (StringClass str) => str -> Body
toBody = toByteStringL

--fireAction :: Connection -> Request -> ActionType a -> RawPathParams -> IO ControllerResponse
--fireAction conn req action rawparams =
--  action (case rawPathParamsToArgs rawparams of
--             Left  _ -> error "bad request"
--             Right x -> x) conn req
