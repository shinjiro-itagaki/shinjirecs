{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Server where
import qualified Data.Text.Lazy as L
import qualified Web.Scotty as Scotty
import Data.Aeson.Types(ToJSON,FromJSON) -- aeson

type ServerM      = Scotty.ScottyM
type ActionM      = Scotty.ActionM
type RoutePattern = Scotty.RoutePattern

server       = Scotty.scotty
middleware   = Scotty.middleware
status       = Scotty.status
get          = Scotty.get
patch        = Scotty.patch
delete       = Scotty.delete
post         = Scotty.post
options      = Scotty.options

json :: ToJSON a => a -> ActionM ()
json = Scotty.json
param :: Scotty.Parsable a => L.Text -> ActionM a
param = Scotty.param
jsonData :: FromJSON a => ActionM a
jsonData = Scotty.jsonData
