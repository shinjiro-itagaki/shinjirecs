{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Server where
import qualified Web.Scotty as Scotty

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

