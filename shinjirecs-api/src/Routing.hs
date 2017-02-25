{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Routing where

import qualified Controller
import Controller(ControllerAction(..), Controller(..), ActionSymbol(..))

import qualified Controllers.Channels     as ChannelsC
import qualified Controllers.Install      as InstallC
import qualified Controllers.Programs     as ProgramsC
import qualified Controllers.Reservations as ReservationsC
import Web.Scotty (ScottyM, scotty, status, json, get, patch, delete , post, options, ActionM, param, jsonData, addroute, setHeader,middleware, RoutePattern)
import qualified Database.Persist.Sql as Sql --persistent

import Network.HTTP.Types (status200, status201, status400, status404, StdMethod(..))

def :: (Controller c) =>
  (RoutePattern -> ActionM () -> ScottyM ())
  -> Sql.ConnectionPool
  -> RoutePattern
  -> ControllerAction c
  -> ScottyM ()
def func conn pat act = do
  options pat (status status200) -- add OPTIONS
  func pat $ Controller.run conn act

run :: Sql.ConnectionPool -> ScottyM ()
run conn = do
  _GET    "/channels/list" ChannelsC.list
  _GET    "/channels/:id"  ChannelsC.get
  _PATCH  "/channels/:id"  ChannelsC.modify
  _POST   "/channels"      ChannelsC.create
  _DELETE "/channels/:id"  ChannelsC.destroy
    
  _GET    "/install/index" InstallC.index
  _GET    "/install/result_detect_channels" InstallC.resultDetectChannels
  _GET    "/install/step1" (InstallC.step 1)
  _GET    "/install/step2" (InstallC.step 2)
  _GET    "/install/step3" (InstallC.step 3)
    
  _GET    "/programs/list" ProgramsC.list
  _GET    "/programs/:id"  ProgramsC.get
  _PATCH  "/programs/:id"  ProgramsC.modify
  _POST   "/programs"      ProgramsC.create
  _DELETE "/programs/:id"  ProgramsC.destroy
    
  _GET    "/reservations/list" ReservationsC.list
  _GET    "/reservations/:id"  ReservationsC.get
  _PATCH  "/reservations/:id"  ReservationsC.modify
  _POST   "/reservations"      ReservationsC.create
  _DELETE "/reservations/:id"  ReservationsC.destroy
  where
    _GET, _PATCH, _POST, _DELETE :: (Controller c) => RoutePattern -> ControllerAction c -> ScottyM ()
    _GET    = def get    conn
    _PATCH  = def patch  conn
    _POST   = def post   conn
    _DELETE = def delete conn

    
    
