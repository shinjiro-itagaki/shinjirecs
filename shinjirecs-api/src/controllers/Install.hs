{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Controllers.Install where
import Controller(Controller(..), def, ActionSymbol(..), ToJsonResponse(..), toJsonResponseME, ResponseType(..), findRecord)
import Database.Persist.Sql(ConnectionPool, SqlPersistT, runSqlPool)  --persistent
import Web.Scotty(json,param,jsonData, ActionM, status)

-- import qualified DB

data InstallController = InstallController { conn_ :: ConnectionPool }

data InstallActionSymbol = Index | ResultDetectChannels | Step Int deriving Eq

instance ActionSymbol InstallActionSymbol

instance (Controller InstallActionSymbol) InstallController where
  new  _              = InstallController
  conn _              = conn_
  beforeAction Index c = return (True, c)
  beforeAction (Step _) c = return (True, c)
  beforeAction _     c = return (True, c)

-- toMaybeEntity' x = toMaybeEntity x :: Maybe (Entity DB.Channel)

index, resultDetectChannels :: (InstallActionSymbol, (InstallController -> ActionM InstallController))

index = def Index impl'
  where
    impl' c = return c

resultDetectChannels = def ResultDetectChannels impl'
  where
    impl' c = return c

step :: Int -> (InstallActionSymbol, (InstallController -> ActionM InstallController))
step n = def (Step n) impl'
  where
    impl' c = return c
