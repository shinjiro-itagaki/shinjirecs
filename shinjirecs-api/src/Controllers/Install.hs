{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Controllers.Install where
import Controller(Controller(..), def, ActionSymbol(..), ToJsonResponse(..), ResponseType(..), findRecord)
import Database.Persist.Sql(ConnectionPool, SqlPersistT, runSqlPool)  --persistent
import Server(json,param,jsonData,ActionM,status)

data InstallController = InstallController { conn_ :: ConnectionPool }

instance Controller InstallController where
  new                        = InstallController
  conn                       = conn_
  beforeAction Index         = return . (,) True
  beforeAction (SI "step" 1) = return . (,) True
  beforeAction _             = return . (,) True

-- toMaybeEntity' x = toMaybeEntity x :: Maybe (Entity DB.Channel)

index, resultDetectChannels :: (ActionSymbol, (InstallController -> ActionM InstallController))

index = def Index impl'
  where
    impl' c = return c

resultDetectChannels = def (S "resultDetectChannels") impl'
  where
    impl' c = return c

step :: Int -> (ActionSymbol, (InstallController -> ActionM InstallController))
step n = def (SI "step" n) impl'
  where
    impl' c = return c
