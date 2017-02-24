{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Controllers.Install where
import Controller(Controller(..), def, ActionSymbol(..), ToJsonResponse(..), toJsonResponseME, ResponseType(..), findRecord)
import Database.Persist.Sql(ConnectionPool, SqlPersistT, runSqlPool)  --persistent
import Web.Scotty(json,param,jsonData, ActionM, status)

-- import qualified DB

data InstallController = InstallController { conn_ :: ConnectionPool }

data InstallActionSymbol = Index | ResultDetectChannels | Step1 | Step2 | Step3 deriving Enum

instance ActionSymbol InstallActionSymbol

instance (Controller InstallActionSymbol) InstallController where
  new  _              = InstallController
  conn _              = conn_
  beforeAction Index c = return (True, c)
  beforeAction _     c = return (True, c)

-- toMaybeEntity' x = toMaybeEntity x :: Maybe (Entity DB.Channel)

index, resultDetectChannels, step1, step2, step3 :: (InstallActionSymbol, (InstallController -> ActionM InstallController))

index = def Index impl'
  where
    impl' c = return c

resultDetectChannels = def ResultDetectChannels impl'
  where
    impl' c = return c

step1 = def Step1 impl'
  where
    impl' c = return c
    
step2 = def Step2 impl'
  where
    impl' c = return c    

step3 = def Step3 impl'
  where
    impl' c = return c
