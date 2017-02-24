{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Controllers.Install where
import Controller(Controller(..), def, ActionSymbol(..), ToJsonResponse(..), ResponseType(..), findRecord)
import Database.Persist.Sql(ConnectionPool, SqlPersistT, runSqlPool)  --persistent
import Web.Scotty(json,param,jsonData, ActionM, status)

-- import qualified DB

data InstallController = InstallController { conn_ :: ConnectionPool }

-- data InstallActionSymbol = Index | ResultDetectChannels | Step Int deriving Eq
-- newtype Step = Custom Int

instance Controller InstallController where
  new                  = InstallController
  conn                 = conn_
  beforeAction Index c = return (True, c)
--  beforeAction (Custom _) c = return (True, c)
  beforeAction _     c = return (True, c)

-- toMaybeEntity' x = toMaybeEntity x :: Maybe (Entity DB.Channel)

index, resultDetectChannels :: (ActionSymbol, (InstallController -> ActionM InstallController))

index = def Index impl'
  where
    impl' c = return c
--ResultDetectChannels
resultDetectChannels = def (Custom 1) impl'
  where
    impl' c = return c

step :: Int -> (ActionSymbol, (InstallController -> ActionM InstallController))
step n = def (Custom n) impl'
  where
    impl' c = return c
