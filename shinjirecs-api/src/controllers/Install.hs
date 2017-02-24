{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Controllers.Install where
import Controller(Controller(..), def, ActionSymbol(..), ToJsonResponse(..), ResponseType(..), findRecord)
import Database.Persist.Sql(ConnectionPool, SqlPersistT, runSqlPool)  --persistent
import Web.Scotty(json,param,jsonData, ActionM, status)

-- data Step = Custom Int

data InstallController = InstallController { conn_ :: ConnectionPool }

-- data InstallActionSymbol = Index | ResultDetectChannels | Step Int deriving Eq
-- newtype Step = Custom Int

data Sym x y = X x | Y y | N

instance Controller InstallController where
  new                  = InstallController
  conn                 = conn_
  beforeAction Index c = return (True, c)
  beforeAction (SI "step" 1) c = return (True, c)
  beforeAction _     c = return (True, c)

-- toMaybeEntity' x = toMaybeEntity x :: Maybe (Entity DB.Channel)

index, resultDetectChannels :: (ActionSymbol, (InstallController -> ActionM InstallController))

index = def Index impl'
  where
    impl' c = return c
--ResultDetectChannels
resultDetectChannels = def (S "resultDetectChannels") impl'
  where
    impl' c = return c

step :: Int -> (ActionSymbol, (InstallController -> ActionM InstallController))
step n = def (SI "step" n) impl'
  where
    impl' c = return c
