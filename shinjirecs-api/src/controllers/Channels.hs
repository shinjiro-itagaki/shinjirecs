{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Controllers.Channels where
--import Data.Eq (Eq)
-- import Data.Aeson(json)
import Web.Scotty(json)
import Controller(Controller(..), DefaultActionSymbol(..), def, ActionSymbol(..))

import Web.Scotty (ActionM)
import Control.Monad.IO.Class(MonadIO,liftIO) -- base
import qualified Database.Persist as P --persistent
import qualified DB
import qualified Database.Persist.Class as PS
import Database.Persist.Sql(ConnectionPool, SqlPersistT, runSqlPool)  --persistent
import Database.Persist.Sql.Types.Internal (SqlBackend)
-- import Model (Model, Models, getModel, getModels)

-- data Channels = Channels { conn :: ConnectionPool, models :: Models }
data ChannelsController = ChannelsController { conn_ :: ConnectionPool }
-- before :: Channels -> DefaultActionSymbol -> (Bool, Channels)

instance (Controller DefaultActionSymbol) ChannelsController where
  new  _              = ChannelsController
  conn _              = conn_ 
  beforeAction List c = return (True, c)
  beforeAction _    c = return (True, c)

list :: (DefaultActionSymbol, (ChannelsController -> ActionM ChannelsController))
list = def List list'
  where
    filter = [] :: [P.Filter DB.Channel]
    opt    = [] :: [P.SelectOpt DB.Channel]
    list' :: ChannelsController -> ActionM ChannelsController
    list' c = (db List c $ P.selectList filter opt) >>= json . map P.entityVal >> return c
