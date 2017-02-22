{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
module Controllers.Channels where
--import Data.Eq (Eq)
-- import Data.Aeson(json)
import Web.Scotty(json)
import Controller(Controller(..), ControllerAction(..), def)
import Controller(ActionSymbol(List), toActionSym)

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

instance Controller ChannelsController where
  new conn' = ChannelsController { conn_ = conn' }
  conn                = conn_ 
  beforeAction List c = return (True, c)
  beforeAction _    c = return (True, c)

list :: ControllerAction ChannelsController
list = def List list'
  where
    filter = [] :: [P.Filter DB.Channel]
    opt    = [] :: [P.SelectOpt DB.Channel]
    list' :: ChannelsController -> ActionM ChannelsController
    list' c = (db c $ P.selectList filter opt) >>= json . map P.entityVal >> return c
