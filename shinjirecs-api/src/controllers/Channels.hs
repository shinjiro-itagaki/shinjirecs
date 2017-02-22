{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Controllers.Channels where
--import Data.Eq (Eq)
-- import Data.Aeson(json)
import Data.Maybe(maybe, fromMaybe, isJust, isNothing, fromJust) -- !!!
import Web.Scotty(json,param)
import Network.HTTP.Types (status200, status201, status400, status404, StdMethod(..))
import Controller(Controller(..), DefaultActionSymbol(..), def, ActionSymbol(..))

import Web.Scotty (ActionM, status)
import Control.Monad.IO.Class(MonadIO,liftIO) -- base
import qualified Database.Persist as P --persistent
import Database.Persist.Types (Entity(entityVal))
import qualified DB
import qualified Database.Persist.Class as PS
import Database.Persist.Sql(ConnectionPool, SqlPersistT, runSqlPool)  --persistent
import Database.Persist.Sql.Types.Internal (SqlBackend)

import Model (find)

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

get :: (DefaultActionSymbol, (ChannelsController -> ActionM ChannelsController))
get = def Get impl'
  where
    res :: Maybe (Entity DB.Channel) -> ActionM ()
    res (Just e) = json $ entityVal e
    res Nothing  = status status404
    impl' :: ChannelsController -> ActionM ChannelsController
    impl' c = do
      mEntity <- ((param "id" :: ActionM Integer) >>= db Get c . find) :: ActionM (Maybe (Entity DB.Channel))
      res mEntity >> return c
