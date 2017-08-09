{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Controllers.ChannelsController where
-- import Controller(Controller(..), def, ActionSymbol(..), ToJsonResponse(..), ResponseType(..), findRecord)
-- import Controller(ActionSymbol(..))

import Data.Bool(bool)
import Data.Maybe(maybe, fromMaybe, isJust, isNothing, fromJust) -- !!!
-- import Server(json,param,jsonData,ActionM,status)
import Network.HTTP.Types (status200, status201, status400, status404, StdMethod(..))
import Control.Monad.IO.Class(MonadIO,liftIO) -- base
import qualified Database.Persist as P --persistent
import Database.Persist.Types (Entity(entityVal))
import qualified DB
import qualified Database.Persist.Class as PS
import Database.Persist.Sql(ConnectionPool, SqlPersistT, runSqlPool)  --persistent
import Database.Persist.Sql.Types.Internal (SqlBackend)
import Model (find, saveE,saveR ,ToMaybeEntity(..))
import qualified Model as M
import Models.Channel
import Control.Monad.Reader(ReaderT) -- mtl

data ChannelsController = ChannelsController { conn_ :: ConnectionPool }

{-
instance Controller ChannelsController where
  new                 = ChannelsController
  conn                = conn_
  beforeAction List c = return (True, c)
  beforeAction _    c = return (True, c)
-}
-- クラスに記載された関数を実行したら、インスタンスの候補が複数存在するとしてエラーになるので以下のように戻り値の型を明示した関数を作成した
toMaybeEntity' x = toMaybeEntity x :: Maybe (Entity DB.Channel)
{- 
list, get, modify, create, destroy :: (ActionSymbol, (ChannelsController -> ActionM ChannelsController))

list = def List list'
  where
    filter = [] :: [P.Filter DB.Channel]
    opt    = [] :: [P.SelectOpt DB.Channel]
    list' :: ChannelsController -> ActionM ChannelsController
    list' c = (db c $ P.selectList filter opt) >>= json . map P.entityVal >> return c

get = def Get impl'
  where
    impl' :: ChannelsController -> ActionM ChannelsController
    impl' c = do
      mEntity <- findRecord c "id" :: ActionM (Maybe (Entity DB.Channel))
      toJsonResponseMaybeEntity FindR mEntity >> return c

modify = def Modify impl'
  where
    impl' :: ChannelsController -> ActionM ChannelsController
    impl' c = do
      mEntity <- findRecord c "id" :: ActionM (Maybe (Entity DB.Channel))
      newrec <- (jsonData :: ActionM DB.Channel)
      case mEntity of
        Just e  -> (db c $ saveE $ e {entityVal = newrec}) >>= return . toMaybeEntity' >>= toJsonResponseMaybeEntity SaveR >> return c
        Nothing -> return c

create = def Create impl'
  where
    impl' :: ChannelsController -> ActionM ChannelsController
    impl' c = do
      newrec <- (jsonData :: ActionM DB.Channel)
      (db c $ saveR newrec) >>= return . toMaybeEntity' >>= toJsonResponseMaybeEntity SaveR >> return c

destroy = def Destroy impl'
  where
    destroy' :: Entity DB.Channel -> ReaderT SqlBackend IO (Bool, (Entity DB.Channel), PS.Key DB.Channel)
    destroy' e = M.destroy e
    findRecord' c = findRecord c "id" :: ActionM (Maybe (Entity DB.Channel))
    impl' :: ChannelsController -> ActionM ChannelsController
    impl' c = do
      findRecord' c >>= maybe
        (status status404 >> return c)
        (\e -> do
            (b, e2, k) <- db c $ destroy' e
            status $ bool status201 status400 b
            return c
        )
-}
