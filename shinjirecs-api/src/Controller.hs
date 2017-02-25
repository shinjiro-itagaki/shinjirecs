{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Controller where
import DB
import Data.Aeson(ToJSON(..))
import Web.Scotty (ActionM)
import Control.Monad.IO.Class(liftIO,MonadIO) -- base
import Database.Persist.Sql(ConnectionPool,SqlPersistT, runSqlPool)  --persistent
import Database.Persist (PersistEntity (..)) --persistent
import qualified Database.Persist.Class as PS
import Data.Enumerator (Enumerator) -- enumerator
import qualified Data.Text.Lazy as LText
import Model(ActiveRecord(..), find, saveR, saveE, ToMaybeEntity(..))
import Database.Persist.Types (Entity(..))
import Web.Scotty(json,param,jsonData,ActionM,status)
import Network.HTTP.Types (status200, status201, status400, status404, StdMethod(..))

import qualified Database.Persist as P --persistent
import Control.Monad.Reader(ReaderT) -- mtl
import Database.Persist.Sql.Types.Internal (SqlBackend)
import qualified Model as M
import Data.Bool(bool)

runDB :: MonadIO m => ConnectionPool -> SqlPersistT IO a -> m a
runDB p action = liftIO $ runSqlPool action p

-- class (Controller c) => (ControllerAction c) ca

data ActionSymbol = Index | List | Get | Read | Modify | Edit | Create | New | Delete | Destroy
                  | IndexN Int | ListN Int | GetN Int | ReadN Int | ModifyN Int | EditN Int | CreateN Int | NewN Int | DeleteN Int | DestroyN Int
                  | S String | I Int | SI String Int deriving Eq

-- data DefaultActionSymbol = Index | List | Get | Read | Modify | Edit | Create | New | Delete | Destroy deriving Eq
{-
newtype Index   = ActionSymbol 0
newtype List    = ActionSymbol 1
newtype Get     = ActionSymbol 2
newtype Read    = ActionSymbol 3
newtype Modify  = ActionSymbol 4
newtype Edit    = ActionSymbol 5
newtype Create  = ActionSymbol 6
newtype New     = ActionSymbol 7
newtype Delete  = ActionSymbol 8
newtype Destroy = ActionSymbol 9
-}
-- instance ActionSymbol DefaultActionSymbol

class Controller a where
  new :: ConnectionPool -> a
  conn :: a -> ConnectionPool
  db :: (MonadIO m) => a -> (SqlPersistT IO b -> m b)
  db a = runDB $ conn a
  beforeAction :: ActionSymbol -> a -> ActionM (Bool, a)
  beforeAction sym c = return (True, c)
  afterAction  :: ActionSymbol -> a -> ActionM ()
  afterAction  sym c = return ()

  findRecord :: (Show keyname, ActiveRecord e) => a -> keyname -> ActionM (Maybe (Entity e))
  findRecord a keyname = (param $ LText.pack $ show keyname :: ActionM String) >>= db a . find

  findRecords :: (ActiveRecord e) => a -> [P.Filter e] -> [P.SelectOpt e] -> ActionM [Entity e]
  findRecords a filters opts = db a $ M.selectBy filters opts

type ControllerAction c = (ActionSymbol, (c -> ActionM c))

def :: (Controller c) => ActionSymbol -> (c -> ActionM c) -> (ActionSymbol, (c -> ActionM c))
def sym impl = (sym, impl)

data ResponseType = FindR | SaveR | DeleteR

class (PS.PersistEntity a, ToJSON a) => ToJsonResponse a where
  toJsonResponse :: ResponseType -> a -> ActionM ()
  toJsonResponse FindR x = status status200 >> json x
  toJsonResponse _     x = status status201 >> json x

  toJsonResponseEntity :: ResponseType -> Entity a -> ActionM ()
  toJsonResponseEntity t (Entity k v) = toJsonResponse t v

  toJsonResponseMaybeEntity :: ResponseType -> Maybe (Entity a) -> ActionM ()
  toJsonResponseMaybeEntity t    (Just (Entity k v)) = toJsonResponse t v
  toJsonResponseMaybeEntity FindR Nothing  = status status404
  toJsonResponseMaybeEntity _     Nothing  = status status400

instance (PS.PersistEntity e, ToJSON e) => ToJsonResponse e

--      filter = [] :: [P.Filter e]
--      opt    = [] :: [P.SelectOpt e]
{- 
class (PS.PersistEntity e, PS.ToBackendKey SqlBackend e, PS.PersistRecordBackend e SqlBackend, Controller c) => Resources e c where
  list, get, modify, create, destroy :: Maybe e -> (ActionSymbol, (c -> ActionM c))

  list me = def List impl'
    where
      filter = [] :: [P.Filter e]
      opt    = [] :: [P.SelectOpt e]
      impl' :: c -> ActionM c
      impl' c = (db c $ P.selectList filter opt) >>= json . map P.entityVal >> return c

  get me = def Get impl'
    where
      impl' :: c -> ActionM c
      impl' c = do
        mEntity <- findRecord "id" c :: ActionM (Maybe (Entity e))
        toJsonResponseMaybeEntity FindR mEntity >> return c

  modify me = def Modify impl'
    where
      -- クラスに記載された関数を実行したら、インスタンスの候補が複数存在するとしてエラーになるので以下のように戻り値の型を明示した関数を作成した
      toMaybeEntity' x = toMaybeEntity x :: Maybe (Entity e)
      impl' :: c -> ActionM c
      impl' c = do
        mEntity <- findRecord "id" c :: ActionM (Maybe (Entity e))
        newrec <- (jsonData :: ActionM e)
        case mEntity of
          Just e  -> (db c $ saveE $ e {entityVal = newrec}) >>= return . toMaybeEntity' >>= toJsonResponseMaybeEntity SaveR >> return c
          Nothing -> return c

  create me = def Create impl'
    where
      toMaybeEntity' x = toMaybeEntity x :: Maybe (Entity e)
      impl' :: c -> ActionM c
      impl' c = do
        newrec <- (jsonData :: ActionM e)
        (db c $ saveR newrec) >>= return . toMaybeEntity' >>= toJsonResponseMaybeEntity SaveR >> return c
  
  destroy me = def Destroy impl'
    where
      destroy' :: Entity e -> ReaderT SqlBackend IO (Bool, (Entity e), PS.Key e)
      destroy' e = M.destroy e
      findRecord' c = findRecord "id" c :: ActionM (Maybe (Entity e))
      impl' :: c -> ActionM c
      impl' c = do
        findRecord' c >>= maybe
          (status status404 >> return c)
          (\e -> do
              (b, e2, k) <- db c $ destroy' e
              status $ bool status201 status400 b
              return c
          )
-}
