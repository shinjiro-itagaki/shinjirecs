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
import Server(json,param,jsonData,ActionM,status)
import Control.Monad.IO.Class(liftIO,MonadIO) -- base
import Database.Persist.Sql(ConnectionPool,SqlPersistT, runSqlPool)  --persistent
import Database.Persist (PersistEntity (..)) --persistent
import qualified Database.Persist.Class as PS
import Data.Enumerator (Enumerator) -- enumerator
import qualified Data.Text.Lazy as LText
import Model(ActiveRecord(..), find, saveR, saveE, ToMaybeEntity(..))
import Database.Persist.Types (Entity(..))

import Network.HTTP.Types (status200, status201, status400, status404, StdMethod(..))

import qualified Database.Persist as P --persistent
import Control.Monad.Reader(ReaderT) -- mtl
import Database.Persist.Sql.Types.Internal (SqlBackend)
import qualified Model as M
import Data.Bool(bool)


-- class (Controller c) => (ControllerAction c) ca

data ActionSymbol = Index | List | Get | Read | Modify | Edit | Create | New | Delete | Destroy
                  | IndexN Int | ListN Int | GetN Int | ReadN Int | ModifyN Int | EditN Int | CreateN Int | NewN Int | DeleteN Int | DestroyN Int
                  | S String | I Int | SI String Int deriving Eq

class Controller a where
  new :: ConnectionPool -> a
  conn :: a -> ConnectionPool
  db :: (MonadIO m) => a -> (SqlPersistT IO b -> m b)
  db a = DB.run $ conn a
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


run :: (Controller c) => ConnectionPool -> ControllerAction c -> ActionM ()
run conn (sym, main) = do
  (res, c) <- beforeAction sym $ new conn
  if res
  then afterAction sym =<< main c
  else return () -- do nothing
