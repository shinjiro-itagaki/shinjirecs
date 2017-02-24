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
import Model(ActiveRecord(..), find, saveE, ToMaybeEntity(..))
import Database.Persist.Types (Entity(..))
import Web.Scotty(json,param,jsonData,ActionM,status)
import Network.HTTP.Types (status200, status201, status400, status404, StdMethod(..))

runDB :: MonadIO m => ConnectionPool -> SqlPersistT IO a -> m a
runDB p action = liftIO $ runSqlPool action p

-- class (Controller c) => (ControllerAction c) ca

class (Eq sym) => ActionSymbol sym
data DefaultActionSymbol = Index | List | Get | Read | Modify | Edit | Create | New | Delete | Destroy deriving Eq

instance ActionSymbol DefaultActionSymbol

class (ActionSymbol sym) => Controller sym a where
  new :: sym -> ConnectionPool -> a
  conn :: sym -> a -> ConnectionPool
  db :: (MonadIO m) => sym -> a -> (SqlPersistT IO b -> m b)
  db sym a = runDB $ conn sym a
  beforeAction :: (Controller sym a) => sym -> a -> ActionM (Bool, a)
  beforeAction sym c = return (True, c)
  afterAction  :: (Controller sym a) => sym -> a -> ActionM ()
  afterAction  sym c = return ()

  findRecord :: (Show keyname, ActiveRecord e) => keyname -> sym -> a -> ActionM (Maybe (Entity e))
  findRecord keyname sym a = (param $ LText.pack $ show keyname :: ActionM String) >>= db sym a . find

-- instance (Controller c) => (ControllerAction c) (DefaultActionSymbol, (c -> ActionM c))

type ControllerAction c sym = (sym, (c -> ActionM c))

def :: (Controller sym c, ActionSymbol sym) => sym -> (c -> ActionM c) -> (sym, (c -> ActionM c))
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
