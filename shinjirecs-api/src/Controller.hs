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

class (Enum sym) => ActionSymbol sym
data DefaultActionSymbol = Index | List | Get | Read | Modify | Edit | Create | New | Delete | Destroy deriving Enum

instance ActionSymbol DefaultActionSymbol

class (ActionSymbol sym) => Controller sym a where
  new :: sym -> ConnectionPool -> a
  conn :: sym -> a -> ConnectionPool
  db :: (MonadIO m) => sym -> a -> (SqlPersistT IO b -> m b)
  db sym a = runDB $ conn sym a
  beforeAction :: (ActionSymbol sym) => sym -> a -> ActionM (Bool, a)
  beforeAction sym c = return (True, c)
  afterAction  :: (ActionSymbol sym) => sym -> a -> ActionM ()
  afterAction  sym c = return ()

--  findRecord :: (Show keyname, ActiveRecord e) => keyname -> c -> ActionM (Maybe (Entity e))
--  findRecord keyname a = (param $ LText.pack $ show keyname :: ActionM String) >>= db sym c . find


  
-- instance (Controller c) => (ControllerAction c) (DefaultActionSymbol, (c -> ActionM c))

type ControllerAction c sym = (sym, (c -> ActionM c))

def :: (Controller sym c, ActionSymbol sym) => sym -> (c -> ActionM c) -> (sym, (c -> ActionM c))
def sym impl = (sym, impl)

data ResponseType = FindR | SaveR | DeleteR

class (ToJSON a) => ToJsonResponse a where
  toJsonResponse :: ResponseType -> a -> ActionM ()
  toJsonResponse FindR x = status status200 >> json x
  toJsonResponse _     x = status status201 >> json x

  toJsonResponseFind, toJsonResponseSave, toJsonResponseDelete :: a -> ActionM ()
  toJsonResponseFind   = toJsonResponse FindR
  toJsonResponseSave   = toJsonResponse SaveR
  toJsonResponseDelete = toJsonResponse DeleteR

  toJsonResponseM :: ResponseType -> Maybe a -> ActionM ()
  toJsonResponseM t    (Just x)  = toJsonResponse t x    
  toJsonResponseM FindR Nothing  = status status404
  toJsonResponseM _     Nothing  = status status400

  toJsonResponseFindM, toJsonResponseSaveM, toJsonResponseDeleteM :: Maybe a -> ActionM ()
  toJsonResponseFindM   = toJsonResponseM FindR
  toJsonResponseSaveM   = toJsonResponseM SaveR
  toJsonResponseDeleteM = toJsonResponseM DeleteR

toJsonResponseME :: (PS.PersistEntity a, ToJSON a) => ResponseType -> Maybe (Entity a) -> ActionM ()
toJsonResponseME x (Just e) = toJsonResponseM x (Just $ entityVal e)
toJsonResponseME x Nothing  = status status400 -- ?? [toJsonResponseM x Nothing] is bad

instance (PS.PersistEntity e, ToJSON e) => ToJsonResponse e
