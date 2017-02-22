{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Controller where
import DB
import Web.Scotty (ActionM)
import Control.Monad.IO.Class(liftIO,MonadIO) -- base
import Database.Persist.Sql(ConnectionPool,SqlPersistT, runSqlPool)  --persistent
import Database.Persist (PersistEntity (..)) --persistent
import qualified Database.Persist.Class as PS
import Data.Enumerator (Enumerator) -- enumerator

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
  
-- instance (Controller c) => (ControllerAction c) (DefaultActionSymbol, (c -> ActionM c))

type ControllerAction c sym = (sym, (c -> ActionM c))

def :: (Controller sym c, ActionSymbol sym) => sym -> (c -> ActionM c) -> (sym, (c -> ActionM c))
def sym impl = (sym, impl)

