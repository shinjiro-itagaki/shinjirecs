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

data DefaultActionSymbol = Index | List | Get | Read | Modify | Edit | Create | New | Delete | Destroy deriving Enum

class Controller a where
  new :: ConnectionPool -> a
  conn :: a -> ConnectionPool
  db :: (MonadIO m) => a -> (SqlPersistT IO b -> m b)
  db = runDB . conn
  beforeAction :: DefaultActionSymbol -> a -> ActionM (Bool, a)
  beforeAction sym c = return (True, c)
  afterAction  :: DefaultActionSymbol -> a -> ActionM ()
  afterAction  sym c = return ()
  
-- class (Controller DefaultActionSymbol a) => DefaultController a where

-- instance (Controller c) => (ControllerAction c) (DefaultActionSymbol, (c -> ActionM c))

-- data (Controller c) => ControllerAction c = (DefaultActionSymbol, (c -> ActionM c)

type ControllerAction c = (DefaultActionSymbol, (c -> ActionM c))
                                            
def :: (Controller c) => DefaultActionSymbol -> (c -> ActionM c) -> ControllerAction c
def sym impl = (sym, impl)

