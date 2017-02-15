{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE FunctionalDependencies #-}
-- {-# LANGUAGE ConstraintKinds #-}

module Model where
import Text.Read(readMaybe) -- !!!
import Data.Maybe(fromMaybe) -- !!!
import Control.Monad.IO.Class(MonadIO,liftIO) -- base
import qualified Database.Persist.Class as PS
import Database.Persist.Sql(ConnectionPool, SqlPersistT, runSqlPool, toSqlKey)  --persistent
import Database.Persist.Sql.Types.Internal (SqlBackend)
import Database.Persist.Types (Update,Entity(..),Filter,SelectOpt)
import qualified DB
import Web.Scotty (ActionM)
import Control.Monad.Trans.Resource(MonadResource) -- resourcet
import Control.Monad.Reader(ReaderT) -- mtl
import Control.Monad.Reader.Class(MonadReader) -- mtl
import Data.Acquire(Acquire) -- resourcet
import Data.Conduit(Source) --- conduit
import Data.Int(Int64) -- base

maybeToBool :: Maybe x -> Bool
maybeToBool Nothing = False
maybeToBool _       = True


runDB :: MonadIO m => ConnectionPool -> ReaderT SqlBackend IO a -> m a
runDB p action = liftIO $ runSqlPool action p

{-
let resevation = findById conn $ param "id"
    time_year  = param "time_year"
    time_mon   = param "time_mon"
    time_day   = param "time_day"
    time_hh    = param "time_hh"
    time_mm    = param "time_mm"
    time_ss    = param "time_ss"
res <- save conn reservation [RerservationStart_time +=. ]
-}

find :: (PS.PersistEntity entity, PS.ToBackendKey SqlBackend entity, PS.PersistRecordBackend entity SqlBackend, Read id, Show id) => id -> SqlPersistT IO (Maybe (Entity entity))

find id = do
  mEntity <- PS.get key 
  return $ (case mEntity of
               Just entity -> Just Entity {entityKey = key, entityVal = entity}
               Nothing     -> Nothing )
  where
    ifNothing' = -1
    key = toSqlKey $ fromMaybe ifNothing' $ readMaybe $ show id

data SaveType = Update | Insert

class (PS.PersistEntity entity, PS.ToBackendKey SqlBackend entity, PS.PersistRecordBackend entity SqlBackend) => ActiveRecord entity where
  -- please override if you need
  beforeSave :: SaveType -> entity -> ReaderT SqlBackend IO (Maybe entity)
  beforeSave _ = return . Just

  afterSave :: SaveType -> entity -> ReaderT SqlBackend IO (Maybe entity)
  afterSave _ = return . Just
  
  -- please override if you need
  beforeInsertSave :: entity -> ReaderT SqlBackend IO (Maybe entity)
  beforeInsertSave = return . Just

  -- please override if you need
  beforeUpdateSave :: entity -> ReaderT SqlBackend IO (Maybe entity)
  beforeUpdateSave = return . Just

  -- please override if you need
  beforeDestroy :: entity -> ReaderT SqlBackend IO (Maybe entity)
  beforeDestroy = return . Just
  
  existOnDb :: PS.Key entity -> ReaderT SqlBackend IO Bool
  existOnDb key = (return . maybeToBool) =<< (PS.liftPersist $ PS.get key)
  
  existOnDbBy :: entity -> ReaderT SqlBackend IO Bool
  existOnDbBy entity= (return . maybeToBool) =<< (PS.liftPersist . PS.getBy) =<< (PS.liftPersist $ PS.onlyUnique entity)
  
  saveAsUpdate :: PS.Key entity -> entity -> ReaderT SqlBackend IO (Maybe (Entity entity))
  saveAsUpdate key entity = do
    PS.replace key entity
    mEntity <- PS.liftPersist $ PS.get key
    
    -- need to check updated here
    
    return $ (case mEntity of
                Just entity2 -> Just Entity {entityKey = key, entityVal = entity2 }
                Nothing -> Nothing)
      
  saveAsInsert :: entity -> ReaderT SqlBackend IO (Maybe (Entity entity))
  saveAsInsert entity = do
    
    -- need to check failed(exception thrown) here
    
    key <- PS.liftPersist $ PS.insert entity
    mEntity <- PS.liftPersist $ PS.get key
    return $ (case mEntity of
                Just entity2 -> Just Entity {entityKey = key, entityVal = entity2 }
                Nothing      -> Nothing )

class (PS.PersistEntity entity, PS.ToBackendKey SqlBackend entity, PS.PersistRecordBackend entity SqlBackend) => (ActiveRecordKeyVal entity) record where
  getKey :: record -> Maybe (PS.Key entity)
  getVal :: record -> entity

instance (ActiveRecord entity) => (ActiveRecordKeyVal entity) entity where
  getKey self = Nothing
  getVal self = self
  
instance (ActiveRecord entity) => ActiveRecordKeyVal entity (Entity entity) where  
  getKey = Just . entityKey
  getVal = entityVal
  
-- need MultiParamTypeClasses
-- need AllowAmbiguousTypes
class (PS.PersistEntity entity, PS.ToBackendKey SqlBackend entity, PS.PersistRecordBackend entity SqlBackend) => (ActiveRecordSaver entity) record where
  saveWithoutHook :: record -> ReaderT SqlBackend IO (Maybe (Entity entity))
  exist           :: record -> ReaderT SqlBackend IO Bool
  save :: record -> ReaderT SqlBackend IO (Maybe (Entity entity))
  saveByKeyVal :: record -> Maybe (PS.Key entity) -> entity -> ReaderT SqlBackend IO (Maybe (Entity entity))
  saveByKeyVal self key val = do
    res <- beforeSave (maybeToSaveType' key) val
    case res of
      Just val2 -> saveWithoutHook val2
      Nothing   -> return Nothing
    where
      maybeToSaveType' Nothing = Insert
      maybeToSaveType' (Just x)= Update
  
class (PS.PersistEntity entity, PS.ToBackendKey SqlBackend entity, PS.PersistRecordBackend entity SqlBackend) => (ActiveRecordDestroyer entity) record where
  destroyWithoutHook :: record -> ReaderT SqlBackend IO (Maybe (PS.Key entity))
  destroy            :: record -> ReaderT SqlBackend IO (Maybe (PS.Key entity))
  
instance (PS.PersistEntity entity, PS.ToBackendKey SqlBackend entity, PS.PersistRecordBackend entity SqlBackend) => ActiveRecord entity

instance (ActiveRecord entity) => (ActiveRecordSaver entity) entity where
  exist = existOnDbBy
  saveWithoutHook = saveAsInsert
  save self = saveByKeyVal self Nothing self

instance (ActiveRecord entity) => ActiveRecordSaver entity (Entity entity) where
  exist = existOnDb . entityKey
  saveWithoutHook self = saveAsUpdate (entityKey self) (entityVal self)
  save self@(Entity key val) = saveByKeyVal self (Just key) val

instance (ActiveRecord entity) => ActiveRecordDestroyer entity (Entity entity) where
  destroyWithoutHook record = do
    let key = entityKey record
    PS.delete key
    return $ Just key
    
  destroy record@(Entity key val) = do
    res <- beforeDestroy val
    case res of
      Just _ -> do
        res <- destroyWithoutHook record
        -- check destroyed here
        return res
      Nothing -> return Nothing
      
class (PS.PersistEntity record, PS.DeleteCascade record SqlBackend) => CascadeDeletable record where
  deleteCascade      :: (MonadIO m) => ConnectionPool -> PS.Key record -> m ()
  deleteCascadeWhere :: (MonadIO m) => ConnectionPool -> [Filter record] -> m ()  
  -- deleteCascade conn key             = do { runDB conn $ PS.deleteCascade key }
  -- deleteCascadeWhere conn filters    = do { runDB conn $ PS.deleteCascadeWhere filters }

class (PS.PersistEntity record, Eq record, Eq (PS.Unique record) )=> UniqueReplaceable record where
  replaceUnique :: (MonadIO m) => ConnectionPool -> PS.Key record -> record -> m (Maybe (PS.Unique record))
--  replaceUnique conn key r = do { runDB conn $ PS.replaceUnique key r }
