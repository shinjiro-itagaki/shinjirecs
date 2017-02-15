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
  beforeSave :: SaveType -> entity -> Maybe entity
  beforeSave _ = Just
  
  -- please override if you need
  beforeInsertSave :: entity -> Maybe entity
  beforeInsertSave = Just

  -- please override if you need
  beforeUpdateSave :: entity -> Maybe entity
  beforeUpdateSave = Just
  
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
      
class (PS.PersistEntity entity, PS.ToBackendKey SqlBackend entity, PS.PersistRecordBackend entity SqlBackend) => (Saver entity) record where
  saveWithoutHook :: record -> ReaderT SqlBackend IO (Maybe (Entity entity))
  exist           :: record -> ReaderT SqlBackend IO Bool
  save :: record -> ReaderT SqlBackend IO (Maybe (Entity entity))
  
instance (PS.PersistEntity entity, PS.ToBackendKey SqlBackend entity, PS.PersistRecordBackend entity SqlBackend) => ActiveRecord entity where

instance (ActiveRecord entity) => (Saver entity) entity where
  exist = existOnDbBy
  saveWithoutHook = saveAsInsert
  save val = do
    case beforeSave Insert val of
      Just val2 -> saveWithoutHook val2
      Nothing   -> return Nothing
  
instance (ActiveRecord entity) => Saver entity (Entity entity) where
  exist = existOnDb . entityKey
  saveWithoutHook self = saveAsUpdate (entityKey self) (entityVal self)
  save (Entity key val) = do
    case beforeSave Update val of
      Just val2 -> saveWithoutHook val2
      Nothing   -> return Nothing

--  exist = existOnDbBy
-- instance (PS.PersistEntity entity, PS.ToBackendKey SqlBackend entity, PS.PersistRecordBackend entity SqlBackend) => ActiveRecord (Entity entity) where
--  exist = existOnDb . entityKey
  
  {-
  exist entity = do
    uniqueKey <- PS.onlyUnique entity
    entity2 <- PS.getBy uniqueKey
    return (case entity2 of
      Nothing -> False
      _       -> True)  
-}
class (PS.PersistEntity entity
      , PS.ToBackendKey SqlBackend entity
      , PS.PersistRecordBackend entity SqlBackend
      ) => Record entity where
{-  
  exist record = do
    mExists <- PS.get $ getKey record
    return (case mExists of
      Nothing -> False
      _       -> True)

  beforeSave record isUpdate = Just record
  
  save record = do
    uniqueKey <- PS.onlyUnique record
    isExist' <- exist record
    case beforeSave record isExist' of
      Just record2 -> do
        res <- (if isExist'
                then PS.insert record2 --PS.replace (getKey record2) record2 -- update
                else PS.insert record2 -- insert
               )
        -- return $ maybeToBool res
        return True
      Nothing -> return False
-}
      
class (PS.PersistEntity record, PS.DeleteCascade record SqlBackend) => CascadeDeletable record where
  deleteCascade      :: (MonadIO m) => ConnectionPool -> PS.Key record -> m ()
  deleteCascadeWhere :: (MonadIO m) => ConnectionPool -> [Filter record] -> m ()  
  -- deleteCascade conn key             = do { runDB conn $ PS.deleteCascade key }
  -- deleteCascadeWhere conn filters    = do { runDB conn $ PS.deleteCascadeWhere filters }

class (PS.PersistEntity record, Eq record, Eq (PS.Unique record) )=> UniqueReplaceable record where
  replaceUnique :: (MonadIO m) => ConnectionPool -> PS.Key record -> record -> m (Maybe (PS.Unique record))
--  replaceUnique conn key r = do { runDB conn $ PS.replaceUnique key r }

-- need MultiParamTypeClasses
-- need AllowAmbiguousTypes
-- class (BaseModel record) => Record record where
--   entity  :: record -> e
--   save    :: record -> Bool
--   destroy :: record -> Bool

-- class TableClass table where
--   db :: (DBInfoClass dbinfoclass) => table -> dbinfoclass
--   find :: (PS.PersistEntity e, MonadIO m, RecordClass e r) => table -> PS.Key e -> m (Maybe r)

-- data DBInfo = DBInfo ConnectionPool
-- instance DBInfoClass ConnectionPool where
--   connection conn = conn
  
-- instance TableClass ConnectionPool where
--   db self = self
--   --   find :: (MonadIO m, RecordClass e r) => table -> PS.Key e -> m (Maybe r)
--   find conn key = do 
--     r <- get conn key
--     case r of
--       Just r2 -> return $ Just (MkRecordData r2 table')
--       Nothing -> return $ Nothing
    
-- instance (BaseModel e) => TableClass e DBInfo where

-- data (BaseModel e, DBInfoClass dbinfo) => TableData e dbinfo = MkTableData dbinfo
-- data (BaseModel e) => TableData e = MkTableData DBInfo
--type (BaseModel e) => TableDate e = (e, DBInfo)

-- data (BaseModel e, TableClass e table) => RecordData e table = MkRecordData e table
-- data (BaseModel e) => RecordData e = MkRecordData e (TableData e)
--type (BaseModel e) => RecordData e = (e, TableData e)

-- class (BaseModel e) => TableClass e table where
-- data (BaseModel e) => TableData e = MkTableData DBInfo
--instance (BaseModel e) => TableClass e (TableData e) where
--  db :: (DBInfoClass dbinfoclass) => table -> dbinfoclass
--  find :: (MonadIO m, RecordClass e r) => table -> PS.Key e -> m (Maybe r)  
  -- db (MkTableData dbinfo) = dbinfo
  -- find table' key' = do
  --   r <- get (connection $ db table') key'
  --   case r of
  --     Just r2 -> return $ Just (MkRecordData r2 table')
  --     Nothing -> return $ Nothing
     
-- FlexibleInstances
-- UndecidableInstances
-- instance (BaseModel e) => RecordClass e (RecordData e) where
--   entity  (MkRecordData e table) = e
--   save    (MkRecordData e table) = True
--   destroy (MkRecordData e table) = True


-- data (BaseModel record) => Model record = Model {
--                                                  }
-- data Models = Models {
--   channels :: Model DB.Channel
--   }

-- getModel :: (PS.PersistEntity record) => ConnectionPool -> Model record
-- getModel conn = Model {
-- --  finder = runDB conn
--   }

-- getModels :: ConnectionPool -> Models
-- getModels conn = Models {
--   channels = getModel conn
--   }
