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
import Control.Monad.IO.Class(MonadIO,liftIO) -- base
import qualified Database.Persist.Class as PS
import Database.Persist.Sql(ConnectionPool, SqlPersistT, runSqlPool, toSqlKey)  --persistent
import Database.Persist.Sql.Types.Internal (SqlBackend)
import Database.Persist.Types (Update,Entity,Filter,SelectOpt)
import qualified DB
import Web.Scotty (ActionM)
import Control.Monad.Trans.Resource(MonadResource) -- resourcet
import Control.Monad.Reader(ReaderT) -- mtl
import Control.Monad.Reader.Class(MonadReader) -- mtl
import Data.Acquire(Acquire) -- resourcet
import Data.Conduit(Source) --- conduit
-- import Data.Aeson.Types(ToJSON, FromJSON) -- aeson

runDB :: MonadIO m => ConnectionPool -> SqlPersistT IO a -> m a
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

class (PS.PersistEntity entity
      , PS.ToBackendKey SqlBackend entity
      , PS.PersistRecordBackend entity SqlBackend
      ) => Record entity where
  -- toKey :: PS.ToBackendKey Sql.SqlBackend record => L.Text -> ActionM (PS.Key record)
  -- toKey keyname = Sql.toSqlKey <$> (param keyname) -- :: ActionM (PS.Key record)-
  -- getId              :: entity -> PS.Key entity
  -- getId self = toSqlKey $ 
  get                :: (MonadIO m) => ConnectionPool -> PS.Key entity -> m (Maybe entity)
  insert             :: (MonadIO m) => ConnectionPool -> entity -> m (PS.Key entity)
  insert_            :: (MonadIO m) => ConnectionPool -> entity -> m ()
  insertMany         :: (MonadIO m) => ConnectionPool -> [entity] -> m [PS.Key entity]
  insertMany_        :: (MonadIO m) => ConnectionPool -> [entity] -> m ()
  insertEntityMany   :: (MonadIO m) => ConnectionPool -> [Entity entity] -> m ()
  insertKey          :: (MonadIO m) => ConnectionPool -> PS.Key entity -> entity -> m ()
  repsert            :: (MonadIO m) => ConnectionPool -> PS.Key entity -> entity -> m ()
  replace            :: (MonadIO m) => ConnectionPool -> PS.Key entity -> entity -> m ()
  delete             :: (MonadIO m) => ConnectionPool -> PS.Key entity -> m ()
  update             :: (MonadIO m) => ConnectionPool -> PS.Key entity -> [Update entity] -> m ()
  updateGet          :: (MonadIO m) => ConnectionPool -> PS.Key entity -> [Update entity] -> m entity
  getJust            :: (MonadIO m) => ConnectionPool -> PS.Key entity -> m entity
  belongsTo          :: (MonadIO m, PS.PersistEntity entity2, PS.ToBackendKey SqlBackend entity2) => ConnectionPool -> (entity -> Maybe (PS.Key entity2)) -> entity -> m (Maybe entity2)
  belongsToJust      :: (MonadIO m, PS.PersistEntity entity2, PS.ToBackendKey SqlBackend entity2) => ConnectionPool -> (entity -> PS.Key entity2) -> entity -> m entity2
  insertEntity       :: (MonadIO m) => ConnectionPool -> entity -> m (Entity entity)
  getBy              :: (MonadIO m) => ConnectionPool -> PS.Unique entity -> m (Maybe (Entity entity))
  deleteBy           :: (MonadIO m) => ConnectionPool -> PS.Unique entity -> m ()
  insertUnique       :: (MonadIO m) => ConnectionPool -> entity -> m (Maybe (PS.Key entity))
  selectSourceRes    :: (MonadIO m1, MonadIO m2) => ConnectionPool -> [Filter entity] -> [SelectOpt entity] -> m1 (Acquire (Source m2 (Entity entity)))
  upsert             :: (MonadIO m) => ConnectionPool -> entity -> [Update entity] -> m (Entity entity)
  upsertBy           :: (MonadIO m) => ConnectionPool -> PS.Unique entity -> entity -> [Update entity] -> m (Entity entity)
  getByValue         :: (MonadIO m) => ConnectionPool -> entity -> m (Maybe (Entity entity))
  insertBy           :: (MonadIO m) => ConnectionPool -> entity -> m (Either (Entity entity) (PS.Key entity))
  checkUnique        :: (MonadIO m) => ConnectionPool -> entity -> m (Maybe (PS.Unique entity))
  onlyUnique         :: (MonadIO m) => ConnectionPool -> entity -> m (PS.Unique entity)
  selectFirst        :: (MonadIO m) => ConnectionPool -> [Filter entity] -> [SelectOpt entity] -> m (Maybe (Entity entity))
  selectKeysRes      :: (MonadIO m1, MonadIO m2) => ConnectionPool -> [Filter entity] -> [SelectOpt entity] -> m1 (Acquire (Source m2 (PS.Key entity)))
  count              :: (MonadIO m) => ConnectionPool -> [Filter entity] -> m Int
  updateWhere        :: (MonadIO m) => ConnectionPool -> [Filter entity] -> [Update entity] -> m ()
  deleteWhere        :: (MonadIO m) => ConnectionPool -> [Filter entity] -> m ()
  selectSource       :: (PS.PersistQueryRead (PS.BaseBackend backend), MonadResource m, PS.PersistEntityBackend entity ~ PS.BaseBackend (PS.BaseBackend backend), MonadReader backend m, PS.HasPersistBackend backend) => [Filter entity] -> [SelectOpt entity] -> Source m (Entity entity)
  selectSource filters conds    = PS.selectSource filters conds
  selectKeys         :: (PS.PersistQueryRead (PS.BaseBackend backend), MonadResource m, PS.BaseBackend (PS.BaseBackend backend) ~ PS.PersistEntityBackend entity, MonadReader backend m, PS.HasPersistBackend backend) => [Filter entity] -> [SelectOpt entity] -> Source m (PS.Key entity)
  selectKeys filters conds      = PS.selectKeys filters conds
  selectList         :: (MonadIO m) => ConnectionPool -> [Filter entity] -> [SelectOpt entity] -> m [Entity entity]
  selectKeysList     :: (MonadIO m) => ConnectionPool -> [Filter entity] -> [SelectOpt entity] -> m [PS.Key entity]

  save             :: (MonadIO m) => ConnectionPool -> entity -> [Update entity] -> m (Entity entity)
  saveBy           :: (MonadIO m) => ConnectionPool -> PS.Unique entity -> entity -> [Update entity] -> m (Entity entity)
  save = upsert
  saveBy = upsertBy

  exist            :: (MonadIO m) => ConnectionPool -> entity -> m Bool
  exist conn record = do
    res <- checkUnique conn record
    case res of
      Nothing -> return True
      _       -> return False 

  get    conn key               = do { runDB conn $ PS.get key }
  insert conn record            = do { runDB conn $ PS.insert record }  
  insert_ conn record           = do { runDB conn $ PS.insert_ record }  
  insertMany conn records       = do { runDB conn $ PS.insertMany records }  
  insertMany_ conn records      = do { runDB conn $ PS.insertMany_ records }  
  insertEntityMany conn records = do { runDB conn $ PS.insertEntityMany records }  
  insertKey conn key record     = do { runDB conn $ PS.insertKey key record }  
  repsert conn key record       = do { runDB conn $ PS.repsert key record }  
  replace conn key record       = do { runDB conn $ PS.replace key record }  
  delete conn key               = do { runDB conn $ PS.delete key }  
  update conn key conds         = do { runDB conn $ PS.update key conds }  
  updateGet conn key conds      = do { runDB conn $ PS.updateGet key conds }
  getJust conn key              = do { runDB conn $ PS.getJust key }
  belongsTo conn idfunc r       = do { runDB conn $ PS.belongsTo idfunc r }
  belongsToJust conn idfunc r   = do { runDB conn $ PS.belongsToJust idfunc r }  
  insertEntity conn r           = do { runDB conn $ PS.insertEntity r }
  getBy conn uniq               = do { runDB conn $ PS.getBy uniq }
  deleteBy conn uniq            = do { runDB conn $ PS.deleteBy uniq }  
  insertUnique conn r           = do { runDB conn $ PS.insertUnique r }
  upsert conn r updates         = do { runDB conn $ PS.upsert r updates }
  upsertBy conn uniq r updates  = do { runDB conn $ PS.upsertBy uniq r updates }
  getByValue conn r             = do { runDB conn $ PS.getByValue r }
  insertBy conn r               = do { runDB conn $ PS.insertBy r }
  checkUnique conn r            = do { runDB conn $ PS.checkUnique r }  
  onlyUnique conn r             = do { runDB conn $ PS.onlyUnique r }  
  selectSourceRes conn filters conds = do { runDB conn $ PS.selectSourceRes filters conds }
  selectFirst conn filters conds     = do { runDB conn $ PS.selectFirst filters conds }  
  selectKeysRes conn filters conds   = do { runDB conn $ PS.selectKeysRes filters conds }
  count conn filters                 = do { runDB conn $ PS.count filters }
  updateWhere conn filters updates   = do { runDB conn $ PS.updateWhere filters updates }
  deleteWhere conn filters           = do { runDB conn $ PS.deleteWhere filters }
  selectList conn filters conds      = do { runDB conn $ PS.selectList filters conds }
  selectKeysList conn filters conds  = do { runDB conn $ PS.selectKeysList filters conds }


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
