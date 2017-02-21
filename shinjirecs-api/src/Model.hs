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
import Data.Maybe(fromMaybe, isJust, fromJust) -- !!!
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
import Lib(ResultClass(..), (=<<&&.),(.&&>>=),(<||>),(.||>>=),(=<<||.))

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

data SaveType = Modify | Create

instance (PS.PersistEntity e) => ResultClass (Maybe (PS.Key e), e) (Bool, (Maybe (PS.Key e), e)) where
  toResult b r = (b,r)
  isSuccess r dummy = fst r
  returnValue = snd

instance (PS.PersistEntity e) => ResultClass (Entity e) (Bool, Entity e) where
  toResult b r = (b,r)
  isSuccess r dummy = fst r
  returnValue = snd

--instance (PS.PersistEntity e) => ResultClass e (Bool, e) where
--  toResult b r = (b,r)
--  isSuccess r dummy = fst r
--  returnValue = snd

class (PS.PersistEntity entity, PS.ToBackendKey SqlBackend entity, PS.PersistRecordBackend entity SqlBackend) => ActiveRecord entity where

  -- please override if you need
  afterFind :: entity -> ReaderT SqlBackend IO entity
  afterFind = return
  
  -- please override if you need
  beforeValidation :: (Maybe (PS.Key entity), entity) -> ReaderT SqlBackend IO (Bool, (Maybe (PS.Key entity), entity))
  beforeValidation = return . toSuccess
  
  -- please override if you need
  validate :: (Maybe (PS.Key entity), entity) -> ReaderT SqlBackend IO (Bool, (Maybe (PS.Key entity), entity))
  validate = return . toSuccess
  
  -- please override if you need
  afterValidation :: (Maybe (PS.Key entity), entity) -> ReaderT SqlBackend IO (Bool, (Maybe (PS.Key entity), entity))
  afterValidation = return . toSuccess

  -- please override if you need
  afterValidationFailed :: (Maybe (PS.Key entity), entity) -> ReaderT SqlBackend IO (Maybe (PS.Key entity), entity)
  afterValidationFailed = return
  
  -- please override if you need
  beforeSave :: (Maybe (PS.Key entity), entity) -> ReaderT SqlBackend IO (Bool, (Maybe (PS.Key entity), entity))
  beforeSave = return . toSuccess

  -- please override if you need
  afterSaved :: (Maybe (PS.Key entity), entity) -> ReaderT SqlBackend IO (Bool, (Maybe (PS.Key entity), entity))
  afterSaved = return . toSuccess

  -- please override if you need
  afterSaveFailed :: (Maybe (PS.Key entity), entity) -> ReaderT SqlBackend IO (Maybe (PS.Key entity), entity)
  afterSaveFailed = return
  
  -- please override if you need
  beforeCreate :: entity -> ReaderT SqlBackend IO (Bool, entity)
  beforeCreate val = return (True, val)

  -- please override if you need
  afterCreated :: (Maybe (PS.Key entity), entity) -> ReaderT SqlBackend IO (Bool, (Maybe (PS.Key entity), entity))
  afterCreated = return . toSuccess

  -- please override if you need
  afterCreateFailed :: (Maybe (PS.Key entity), entity) -> ReaderT SqlBackend IO (Maybe (PS.Key entity), entity)
  afterCreateFailed = return

  -- please override if you need
  beforeModify :: (Maybe (PS.Key entity), entity) -> ReaderT SqlBackend IO (Bool, (Maybe (PS.Key entity), entity))
  beforeModify = return . toSuccess

  -- please override if you need
  afterModified :: (Maybe (PS.Key entity), entity) -> ReaderT SqlBackend IO (Bool, (Maybe (PS.Key entity), entity))
  afterModified = return . toSuccess

  -- please override if you need
  afterModifyFailed :: (Maybe (PS.Key entity), entity) -> ReaderT SqlBackend IO (Maybe (PS.Key entity), entity)
  afterModifyFailed = return

  -- please override if you need
  beforeDestroy :: Entity entity -> ReaderT SqlBackend IO Bool
  beforeDestroy _ = return True

  -- please override if you need
  afterDestroyed :: (Maybe (PS.Key entity), entity) -> ReaderT SqlBackend IO (Bool, (Maybe (PS.Key entity), entity))
  afterDestroyed = return . toSuccess

  -- please override if you need
  afterDestroyFailed :: (Maybe (PS.Key entity), entity) -> ReaderT SqlBackend IO (Maybe (PS.Key entity), entity)
  afterDestroyFailed = return

  -- please override if you need
  afterCommit :: (Maybe (PS.Key entity), entity) -> ReaderT SqlBackend IO (Bool, (Maybe (PS.Key entity), entity))
  afterCommit = return . toSuccess

  -- please override if you need
  afterRollback :: (Maybe (PS.Key entity), entity) -> ReaderT SqlBackend IO (Maybe (PS.Key entity), entity)
  afterRollback = return
  
  existOnDb :: PS.Key entity -> ReaderT SqlBackend IO Bool
  existOnDb key = (return . isJust) =<< (PS.liftPersist $ PS.get key)
  
  existOnDbBy :: entity -> ReaderT SqlBackend IO Bool
  existOnDbBy entity= (return . isJust) =<< (PS.liftPersist . PS.getBy) =<< (PS.liftPersist $ PS.onlyUnique entity)
  
  modifyWithoutHooks :: PS.Key entity -> entity -> ReaderT SqlBackend IO (Bool, (Maybe (PS.Key entity)), entity)
  modifyWithoutHooks key entity = do
    PS.replace key entity
    mEntity <- PS.liftPersist $ PS.get key
    
    -- need to check updated here
    
    return $ (case mEntity of
                (Just entity2) -> (True,  Just key, entity2)
                Nothing        -> (False, Nothing,  entity ))

  createWithoutHooks :: entity -> ReaderT SqlBackend IO (Bool, Maybe (PS.Key entity), entity)
  createWithoutHooks entity = do
    
    -- need to check failed(exception thrown) here
    
    key <- PS.liftPersist $ PS.insert entity
    mEntity <- PS.liftPersist $ PS.get key
    return $ (case mEntity of
                (Just entity2) -> (True,  Just key, entity2)
                Nothing        -> (False, Nothing,  entity ))

-- need MultiParamTypeClasses
-- need AllowAmbiguousTypes
class (PS.PersistEntity entity, PS.ToBackendKey SqlBackend entity, PS.PersistRecordBackend entity SqlBackend) => (ActiveRecordSaver entity) record where

  -- need to implement createWithoutHooks or modifyWithoutHooks
  saveWithoutHooks :: record -> ReaderT SqlBackend IO (Bool, Maybe (PS.Key entity), entity)

  -- need to implement, select existOnDb or existOnDbBy
  exist :: record -> ReaderT SqlBackend IO Bool

  -- need to implement to call saveImpl
  save :: record -> ReaderT SqlBackend IO (Bool, Maybe (PS.Key entity), entity)

  -- first argument is not used but it is required for determine type
  saveImpl :: record -> Maybe (PS.Key entity) -> entity -> ReaderT SqlBackend IO (Bool, Maybe (PS.Key entity), entity)
  saveImpl self mkey val = do
    return (True, mkey, val)
    where
      key' = fromJust mkey

      -- beforeCommon :: (Monad m) => a -> m (Bool, a)
      beforeActionCommon' =
        beforeValidation .&&>>= validate .||>>= (afterValidation <||> afterValidationFailed) .&&>>= beforeSave

      beforeActionCreateOrUpdate' =
        case saveType' of
          Modify -> beforeModify
          Create -> \(mkey, entity) -> beforeCreate entity >>= \(b, entity2) -> return (b, (Nothing, entity2))
        
      beforeActionAll' =
        beforeActionCommon' .&&>>= beforeActionCreateOrUpdate'

      main' =
        case saveType' of
          Modify -> modifyWithoutHooks key'
          Create -> createWithoutHooks

      afterActionCreatedOrUpdated' =
        case saveType' of
          Modify -> afterModified
          Create -> afterCreated

      afterActionCommon' = (\a -> return (True,a)) .||>>= (afterSaved <||> afterSaveFailed) .||>>= (afterCommit <||> afterRollback)
      afterActionAll'    = afterActionCreatedOrUpdated' .&&>>= afterActionCommon'

      dummyFunc1' = beforeActionAll' (mkey,val) -- def for clearifing type of toBeforeAll'
      dummyFunc2' = afterActionAll'  (mkey,val)
      saveType' = if isJust mkey then Modify else Create
        
class (PS.PersistEntity entity, PS.ToBackendKey SqlBackend entity, PS.PersistRecordBackend entity SqlBackend) => (ActiveRecordDestroyer entity) record where
  destroyWithoutHook :: record -> ReaderT SqlBackend IO (Maybe (PS.Key entity))

  {-
  beforeDestroy
  afterDestroy
  -}
  destroy :: record -> ReaderT SqlBackend IO (Maybe (PS.Key entity))

findByKey :: (ActiveRecord entity) => PS.Key entity -> ReaderT SqlBackend IO (Maybe (Entity entity))
findByKey key = impl' =<< PS.get key
  where
    impl' (Just x) = (\val' -> return $ Just $ Entity key val') =<< afterFind x
    impl' _        = return Nothing
    
find :: (Read id, Show id, ActiveRecord entity) => id -> ReaderT SqlBackend IO (Maybe (Entity entity))
find = let ifNothing' = -1 in findByKey . toSqlKey . fromMaybe ifNothing' . readMaybe . show
          
instance (PS.PersistEntity entity, PS.ToBackendKey SqlBackend entity, PS.PersistRecordBackend entity SqlBackend) => ActiveRecord entity

instance (ActiveRecord entity) => (ActiveRecordSaver entity) entity where
  exist = existOnDbBy
  saveWithoutHooks = createWithoutHooks
  save self = saveImpl self Nothing self

instance (ActiveRecord entity) => ActiveRecordDestroyer entity (Entity entity) where
  -- destroyWithoutHook :: record -> ReaderT SqlBackend IO (Maybe (PS.Key entity))
  destroyWithoutHook record = do
    let key = entityKey record
    PS.delete key

    -- check destroyed here
    -- if destroy failed then
    --  return Nothing
    -- else
    --  return Just key
      
    return $ Just key
    
  -- destroy :: record -> ReaderT SqlBackend IO (Maybe (PS.Key entity))    
  destroy record@(Entity key val) = do
    res <- beforeDestroy record
    if res then
      destroyWithoutHook record -- ReaderT SqlBackend IO (Maybe (PS.Key entity))
    else
      return Nothing

class (PS.PersistEntity record, PS.DeleteCascade record SqlBackend) => CascadeDeletable record where
  deleteCascade      :: (MonadIO m) => ConnectionPool -> PS.Key record -> m ()
  deleteCascadeWhere :: (MonadIO m) => ConnectionPool -> [Filter record] -> m ()  
  -- deleteCascade conn key             = do { runDB conn $ PS.deleteCascade key }
  -- deleteCascadeWhere conn filters    = do { runDB conn $ PS.deleteCascadeWhere filters }

class (PS.PersistEntity record, Eq record, Eq (PS.Unique record) )=> UniqueReplaceable record where
  replaceUnique :: (MonadIO m) => ConnectionPool -> PS.Key record -> record -> m (Maybe (PS.Unique record))
--  replaceUnique conn key r = do { runDB conn $ PS.replaceUnique key r }
