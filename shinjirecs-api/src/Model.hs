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
import Data.Maybe(fromMaybe, isJust, isNothing, fromJust) -- !!!
import Data.Tuple(swap)
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

instance ResultClass a (Bool, a) where
  toResult b r = (b,r)
  isSuccess r dummy = fst r
  returnValue = snd

class (PS.PersistEntity entity, PS.ToBackendKey SqlBackend entity, PS.PersistRecordBackend entity SqlBackend) => ActiveRecord entity where

  -- please override if you need
  afterFind :: entity -> ReaderT SqlBackend IO entity
  afterFind = return
  
  -- please override if you need
  beforeValidation :: (Maybe (PS.Key entity), entity) -> ReaderT SqlBackend IO (Bool, (Maybe (PS.Key entity), entity))
  beforeValidation = return . toSuccess
  
  -- please override if you need
  validate, afterValidation, beforeSave, afterSaved :: (Maybe (PS.Key entity), entity) -> ReaderT SqlBackend IO (Bool, (Maybe (PS.Key entity), entity))  
  validate = return . toSuccess
  
  -- please override if you need
  afterValidation = return . toSuccess

  -- please override if you need
  afterValidationFailed :: (Maybe (PS.Key entity), entity) -> ReaderT SqlBackend IO (Maybe (PS.Key entity), entity)
  afterValidationFailed = return
  
  -- please override if you need
  beforeSave = return . toSuccess

  -- please override if you need
  afterSaved = return . toSuccess

  afterSaveFailed, afterModifyFailed :: (Maybe (PS.Key entity), entity) -> ReaderT SqlBackend IO (Maybe (PS.Key entity), entity)
  
  -- please override if you need  
  afterSaveFailed = return
  
  -- please override if you need
  beforeCreate :: entity -> ReaderT SqlBackend IO (Bool, entity)
  beforeCreate = return . toSuccess

  afterCreated, beforeModify, afterModified :: (Maybe (PS.Key entity), entity) -> ReaderT SqlBackend IO (Bool, (Maybe (PS.Key entity), entity))

  -- please override if you need  
  afterCreated = return . toSuccess

  -- please override if you need
  afterCreateFailed :: entity -> ReaderT SqlBackend IO entity
  afterCreateFailed = return

  -- please override if you need
  beforeModify = return . toSuccess

  -- please override if you need

  afterModified = return . toSuccess

  -- please override if you need
  afterModifyFailed = return

  beforeDestroy, afterDestroyed :: Entity entity -> ReaderT SqlBackend IO (Bool, (Entity entity))
  
  -- please override if you need  
  beforeDestroy = return . toSuccess

  -- please override if you need
  afterDestroyed = return . toSuccess

  -- please override if you need
  afterDestroyFailed :: Entity entity -> ReaderT SqlBackend IO (Entity entity)
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
  
  modifyWithoutHooks :: PS.Key entity -> entity -> ReaderT SqlBackend IO (Bool, (Maybe (PS.Key entity), entity))
  modifyWithoutHooks key entity =
    let ifNothing' = (False, (Nothing, entity)) in
    -- need to check updated here
    PS.replace key entity >> findByKey key >>= return . maybe ifNothing' (\(Entity k v) -> (True, (Just k, v)))
    
  createWithoutHooks :: entity -> ReaderT SqlBackend IO (Bool, (Maybe (PS.Key entity), entity))
  createWithoutHooks entity =
    let ifNothing' = (False, (Nothing, entity)) in
    -- need to check failed(exception thrown) here      
    PS.insert entity >>= findByKey >>= return . maybe ifNothing' (\(Entity k v) -> (True, (Just k, v)))

  destroyWithoutHook :: Entity entity -> ReaderT SqlBackend IO (Bool, Entity entity)
  destroyWithoutHook entity = let key = entityKey entity in PS.delete key >> PS.get key >>= return . swap . (,) entity . isNothing

  destroyImpl :: record -> Entity entity -> ReaderT SqlBackend IO (Bool, (Entity entity))
  destroyImpl _ = beforeDestroy .&&>>= destroyWithoutHook .||>>= (afterDestroyed <||> afterDestroyFailed)
  
-- need MultiParamTypeClasses
-- need AllowAmbiguousTypes
class (ActiveRecord entity) => (ActiveRecordSaver entity) record where

  -- need to implement, select existOnDb or existOnDbBy
  exist :: record -> ReaderT SqlBackend IO Bool

  -- need to implement to call saveImpl
  save :: record -> ReaderT SqlBackend IO (Bool, (Maybe (PS.Key entity), entity))

  -- first argument is not used but it is required for determine type
  saveImpl :: record -> (Maybe (PS.Key entity), entity) -> ReaderT SqlBackend IO (Bool, (Maybe (PS.Key entity), entity))
  saveImpl self arg@(mkey, v) = beforeActionAll' .&&>>= main' .&&>>= afterActionAll' $ arg
    where
      key' = fromJust mkey

      beforeActionCommon' =
        beforeValidation .&&>>= validate .||>>= (afterValidation <||> afterValidationFailed) .&&>>= beforeSave

      beforeActionCreateOrUpdate' =
        case saveType' of
          Modify -> beforeModify
          Create -> \(mkey, entity) -> beforeCreate entity >>= \(b, entity2) -> return (b, (Nothing, entity2))
        
      beforeActionAll' =
        beforeActionCommon' .&&>>= beforeActionCreateOrUpdate'

      main' =
        (case saveType' of
          Modify -> modifyWithoutHooks key'
          Create -> createWithoutHooks
        ) . snd
          
      afterActionCreatedOrUpdated' =
        case saveType' of
          Modify -> afterModified
          Create -> afterCreated

      afterActionCommon' = (\a -> return (True,a)) .||>>= (afterSaved <||> afterSaveFailed) .||>>= (afterCommit <||> afterRollback)
      afterActionAll'    = afterActionCreatedOrUpdated' .&&>>= afterActionCommon'

      saveType' = if isJust mkey then Modify else Create

class (ActiveRecord entity) => (ActiveRecordDestroyer entity) record where

  -- need to implement to call destroyImpl
  destroy :: record -> ReaderT SqlBackend IO (Bool, record)
  
findByKey :: (ActiveRecord entity) => PS.Key entity -> ReaderT SqlBackend IO (Maybe (Entity entity))
findByKey key = PS.get key >>= maybe (return Nothing) (\x -> afterFind x >>= return . Just . Entity key)
    
find :: (Read id, Show id, ActiveRecord entity) => id -> ReaderT SqlBackend IO (Maybe (Entity entity))
find = let ifNothing' = -1 in findByKey . toSqlKey . fromMaybe ifNothing' . readMaybe . show
          
instance (PS.PersistEntity entity, PS.ToBackendKey SqlBackend entity, PS.PersistRecordBackend entity SqlBackend) => ActiveRecord entity

instance (ActiveRecord entity) => (ActiveRecordSaver entity) entity where
  exist = existOnDbBy
  save self = saveImpl self (Nothing, self)

instance (ActiveRecord entity) => ActiveRecordSaver entity (Entity entity) where
  exist = existOnDb . entityKey
  save self = saveImpl self (Just $ entityKey self, entityVal self)

instance (ActiveRecord entity) => ActiveRecordDestroyer entity (Entity entity) where
  destroy self = destroyImpl self self

-- send key directly
instance (ActiveRecord entity) => ActiveRecordDestroyer entity (PS.Key entity) where
  destroy key = findByKey key >>= maybe (return (False, key)) (\e -> destroyImpl key e >>= (\(b, e2) -> return (b, key)))

class (PS.PersistEntity record, PS.DeleteCascade record SqlBackend) => CascadeDeletable record where
  deleteCascade      :: (MonadIO m) => ConnectionPool -> PS.Key record -> m ()
  deleteCascadeWhere :: (MonadIO m) => ConnectionPool -> [Filter record] -> m ()  
  -- deleteCascade conn key             = do { runDB conn $ PS.deleteCascade key }
  -- deleteCascadeWhere conn filters    = do { runDB conn $ PS.deleteCascadeWhere filters }

class (PS.PersistEntity record, Eq record, Eq (PS.Unique record) )=> UniqueReplaceable record where
  replaceUnique :: (MonadIO m) => ConnectionPool -> PS.Key record -> record -> m (Maybe (PS.Unique record))
--  replaceUnique conn key r = do { runDB conn $ PS.replaceUnique key r }
