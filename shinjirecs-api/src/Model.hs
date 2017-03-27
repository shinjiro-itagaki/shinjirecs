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
import Control.Exception(catch) -- base
import Control.Monad.IO.Class(MonadIO,liftIO) -- base
import qualified Database.Persist.Class as PS
import Database.Persist.Sql(ConnectionPool, SqlPersistT, runSqlPool, toSqlKey)  --persistent
import Database.Persist.Sql.Types.Internal (SqlBackend)
import Database.Persist.Types (Update,Entity(..),Filter,SelectOpt)
import qualified DB
import Server (ActionM)
import Control.Monad.Trans.Resource(MonadResource) -- resourcet
import Control.Monad.Reader(ReaderT) -- mtl
import Control.Monad.Reader.Class(MonadReader) -- mtl
import Data.Acquire(Acquire) -- resourcet
import Data.Conduit(Source) --- conduit
import Data.Int(Int64) -- base
import Helper(ResultClass(..), (=<<&&.),(.&&>>=),(<||>),(.||>>=),(=<<||.))

import qualified Database.Persist as P --persistent

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
  beforeValidation, beforeValidation_ :: (Maybe (PS.Key entity), entity) -> ReaderT SqlBackend IO (Bool, (Maybe (PS.Key entity), entity))
  beforeValidation = return . toSuccess

  beforeValidation_ arg = do
    -- liftIO $ putStrLn "beforeValidation_"
    beforeValidation arg
  
  -- please override if you need
  validate,  afterValidation,  beforeSave,  afterSaved, validate_, afterValidation_, beforeSave_, afterSaved_ :: (Maybe (PS.Key entity), entity) -> ReaderT SqlBackend IO (Bool, (Maybe (PS.Key entity), entity))
  validate = return . toSuccess
  validate_ arg = do
    liftIO $ putStrLn "validate_"
    validate arg
  
  -- please override if you need
  afterValidation = return . toSuccess
  afterValidation_ arg = do
    liftIO $ putStrLn "afterValidation"
    afterValidation arg

  -- please override if you need
  afterValidationFailed, afterValidationFailed_ :: (Maybe (PS.Key entity), entity) -> ReaderT SqlBackend IO (Maybe (PS.Key entity), entity)
  afterValidationFailed = return
  afterValidationFailed_ arg = do
    liftIO $ putStrLn "afterValidationFailed"
    afterValidationFailed arg
  
  -- please override if you need
  beforeSave = return . toSuccess
  beforeSave_ arg = do
    liftIO $ putStrLn "beforeSave"
    beforeSave arg

  -- please override if you need
  afterSaved = return . toSuccess
  afterSaved_ arg = do
    liftIO $ putStrLn "afterSaved"
    afterSaved arg

  afterSaveFailed,  afterModifyFailed, afterSaveFailed_, afterModifyFailed_ :: (Maybe (PS.Key entity), entity) -> ReaderT SqlBackend IO (Maybe (PS.Key entity), entity)
  
  -- please override if you need  
  afterSaveFailed = return
  afterSaveFailed_ arg = do
    liftIO $ putStrLn "afterSaveFailed"
    afterSaveFailed arg
  
  -- please override if you need
  beforeCreate, beforeCreate_ :: entity -> ReaderT SqlBackend IO (Bool, entity)
  beforeCreate = return . toSuccess
  beforeCreate_ arg = do
    liftIO $ putStrLn "beforeCreate"
    beforeCreate arg

  afterCreated,  beforeModify,  afterModified, afterCreated_, beforeModify_, afterModified_ :: (Maybe (PS.Key entity), entity) -> ReaderT SqlBackend IO (Bool, (Maybe (PS.Key entity), entity))

  -- please override if you need  
  afterCreated = return . toSuccess
  afterCreated_ arg = do
    liftIO $ putStrLn "afterCreate"
    afterCreated arg
    
  -- please override if you need
  afterCreateFailed, afterCreateFailed_ :: entity -> ReaderT SqlBackend IO entity
  afterCreateFailed = return
  afterCreateFailed_ arg = do
    liftIO $ putStrLn "afterCreateFailed"
    afterCreateFailed arg

  -- please override if you need
  beforeModify = return . toSuccess
  beforeModify_ arg = do
    liftIO $ putStrLn "beforeModify"
    beforeModify arg

  -- please override if you need
  afterModified = return . toSuccess
  afterModified_ arg = do
    liftIO $ putStrLn "afterModified"
    afterModified arg

  -- please override if you need
  afterModifyFailed = return
  afterModifyFailed_ arg = do
    liftIO $ putStrLn "afterModifyFailed"
    afterModifyFailed arg

  beforeDestroy, afterDestroyed :: Entity entity -> ReaderT SqlBackend IO (Bool, (Entity entity))
  
  -- please override if you need  
  beforeDestroy = return . toSuccess

  -- please override if you need
  afterDestroyed = return . toSuccess

  -- please override if you need
  afterDestroyFailed :: Entity entity -> ReaderT SqlBackend IO (Entity entity)
  afterDestroyFailed = return

  -- please override if you need
  afterCommit, afterCommit_ :: (Maybe (PS.Key entity), entity) -> ReaderT SqlBackend IO (Bool, (Maybe (PS.Key entity), entity))
  afterCommit = return . toSuccess
  afterCommit_ arg = do
    liftIO $ putStrLn "afterCommit"
    afterCommit arg

  -- please override if you need
  afterRollback, afterRollback_ :: (Maybe (PS.Key entity), entity) -> ReaderT SqlBackend IO (Maybe (PS.Key entity), entity)
  afterRollback = return
  afterRollback_ arg = do
    liftIO $ putStrLn "afterRollback"
    afterRollback arg
  
  existOnDb :: PS.Key entity -> ReaderT SqlBackend IO Bool
  existOnDb key = (return . isJust) =<< (PS.liftPersist $ PS.get key)
  
  existOnDbBy :: entity -> ReaderT SqlBackend IO Bool
  existOnDbBy entity= (return . isJust) =<< (PS.liftPersist . PS.getBy) =<< (PS.liftPersist $ PS.onlyUnique entity)
  
  modifyWithoutHooks :: PS.Key entity -> entity -> ReaderT SqlBackend IO (Bool, (Maybe (PS.Key entity), entity))
  modifyWithoutHooks key entity = PS.replace key entity >> afterSaveWithoutHooks entity key
    
  createWithoutHooks :: entity -> ReaderT SqlBackend IO (Bool, (Maybe (PS.Key entity), entity))
  createWithoutHooks entity = do
    key <- PS.insert entity
    afterSaveWithoutHooks entity key

  afterSaveWithoutHooks :: entity -> PS.Key entity -> ReaderT SqlBackend IO (Bool, (Maybe (PS.Key entity), entity))
  afterSaveWithoutHooks old_entity key =
    let ifNothing' = (False, (Nothing, old_entity)) in
    findByKey key >>= return . maybe ifNothing' (\(Entity k v) -> (True, (Just k, v)))

  destroyWithoutHook :: Entity entity -> ReaderT SqlBackend IO (Bool, Entity entity)
  destroyWithoutHook entity = let key = entityKey entity in PS.delete key >> PS.get key >>= return . swap . (,) entity . isNothing

  destroyImpl :: record -> Entity entity -> ReaderT SqlBackend IO (Bool, (Entity entity))
  destroyImpl _ = beforeDestroy .&&>>= destroyWithoutHook .||>>= (afterDestroyed <||> afterDestroyFailed)


class (PS.PersistEntity entity) => ToMaybeEntity entity a where
  toMaybeEntity :: a -> Maybe (Entity entity)

instance (PS.PersistEntity entity) => (ToMaybeEntity entity) (Maybe (PS.Key entity), entity) where
  toMaybeEntity (Just key, e) = Just $ Entity {entityKey = key, entityVal = e}
  toMaybeEntity _             = Nothing

instance (PS.PersistEntity entity) => (ToMaybeEntity entity) (Bool, (Maybe (PS.Key entity), entity)) where
  toMaybeEntity (True,  a) = toMaybeEntity a
  toMaybeEntity (False, a) = Nothing
  
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
        beforeValidation_ .&&>>= validate_ .||>>= (afterValidation_ <||> afterValidationFailed_) .&&>>= beforeSave_

      beforeActionCreateOrUpdate' =
        case saveType' of
          Modify -> beforeModify_
          Create -> \(mkey, entity) -> beforeCreate_ entity >>= \(b, entity2) -> return (b, (Nothing, entity2))
        
      beforeActionAll' =
        beforeActionCommon' .&&>>= beforeActionCreateOrUpdate'

      main' =
        (case saveType' of
          Modify -> modifyWithoutHooks key'
          Create -> createWithoutHooks
        ) . snd
          
      afterActionCreatedOrUpdated' =
        case saveType' of
          Modify -> afterModified_
          Create -> afterCreated_

      afterActionCommon' = (\a -> return (True,a)) .||>>= (afterSaved_ <||> afterSaveFailed_) .||>>= (afterCommit_ <||> afterRollback_)
      afterActionAll'    = afterActionCreatedOrUpdated' .&&>>= afterActionCommon'

      saveType' = if isJust mkey then Modify else Create

findByKey :: (ActiveRecord entity) => PS.Key entity -> ReaderT SqlBackend IO (Maybe (Entity entity))
findByKey key = PS.get key >>= maybe (return Nothing) (\x -> afterFind x >>= return . Just . Entity key)
    
find :: (Read id, Show id, ActiveRecord entity) => id -> ReaderT SqlBackend IO (Maybe (Entity entity))
find = let ifNothing' = -1 in findByKey . toSqlKey . fromMaybe ifNothing' . readMaybe . show

selectBy :: (ActiveRecord e) => [P.Filter e] -> [P.SelectOpt e] -> ReaderT SqlBackend IO [Entity e]
selectBy filters opts = do
  list <- PS.liftPersist $ P.selectList filters opts
  mapM (\(Entity k v) -> afterFind v >>= return . Entity k) list
          
instance (PS.PersistEntity entity, PS.ToBackendKey SqlBackend entity, PS.PersistRecordBackend entity SqlBackend) => ActiveRecord entity

instance (ActiveRecord entity) => (ActiveRecordSaver entity) entity where
  exist = existOnDbBy
  save self = saveImpl self (Nothing, self)

instance (ActiveRecord entity) => (ActiveRecordSaver entity) (Entity entity) where
  exist = existOnDb . entityKey
  save self@(Entity k v) = saveImpl self (Just k, v)

saveE :: (ActiveRecord entity) => Entity entity -> ReaderT SqlBackend IO (Bool, (Maybe (PS.Key entity), entity))
saveE x = save x

saveR :: (ActiveRecord entity) => entity -> ReaderT SqlBackend IO (Bool, (Maybe (PS.Key entity), entity))
saveR x = save x

class (ActiveRecord entity) => (ActiveRecordDestroyer entity) record where
  -- need to implement to call destroyImpl
  destroy :: record -> ReaderT SqlBackend IO (Bool, record, PS.Key entity)

instance (ActiveRecord entity) => ActiveRecordDestroyer entity (Entity entity) where
  destroy self@(Entity k v) = destroyImpl self self >>= (\(b, self') -> return (b, self', k))

-- send key directly
instance (ActiveRecord entity) => ActiveRecordDestroyer entity (PS.Key entity) where
  destroy key = findByKey key >>= maybe (return (False, key, key)) (\e -> destroyImpl key e >>= (\(b, e2) -> return (b, key, key)))

class (PS.PersistEntity record, PS.DeleteCascade record SqlBackend) => CascadeDeletable record where
  deleteCascade      :: (MonadIO m) => ConnectionPool -> PS.Key record -> m ()
  deleteCascadeWhere :: (MonadIO m) => ConnectionPool -> [Filter record] -> m ()  
  -- deleteCascade conn key             = do { runDB conn $ PS.deleteCascade key }
  -- deleteCascadeWhere conn filters    = do { runDB conn $ PS.deleteCascadeWhere filters }

class (PS.PersistEntity record, Eq record, Eq (PS.Unique record) )=> UniqueReplaceable record where
  replaceUnique :: (MonadIO m) => ConnectionPool -> PS.Key record -> record -> m (Maybe (PS.Unique record))
--  replaceUnique conn key r = do { runDB conn $ PS.replaceUnique key r }

