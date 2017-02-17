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

data SaveType = Modify | Create

class (PS.PersistEntity entity, PS.ToBackendKey SqlBackend entity, PS.PersistRecordBackend entity SqlBackend) => ActiveRecord entity where
  isSuccess :: (Bool, entity) -> Bool
  isSuccess (b, v) = b
  
  returnValue :: (Bool, entity) -> entity
  returnValue (b, v) = v
  
  (=<<.) :: (Monad m) => (entity -> m (Bool, entity)) -> (entity -> m (Bool, entity)) -> (entity -> m (Bool, entity))
  (=<<.) f1 f2 = (\arg -> f1 arg >>= (\res -> let arg2 = returnValue res in if isSuccess res then f2 arg2 else return res))
  infixr 8 =<<.


  (.>>=) :: (Monad m) => (entity -> m (Bool, entity)) -> (entity -> m (Bool, entity)) -> (entity -> m (Bool, entity))
  (.>>=) f1 f2 = f2 =<<. f1
  infixl 8 .>>=

  (<||>) :: (Monad m) =>
             (entity -> m (Bool, entity)) -- on success
          -> (entity -> m entity) -- on failed
          -> ((Bool, entity) -> m (Bool, entity)) -- result
  (<||>) onSuccess onFailed =
    \arg ->
      let rtn = returnValue arg in
        if isSuccess arg
        then onSuccess rtn
        else onFailed rtn
             >>= (\rtn2 -> return (False, rtn2))

  infix 7 <||>

  (.>>||) :: (Monad m) =>
             (        entity -> m (Bool, entity))
          -> ((Bool, entity) -> m (Bool, entity))
          -> (        entity -> m (Bool, entity))
          
  (.>>||) f1 f2 = (\arg -> f1 arg >>= f2)
  infixl 8 .>>||

  (||<<.) :: (Monad m) =>
             (        entity -> m (Bool, entity))
          -> ((Bool, entity) -> m (Bool, entity))
          -> (        entity -> m (Bool, entity))    
  (||<<.) f1 f2 = f1 .>>|| f2 

  -- ($>>) v f1 = f1 v 

  -- please override if you need
  afterFind :: entity -> ReaderT SqlBackend IO entity
  afterFind = return
  
  -- please override if you need
  beforeValidation :: SaveType -> entity -> ReaderT SqlBackend IO (Bool, entity)
  beforeValidation type' = return . (,) True
  
  -- please override if you need
  validate :: entity -> ReaderT SqlBackend IO (Bool, entity)
  validate = return . (,) True
  
  -- please override if you need
  afterValidation :: entity -> ReaderT SqlBackend IO (Bool, entity)
  afterValidation = return . (,) True

  -- please override if you need
  afterValidationFailed :: entity -> ReaderT SqlBackend IO entity
  afterValidationFailed = return
  
  -- please override if you need
  beforeSave :: SaveType -> entity -> ReaderT SqlBackend IO (Bool, entity)
  beforeSave _ = return . (,) True

  -- please override if you need
  afterSaved :: SaveType -> entity -> ReaderT SqlBackend IO (Bool, entity)
  afterSaved _ = return . (,) True

  -- please override if you need
  afterSaveFailed :: SaveType -> entity -> ReaderT SqlBackend IO entity
  afterSaveFailed _ = return
  
  -- please override if you need
  beforeCreate :: entity -> ReaderT SqlBackend IO (Bool, entity)
  beforeCreate = return . (,) True

  -- please override if you need
  afterCreated :: entity -> ReaderT SqlBackend IO (Bool, entity)
  afterCreated = return . (,) True

  -- please override if you need
  afterCreateFailed :: entity -> ReaderT SqlBackend IO entity
  afterCreateFailed = return

  -- please override if you need
  beforeModify :: entity -> ReaderT SqlBackend IO (Bool, entity)
  beforeModify = return . (,) True

  -- please override if you need
  afterModified :: entity -> ReaderT SqlBackend IO (Bool, entity)
  afterModified = return . (,) True

  -- please override if you need
  afterModifyFailed :: entity -> ReaderT SqlBackend IO entity
  afterModifyFailed = return

  -- please override if you need
  beforeDestroy :: entity -> ReaderT SqlBackend IO (Bool, entity)
  beforeDestroy = return . (,) True

  -- please override if you need
  afterDestroyed :: entity -> ReaderT SqlBackend IO (Bool, entity)
  afterDestroyed = return . (,) True

  -- please override if you need
  afterDestroyFailed :: entity -> ReaderT SqlBackend IO entity
  afterDestroyFailed = return

  -- please override if you need
  afterCommit :: entity -> ReaderT SqlBackend IO (Bool, entity)
  afterCommit = return . (,) True

  -- please override if you need
  afterRollback :: entity -> ReaderT SqlBackend IO entity
  afterRollback = return
  
  existOnDb :: PS.Key entity -> ReaderT SqlBackend IO Bool
  existOnDb key = (return . maybeToBool) =<< (PS.liftPersist $ PS.get key)
  
  existOnDbBy :: entity -> ReaderT SqlBackend IO Bool
  existOnDbBy entity= (return . maybeToBool) =<< (PS.liftPersist . PS.getBy) =<< (PS.liftPersist $ PS.onlyUnique entity)
  
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
        beforeValidation saveType' .>>|| (afterValidation <||> afterValidationFailed) .>>= beforeSave saveType'

      beforeActionCreateOrUpdate =
        case saveType' of
          Modify -> beforeModify
          Create -> beforeCreate
        
      beforeActionAll' =
        beforeActionCommon' .>>= beforeActionCreateOrUpdate

      main' =
        case saveType' of
          Modify -> modifyWithoutHooks key'
          Create -> createWithoutHooks

      afterActionCreatedOrUpdated' =
        case saveType' of
          Modify -> beforeModify
          Create -> beforeCreate

      afterActionCommon' = (\a -> return (True,a)) .>>|| (afterSaved saveType' <||> afterSaveFailed saveType') .>>|| (afterCommit <||> afterRollback)
      afterActionAll'    = afterActionCreatedOrUpdated' .>>= afterActionCommon'

      dummyFunc1' = beforeActionAll' val -- def for clearifing type of toBeforeAll'
      dummyFunc2' = afterActionAll' val
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

instance (ActiveRecord entity) => ActiveRecordSaver entity (Entity entity) where
  exist = existOnDb . entityKey
  saveWithoutHooks self = modifyWithoutHooks (entityKey self) (entityVal self)
  save self@(Entity key val) = saveImpl self (Just key) val

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
    (res, r) <- beforeDestroy val
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
