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
import Data.Maybe(fromMaybe, isJust) -- !!!
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
  afterCommit :: entity -> ReaderT SqlBackend IO entity
  afterCommit = return

  -- please override if you need
  afterRollback :: entity -> ReaderT SqlBackend IO entity
  afterRollback = return
  
  existOnDb :: PS.Key entity -> ReaderT SqlBackend IO Bool
  existOnDb key = (return . maybeToBool) =<< (PS.liftPersist $ PS.get key)
  
  existOnDbBy :: entity -> ReaderT SqlBackend IO Bool
  existOnDbBy entity= (return . maybeToBool) =<< (PS.liftPersist . PS.getBy) =<< (PS.liftPersist $ PS.onlyUnique entity)
  
  modifyWithoutHooks :: PS.Key entity -> entity -> ReaderT SqlBackend IO (Maybe (Entity entity))
  modifyWithoutHooks key entity = do
    PS.replace key entity
    mEntity <- PS.liftPersist $ PS.get key
    
    -- need to check updated here
    
    return $ (case mEntity of
                Just entity2 -> Just Entity {entityKey = key, entityVal = entity2 }
                Nothing -> Nothing)      

  createWithoutHooks :: entity -> ReaderT SqlBackend IO (Maybe (Entity entity))
  createWithoutHooks entity = do
    
    -- need to check failed(exception thrown) here
    
    key <- PS.liftPersist $ PS.insert entity
    mEntity <- PS.liftPersist $ PS.get key
    return $ (case mEntity of
                Just entity2 -> Just Entity {entityKey = key, entityVal = entity2 }
                Nothing      -> Nothing )

--  before self (beforeValidation saveType) afterValidation afterValidationFailed (beforeSave saveType)
before :: (Monad m) => a -- self m = ReaderT SqlBackend IO
       -> (a -> m (Bool, a)) -- validator
       -> ((a -> m (Bool, a)) -- ok
          ,(a -> m a)) -- failed
       -> (a -> m (Bool, a)) -- after ok
       -> m (Bool, a) -- return
before self validator (ok, failed) afterOk = do
  (res1, r1) <- validator self
  (res2, r2) <- if res1 then ok r1 else failed r1 >>= (\r' -> return (False,r'))
  if res2 then afterOk r2 else return (res2, r2)
      
-- need MultiParamTypeClasses
-- need AllowAmbiguousTypes
class (PS.PersistEntity entity, PS.ToBackendKey SqlBackend entity, PS.PersistRecordBackend entity SqlBackend) => (ActiveRecordSaver entity) record where

  -- need to implement createWithoutHooks or modifyWithoutHooks
  saveWithoutHooks :: record -> ReaderT SqlBackend IO (Maybe (Entity entity))

  -- need to implement, select existOnDb or existOnDbBy
  exist :: record -> ReaderT SqlBackend IO Bool

  -- need to implement to call saveImpl
  save :: record -> ReaderT SqlBackend IO (Bool, Maybe (PS.Key entity), entity)

  {- flow on save
  save mkey val = do
    let saveType = exist self ? Modify |:| Create in
    before self (beforeValidation saveType) afterValidation afterValidationFailed (beforeSave saveType)
    main
      (case saveType of
        MODIFY -> beforeModify =>> modifyWithoutHooks =>> (afterModified <||> afterModifyFailed)
        CREATE -> beforeCreate =>> createWithoutHooks =>> (afterCreated  <||> afterCreateFailed)
      )
    after
      (afterSaved <||> afterSaveFailed) =>> (afterCommit <||> afterRollback)
  -}

  -- first argument is not used but it is required for determine type
  saveImpl :: record -> Maybe (PS.Key entity) -> entity -> ReaderT SqlBackend IO (Bool, Maybe (PS.Key entity), entity)
  saveImpl self mkey val = do
    (res, val2) <-
      before
        val
        (beforeValidation saveType')
        (afterValidation, afterValidationFailed)
        (beforeSave saveType')
    return (res, mkey, val2)
    where
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
