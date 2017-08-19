{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE FunctionalDependencies #-}
-- {-# LANGUAGE ConstraintKinds #-}

module Model (
  ColumnName(..)
  ,FailedReason(..)
  ,ValidationResult(..)
  ,BeforeActionResult(..)
  ,AfterActionResult(..)
  ,BeforeActionStep(..)
  ,SaveResult(..)
  ,BeforeCreate(..)
  ,BeforeModify(..)
  ,CreateResult(..)
  ,ModifyResult(..)
  ,DestroyResult(..)
  ,valid
  ,invalid
  ,go
  ,cancel
  ,commit
  ,rollback
  ,ModelClass(..)
  ,SelectQuery(..)
  ,fireAfterFindME
  ,fireAfterFind
  ,find
  ,get
  ,select
  ,create
  ,modify
  ,HookAndOnCancelActionsPair(..)
  ,(>>==)
  ,(|||)
  ,onCancel
  ,save
  ,destroy
  ) where
import qualified DB
import Data.Int(Int64)
import Data.Maybe(isNothing)
import Control.Monad(sequence)

type ColumnName = String
data FailedReason = TooLarge | TooSmall | NotNull | ReferenceNotFound | Others

data ValidationResult   a = Valid   a | Invalid a [(ColumnName, FailedReason)]
data BeforeActionResult a = Go      a | Cancel a
data AfterActionResult  a = Commit  a | Rollback a

data HookResult a = Validation (ValidationResult a) | BeforeAction (BeforeActionResult a) | AfterAction (AfterActionResult a)

data BeforeActionStep a = OnBeforeValidation | OnValidation | OnBeforeSave | On a

data SaveResult s f c on_x = SaveSuccess s | SaveFailed f [(ColumnName, FailedReason)] | Canceled (BeforeActionStep on_x) c

data BeforeCreate = BeforeCreate
data BeforeModify = BeforeModify

-- SaveSuccess (DB.Entity a) | SaveFailed a [(ColumnName, FailedReason)] | Canceled (OnValidation | OnBeforeSave | On BeforeCreate) a
type CreateResult a = SaveResult (DB.Entity a)            a             a  BeforeCreate

-- SaveSuccess (DB.Entity a) | SaveFailed (DB.Entity a) [(ColumnName, FailedReason)] | Canceled (OnValidation | OnBeforeSave | On BeforeModify) (DB.Entity a)
type ModifyResult a = SaveResult (DB.Entity a) (DB.Entity a) (DB.Entity a) BeforeModify
data DestroyResult a = DeleteSuccess a | RecordNotFound | DeleteFailed a | DeleteCanceled a

valid :: a -> IO (ValidationResult a)
valid = return . Valid

invalid :: a -> [(ColumnName, FailedReason)] -> IO (ValidationResult a)
invalid x ys = return $ Invalid x ys

go :: a -> IO (BeforeActionResult a)
go = return . Go

cancel :: a -> IO (BeforeActionResult a)
cancel = return . Cancel

commit :: a -> IO (AfterActionResult a)
commit = return . Commit

rollback :: a -> IO (AfterActionResult a)
rollback = return . Rollback

class (GetKey m) a where
  getKey :: a -> Maybe (DB.Key m)

instance (GetKey r) (DB.Entity r) where
  getKey (k,v) = Just k

doNothing' :: a -> IO a
doNothing' x = return x

class ModelClass m where
  
  afterFind :: DB.Table m -> DB.Entity m -> IO (DB.Entity m)
  afterFind _ = doNothing'
  
  beforeValidation :: DB.Table m -> Maybe (DB.Key m) -> m -> IO (BeforeActionResult m)
  beforeValidation _ _ = go
  
  validate :: DB.Table m -> Maybe (DB.Key m) -> m -> IO (ValidationResult m)
  validate _ _ = valid

  afterValidation :: DB.Table m -> Maybe (DB.Key m) -> m -> IO m
  afterValidation _ _ = doNothing'

  afterValidationFailed :: DB.Table m -> Maybe (DB.Key m) -> m -> IO m
  afterValidationFailed _ _ = doNothing'

  beforeSave :: DB.Table m -> Maybe (DB.Key m) -> m -> IO (BeforeActionResult m)
  beforeSave _ _ = go

  beforeCreate :: DB.Table m -> m -> IO (BeforeActionResult m)
  beforeCreate _ = go

  beforeModify :: DB.Table m -> DB.Entity m -> IO (BeforeActionResult m)
  beforeModify _ = go . snd 
  
  afterCreated :: DB.Table m -> DB.Entity m -> IO m
  afterCreated _ = doNothing' . snd

  afterModified :: DB.Table m -> DB.Entity m -> IO m
  afterModified _ = doNothing' . snd
    
  afterSaved :: DB.Table m -> DB.Entity m -> IO m
  afterSaved _ = doNothing' . snd
  
  afterSaveFailed :: DB.Table m -> DB.Entity m -> IO m
  afterSaveFailed _ = doNothing' . snd

  afterModifyFailed :: DB.Table m -> DB.Entity m -> IO m
  afterModifyFailed _ = doNothing' . snd

  afterCreateFailed :: DB.Table m -> m -> IO m
  afterCreateFailed _ = doNothing'
  
  beforeDestroy :: DB.Table m -> DB.Entity m -> IO (BeforeActionResult m)
  beforeDestroy _ = go . snd
  
  afterDestroyed :: DB.Table m -> DB.Entity m -> IO m
  afterDestroyed _ = doNothing' . snd
  
  afterDestroyFailed :: DB.Table m -> DB.Entity m -> IO m
  afterDestroyFailed _ = doNothing' . snd
  
  afterCommit :: DB.Table m -> DB.Entity m -> IO m
  afterCommit _ = doNothing' . snd
  
  afterRollback :: DB.Table m -> DB.Entity m -> IO m
  afterRollback _ = doNothing' . snd

data (ModelClass m) => SelectQuery m = MkSelectQuery [DB.Filter m] [DB.SelectOpt m]

fireAfterFindME :: (ModelClass m) => DB.Table m -> Maybe (DB.Entity m) -> IO (Maybe (DB.Entity m))
fireAfterFindME _ Nothing  = return Nothing
fireAfterFindME t (Just e) = fireAfterFind t e >>= return . Just 

fireAfterFind :: (ModelClass m) => DB.Table m -> (DB.Entity m) -> IO (DB.Entity m)
fireAfterFind t e = afterFind t e

find :: (ModelClass m) => DB.Table m -> Int64 -> IO (Maybe (DB.Entity m))
find t id = DB.find t id >>= fireAfterFindME t

get :: (ModelClass m) => DB.Table m -> DB.Key m -> IO (Maybe (DB.Entity m))
get t k = DB.get t k >>= fireAfterFindME t

select :: (ModelClass m) => DB.Table m -> SelectQuery m -> IO [(DB.Entity m)]
select t (MkSelectQuery filters opts) = DB.select t filters opts >>= sequence . map (fireAfterFind t)

create :: (ModelClass m) => DB.Table m -> m -> IO (CreateResult m)
create t v = save' t v (\k v -> v) beforeCreate (\x -> Nothing) (\x -> x) (\x -> Nothing) (\t k v -> DB.insert t v) BeforeCreate

modify :: (ModelClass m) => DB.Table m -> (DB.Entity m) -> IO (ModifyResult m)
modify t e = save' t e (\k v -> (k,v)) beforeModify (\(k,v) -> k) (\(k,v) -> v) (\(k,v) -> Just k) (\t k v -> DB.repsert t k v >> return k) BeforeModify

type HookAndOnCancelActionsPair m end = ((m -> IO end) , (m -> IO (HookResult m)))
-- HookResult
(>>==) :: [HookAndOnCancelActionsPair m end] -- next hook actions
       -> (m -> IO end) -- tail action
       -> (m -> IO end) -- function :: initial argument -> last action result
(>>==) cancel_go_pairs tailf = impl' cancel_go_pairs
  where
    -- impl' :: [HookAndOnCancelActionsPair m end] -> m -> IO end
    impl' []                   m = tailf m
    impl' ((onCancel,hook):xs) m = hook  m
      >>= (\res -> case res of
            Validation   (Valid    m2)    -> impl' xs m2
            Validation   (Invalid  m2 rs) -> onCancel m2
            BeforeAction (Go       m2)    -> impl' xs m2
            BeforeAction (Cancel   m2)    -> onCancel m2
            AfterAction  (Commit   m2)    -> impl' xs m2
            AfterAction  (Rollback m2)    -> onCancel m2)
            
(|||) :: (m -> IO end) -> (m -> IO (HookResult m)) -> ((m -> IO end), (m -> IO (HookResult m)))
(|||) f_if_cancel f_if_go = (f_if_cancel, f_if_go)

onCancel l r = r ||| l
do' x = x

{-
  validate :: DB.Table m -> m -> IO (ValidationResult m)
  afterValidation :: DB.Table m -> m -> IO m
  afterValidationFailed :: DB.Table m -> m -> IO m
  beforeSave :: DB.Table m -> m -> IO (BeforeActionResult m)
  beforeCreate :: DB.Table m -> m -> IO (BeforeActionResult m)
  beforeModify :: DB.Table m -> DB.Entity m -> IO (BeforeActionResult (DB.Entity m))
  afterCreated :: DB.Table m -> DB.Entity m -> IO (DB.Entity m)
  afterModified :: DB.Table m -> DB.Entity m -> IO (DB.Entity m)
  afterSaved :: DB.Table m -> DB.Entity m -> IO (DB.Entity m)
  afterSaveFailed :: DB.Table m -> DB.Entity m -> IO (DB.Entity m)
  afterModifyFailed :: DB.Table m -> DB.Entity m -> IO (DB.Entity m)
  afterCreateFailed :: DB.Table m -> DB.Entity m -> IO (DB.Entity m)
  afterCommit :: DB.Table m -> DB.Entity m -> IO (DB.Entity m)
  afterRollback :: DB.Table m -> DB.Entity m -> IO (DB.Entity m)
-}
save' :: (ModelClass m) =>
  DB.Table m -- table
  -> arg     -- record or (Entity record)
  -> (argk -> m -> arg) -- cast k v to arg
  -> (DB.Table m -> arg -> IO (BeforeActionResult m)) -- before hook action
  -> (arg -> argk) -- function to get key from arg
  -> (arg -> m)    -- function to get val from arg
  -> (arg -> Maybe (DB.Key m)) --  function to get Maybe key from arg
  -> (DB.Table m -> argk -> m -> IO (DB.Key m)) -- function to do insert or update
  -> canceled_on_before_create_or_modify -- value on canceled
  -> IO (SaveResult (DB.Entity m) arg arg canceled_on_before_create_or_modify)
save' t e kv_to_arg' beforeCreateOrUpdate' k_getter' v_getter' arg_to_mkey' insert_or_update' beforeStepFlag' = do
  -- data HookResult a = Validation (ValidationResult a) | BeforeAction (BeforeActionResult a) | AfterAction (AfterActionResult a)
--data ValidationResult   a = Valid   a | Invalid [(ColumnName, FailedReason)]
--data BeforeActionResult a = Go      a | Cancel a
--data AfterActionResult  a = Commit  a | Rollback
--data HookResult a = Validation (ValidationResult a) | BeforeAction (BeforeActionResult a) | AfterAction (AfterActionResult a)
    (return ev')
      >>= ([ do' ((return . BeforeAction =<<) . beforeValidation      t (arg_to_mkey' e) ) `onCancel` (return . Canceled OnBeforeValidation   . kv_to_arg' ek')
           , do' ((return . BeforeAction =<<) . beforeSave            t (arg_to_mkey' e) ) `onCancel` (return . Canceled OnBeforeSave         . kv_to_arg' ek')
           , do' ((return . BeforeAction =<<) . beforeCreateOrUpdate' t . kv_to_arg' ek') `onCancel` (return . Canceled (On beforeStepFlag') . kv_to_arg' ek')
           ] >>== doImpl')
  where
    ek' = k_getter' e
    ev' = v_getter' e
    get'         = DB.get t
    doFirst' v' = insert_or_update' t ek' v'
    doImpl'  v' = doFirst' v' >>= get' >>= return . maybe (SaveFailed (kv_to_arg' ek' v') []) SaveSuccess

save :: (ModelClass m) => DB.Table m -> Maybe (DB.Key m) -> m -> IO (Either (CreateResult m) (ModifyResult m))
save t Nothing  v = create t    v  >>= return . Left
save t (Just k) v = modify t (k,v) >>= return . Right

destroy :: (ModelClass m) => DB.Table m -> (DB.Entity m) -> IO (DestroyResult (DB.Entity m))
destroy table e@(key, v) = do
  me <- get' key
  case me of
    Nothing -> return RecordNotFound -- maybe already deleted
    Just _ -> do
      bar <- beforeDestroy table e
      case bar of
        Cancel v2 -> return $ DeleteCanceled (key, v2)
        Go     v2 -> doDelete' v2
  where
    get'    = DB.get    table
    delete' = DB.delete table
    doDelete' v2 = delete' key >> get' key >>= (\x -> (if isNothing x then afterSuccess' else afterFailed') v2)
    afterSuccess' v2 = afterDestroyed     table (key,v2) >>= return . DeleteSuccess . (,) key
    afterFailed'  v2 = afterDestroyFailed table (key,v2) >>= return . DeleteFailed  . (,) key
    
-- res <- select table $ asc ChannelHoge $ where [channelHoge .== 1, channelFoo .== 2] $ where [channelAaa .== 22 , channelName .== "hoge"] $ rec
-- return $ toJSON res
class (ToSelectQuery m) a where
  where_  :: [DB.Filter m]  -> a -> SelectQuery m
  whereOr :: [DB.Filter m]  -> a -> SelectQuery m
  limit   :: Int         -> a -> SelectQuery m
  offset  :: Int         -> a -> SelectQuery m
  asc     :: DB.EntityField m typ -> a -> SelectQuery m
  desc    :: DB.EntityField m typ -> a -> SelectQuery m

instance (ModelClass m) => (ToSelectQuery m) m where
  where_  fs x = MkSelectQuery fs []
  whereOr      = where_
  limit   i  x = MkSelectQuery [] [DB.mkLimit  i]
  offset  i  x = MkSelectQuery [] [DB.mkOffset i]
  asc     f  x = MkSelectQuery [] [DB.mkAsc    f]
  desc    f  x = MkSelectQuery [] [DB.mkDesc   f]
  
instance (ModelClass m) => (ToSelectQuery m) (SelectQuery m) where
  where_  fs (MkSelectQuery xs ys) = MkSelectQuery (xs ++ fs) ys
  whereOr fs (MkSelectQuery xs ys) = MkSelectQuery (fs `DB.or` xs) ys
  limit   i  (MkSelectQuery xs ys) = MkSelectQuery xs (ys ++ [DB.mkLimit  i])
  offset  i  (MkSelectQuery xs ys) = MkSelectQuery xs (ys ++ [DB.mkOffset i])
  asc     f  (MkSelectQuery xs ys) = MkSelectQuery xs (ys ++ [DB.mkAsc    f])
  desc    f  (MkSelectQuery ys zs) = MkSelectQuery ys (zs ++ [DB.mkDesc   f])

{-
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
import Control.Monad.Trans.Resource(MonadResource) -- resourcet
import Control.Monad.Reader(ReaderT) -- mtl
import Control.Monad.Reader.Class(MonadReader) -- mtl
import Data.Acquire(Acquire) -- resourcet
import Data.Conduit(Source) --- conduit
import Data.Int(Int64) -- base
import Helper(OnResultFunc(..),(>>==))
import qualified Database.Persist as P --persistent
import Class.Castable


data SaveType = Modify | Create

toSuccess :: a -> (Bool,a)
toSuccess x = (True, x)

class (PS.PersistEntity entity, PS.ToBackendKey SqlBackend entity, PS.PersistRecordBackend entity SqlBackend) => ActiveRecord entity where
  -- please override if you need
  afterFind, afterFind_ :: entity -> ReaderT SqlBackend IO entity
  afterFind = return
  afterFind_ arg = do
    liftIO $ putStrLn "afterFind"
    afterFind arg
  
  -- please override if you need
  beforeValidation, beforeValidation_ :: (Maybe (PS.Key entity), entity) -> ReaderT SqlBackend IO (Bool, (Maybe (PS.Key entity), entity))
  beforeValidation = return . toSuccess
  beforeValidation_ arg = do
    liftIO $ putStrLn "beforeValidation"
    beforeValidation arg
  
  -- please override if you need
  validate,  afterValidation,  beforeSave,  afterSaved, validate_, afterValidation_, beforeSave_, afterSaved_ :: (Maybe (PS.Key entity), entity) -> ReaderT SqlBackend IO (Bool, (Maybe (PS.Key entity), entity))
  validate = return . toSuccess
  validate_ arg = do
    liftIO $ putStrLn "validate"
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
  afterSaved_ arg@(k,e) = do
    liftIO $ putStrLn "afterSaved"
    -- liftIO $ putStrLn $ show k    
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
  afterCreated_ arg@(k,e) = do
    liftIO $ putStrLn "afterCreate"
    -- liftIO $ putStrLn $ show k
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

  beforeDestroy, afterDestroyed, beforeDestroy_, afterDestroyed_ :: Entity entity -> ReaderT SqlBackend IO (Bool, (Entity entity))
  
  -- please override if you need  
  beforeDestroy = return . toSuccess
  beforeDestroy_ arg = do
    liftIO $ putStrLn "beforeDestroy"
    beforeDestroy arg

  -- please override if you need
  afterDestroyed = return . toSuccess
  afterDestroyed_ arg = do
    liftIO $ putStrLn "afterDestroyed"
    afterDestroyed arg

  -- please override if you need
  afterDestroyFailed, afterDestroyFailed_ :: Entity entity -> ReaderT SqlBackend IO (Entity entity)
  afterDestroyFailed = return
  afterDestroyFailed_ arg = do
    liftIO $ putStrLn "afterDestroyFailed"
    afterDestroyFailed arg

  -- please override if you need
  afterCommit, afterCommit_ :: (Maybe (PS.Key entity), entity) -> ReaderT SqlBackend IO (Bool, (Maybe (PS.Key entity), entity))
  afterCommit = return . toSuccess
  afterCommit_ arg@(k,e) = do
    liftIO $ putStrLn "afterCommit"
    -- liftIO $ putStrLn $ show k
    afterCommit arg

  -- please override if you need
  afterRollback, afterRollback_ :: (Maybe (PS.Key entity), entity) -> ReaderT SqlBackend IO (Maybe (PS.Key entity), entity)
  afterRollback = return
  afterRollback_ arg@(k,e) = do
    liftIO $ putStrLn "afterRollback"
    -- liftIO $ putStrLn $ show k
    afterRollback arg
  
  existOnDb :: PS.Key entity -> ReaderT SqlBackend IO Bool
  existOnDb key = (return . isJust) =<< (PS.liftPersist $ PS.get key)
  
  existOnDbBy :: entity -> ReaderT SqlBackend IO Bool
  existOnDbBy entity= (return . isJust) =<< (PS.liftPersist . PS.getBy) =<< (PS.liftPersist $ PS.onlyUnique entity)
  
  modifyWithoutHooks, modifyWithoutHooks_ :: PS.Key entity -> entity -> ReaderT SqlBackend IO (Bool, (Maybe (PS.Key entity), entity))
  modifyWithoutHooks key entity = PS.replace key entity >> afterSaveWithoutHooks entity key
  modifyWithoutHooks_ key entity = do
    liftIO $ putStrLn "modifyWithoutHooks"
    -- liftIO $ putStrLn $ show key
    modifyWithoutHooks key entity
    
  createWithoutHooks, createWithoutHooks_ :: entity -> ReaderT SqlBackend IO (Bool, (Maybe (PS.Key entity), entity))
  createWithoutHooks entity = do
    key <- PS.insert entity
    afterSaveWithoutHooks_ entity key
  createWithoutHooks_ arg = do
    liftIO $ putStrLn "createWithoutHooks"
    -- liftIO $ putStrLn $ show key
    createWithoutHooks arg
    

  afterSaveWithoutHooks, afterSaveWithoutHooks_ :: entity -> PS.Key entity -> ReaderT SqlBackend IO (Bool, (Maybe (PS.Key entity), entity))
  afterSaveWithoutHooks old_entity key =
    let ifNothing' = (False, (Nothing, old_entity)) in
    findByKey key >>= return . maybe ifNothing' (\(Entity k v) -> (True, (Just k, v)))
    
  afterSaveWithoutHooks_ entity key = do
    liftIO $ putStrLn "afterSaveWithoutHooks"
    afterSaveWithoutHooks entity key

  destroyWithoutHooks, destroyWithoutHooks_ :: Entity entity -> ReaderT SqlBackend IO (Bool, Entity entity)
  destroyWithoutHooks entity = let key = entityKey entity in PS.delete key >> PS.get key >>= return . swap . (,) entity . isNothing
  destroyWithoutHooks_ arg = do
    liftIO $ putStrLn "destroyWithoutHooks"
    destroyWithoutHooks arg

  destroyImpl :: record -> Entity entity -> ReaderT SqlBackend IO (Bool, (Entity entity))
  destroyImpl _ x = beforeDestroy_ x >>== (OnSuccessFunc destroyWithoutHooks_) >>== (OnEitherFunc afterDestroyed_ afterDestroyFailed_)


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
  saveImpl self arg@(mkey, v) = do
    validate_ arg
      >>== (OnEitherFunc  afterValidation_ afterValidationFailed_)
      >>== (OnSuccessFunc beforeSave_)
      >>== (OnSuccessFunc beforeActionCreateOrUpdate')
      >>== (OnSuccessFunc main')
      >>== (OnSuccessFunc afterActionCreatedOrUpdated')
      >>== (OnEitherFunc  afterSaved_ afterSaveFailed_)
      >>== (OnEitherFunc  afterCommit_ afterRollback_)
    where
      key' = fromJust mkey
      beforeActionCreateOrUpdate' =
        case saveType' of
          Modify -> beforeModify_
          Create -> \(mkey, entity) -> beforeCreate_ entity >>= \(b, entity2) -> return (b, (Nothing, entity2))

      main' arg = do
        res@(b,(k,e)) <- (case saveType' of
                Modify -> modifyWithoutHooks_ key'
                Create -> createWithoutHooks_
               ) $ snd arg
        liftIO $ putStrLn "main'"
        liftIO $ putStrLn $ show b
        liftIO $ putStrLn $ show k
        return res

      afterActionCreatedOrUpdated' =
        case saveType' of
          Modify -> afterModified_
          Create -> afterCreated_
          
      saveType' = if isJust mkey then Modify else Create

findByKey :: (ActiveRecord entity) => PS.Key entity -> ReaderT SqlBackend IO (Maybe (Entity entity))
findByKey key = PS.get key >>= maybe (return Nothing) (\x -> afterFind_ x >>= return . Just . Entity key)
    
find :: (Read id, Show id, ActiveRecord entity) => id -> ReaderT SqlBackend IO (Maybe (Entity entity))
find = let ifNothing' = -1 in findByKey . toSqlKey . fromMaybe ifNothing' . readMaybe . show

selectBy :: (ActiveRecord e) => [P.Filter e] -> [P.SelectOpt e] -> ReaderT SqlBackend IO [Entity e]
selectBy filters opts = do
  list <- PS.liftPersist $ P.selectList filters opts
  mapM (\(Entity k v) -> afterFind_ v >>= return . Entity k) list
          
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

data DummyDBConn = MkDummyDBConn

type DBConn = DummyDBConn

data (ActiveRecord2 r) => Key r = MkKey Integer

class ActiveRecord2 r where
  key :: r -> Maybe (Key r)
  key2int :: Key r -> Integer
  isExists2 :: DBConn -> r -> Bool
  isNewRecord2 :: DBConn -> r -> Bool
  isNewRecord2 conn r = not $ isExists2 conn r

  afterFind2 :: (DBConn -> r -> r)
  
  beforeValidation2 :: (DBConn -> r -> Maybe r)
  afterValidation2 :: (DBConn -> r -> r)
  afterValidationFailed2 :: (DBConn -> ValidationResult r -> r)
  
  beforeSave2 :: (DBConn -> r -> Maybe r)
  afterSaved2 :: (DBConn -> SaveResult r -> SaveResult r)
  afterSaveFailed2 :: (DBConn -> SaveResult r -> SaveResult r)
  
  beforeCreate2 :: (DBConn -> r -> Maybe r)
  afterCreated2 :: (DBConn -> SaveResult r -> SaveResult r)
  afterCreateFailed2 :: (DBConn -> SaveResult r -> SaveResult r)
  
  beforeModify2 :: (DBConn -> r -> Maybe r)
  afterModified2 :: (DBConn -> SaveResult r -> SaveResult r)
  afterModifyFailed2 :: (DBConn -> SaveResult r -> SaveResult r)
  
  beforeDestroy2 :: (DBConn -> r -> Maybe r)
  afterDestroyed2 :: (DBConn -> r -> r)
  afterDestroyFailed2 :: (DBConn -> r -> r)
  
  afterCommit2 :: (DBConn -> r -> r)
  afterRollback2 :: (DBConn -> r -> r)

data ErrorPoint = Validation | BeforeCreate | BeforeModify | BeforeSave

data SaveError = MkSaveError {
  errorPoint :: ErrorPoint
  }

data (ActiveRecord2 r) => ValidationResult r = ValidationNoProblem r | ValidationError r
data (ActiveRecord2 r) => SaveResult r       = SaveSuccess r | SaveFailed r SaveError

validate2 :: (ActiveRecord2 r) => DBConn -> r -> ValidationResult r
validate2 conn r = ValidationError r

insert2,update2 :: (ActiveRecord2 r) => DBConn -> r -> SaveResult r
insert2 conn r = SaveFailed r MkSaveError {errorPoint = Validation}
update2 conn r = SaveFailed r MkSaveError {errorPoint = Validation}

doValidate :: (ActiveRecord2 r) => DBConn -> r -> ValidationResult r
doValidate conn r =
  case beforeValidation2 conn r of
    Nothing -> ValidationError r
    Just r2 ->
      let res = validate2 conn r2 in
        case res of
          ValidationError r3 -> ValidationError $ afterValidationFailed2 conn res
          ValidationNoProblem r3 -> ValidationNoProblem $ afterValidation2 conn r3
          
doCreate :: (ActiveRecord2 r) => DBConn -> r -> SaveResult r
doCreate conn r =
  case beforeCreate2 conn r of
    Nothing -> SaveFailed r MkSaveError {errorPoint = BeforeCreate}
    Just r2 ->
      let res = insert2 conn r2 in
        case res of
          SaveFailed  r3 save_err -> afterCreateFailed2 conn res
          SaveSuccess r3 -> afterCreated2 conn res

doModify :: (ActiveRecord2 r) => DBConn -> r -> SaveResult r          
doModify conn r =
  case beforeModify2 conn r of
    Nothing -> SaveFailed r MkSaveError {errorPoint = BeforeCreate}
    Just r2 ->
      let res = update2 conn r2 in
        case res of
          SaveFailed  r3 save_err -> afterModifyFailed2 conn res
          SaveSuccess r3 -> afterModified2 conn res

save2 :: (ActiveRecord2 r) => DBConn -> r -> SaveResult r
-- save2 conn r = SaveFailed MkSaveError {errorPoint = Validation}
save2 conn r =
  case key r of
    Nothing   -> doCreate conn r
    otherwise ->
      let res = doModify conn r in
        case res of
          SaveFailed  r2 save_err -> afterSaveFailed2 conn res
          SaveSuccess r2 -> afterSaved2 conn res          

find2 :: (ActiveRecord2 r) => DBConn -> Key r -> Maybe r
find2 conn key = Nothing

-}
