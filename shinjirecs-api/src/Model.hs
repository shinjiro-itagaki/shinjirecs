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
  ,HookAction(..)
  ,(>>==)
  ,(|||)
  ,ifCancel
  ,ifInvalid
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
  
  afterSaveFailed :: DB.Table m -> Maybe (DB.Key m) -> m -> IO m
  afterSaveFailed _ _ = doNothing'

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
create t v = save' t v (\k v -> v) beforeCreate afterCreateFailed (\x -> Nothing) (\x -> x) (\x -> Nothing) (\t k v -> DB.insert t v) BeforeCreate

modify :: (ModelClass m) => DB.Table m -> (DB.Entity m) -> IO (ModifyResult m)
modify t e = save' t e (\k v -> (k,v)) beforeModify afterModifyFailed (\(k,v) -> k) (\(k,v) -> v) (\(k,v) -> Just k) (\t k v -> DB.repsert t k v >> return k) BeforeModify

data HookAction m end = ActionAndCanceled (m -> IO end) (m -> IO (HookResult m)) | DoAlways (m -> IO m)
(>>==) :: [HookAction m end] -- next hook actions
       -> (m -> IO end) -- tail action
       -> (m -> IO end) -- function :: initial argument -> last action result
(>>==) cancel_go_pairs tailf = impl' cancel_go_pairs
  where
    impl' []     m = tailf m
    impl' (x:xs) m = case x of
      DoAlways                     action -> action m >>= impl' xs
      ActionAndCanceled  if_cancel action -> action m
        >>= (\res -> case res of
                Validation   (Valid    m2)    -> impl' xs m2
                Validation   (Invalid  m2 rs) -> if_cancel m2
                BeforeAction (Go       m2)    -> impl' xs m2
                BeforeAction (Cancel   m2)    -> if_cancel m2
                AfterAction  (Commit   m2)    -> impl' xs m2
                AfterAction  (Rollback m2)    -> if_cancel m2)
            
(|||) :: (m -> IO end) -> (m -> IO (HookResult m)) -> HookAction m end
(|||) f_if_cancel f = ActionAndCanceled f_if_cancel f

ifCancel l r = r ||| l
ifInvalid = ifCancel
do' x = x

doAlways :: (a -> IO a) -> HookAction a b
doAlways f = DoAlways f

save' :: (ModelClass m) =>
  DB.Table m -- table
  -> arg     -- record or (Entity record)
  -> (argk -> m -> arg) -- cast k v to arg
  -> (DB.Table m -> arg -> IO (BeforeActionResult m)) -- beforeCreate      or beforeModify
  -> (DB.Table m -> arg -> IO m)                      -- afterCreateFailed or afterModifyFailed
  -> (arg -> argk) -- function to get key from arg
  -> (arg -> m)    -- function to get val from arg
  -> (arg -> Maybe (DB.Key m)) --  function to get Maybe key from arg
  -> (DB.Table m -> argk -> m -> IO (DB.Key m)) -- function to do insert or update
  -> canceled_on_before_create_or_modify -- value on canceled
  -> IO (SaveResult (DB.Entity m) arg arg canceled_on_before_create_or_modify)
save' t e kv_to_arg' beforeCorM' afterCorMFailed' k_getter' v_getter' arg_to_mkey' insert_or_update' beforeStepFlag' = do
    save_result <- (return ev')
      >>= ([ (do' beforeValidation') `ifCancel` (cancelRtn' OnBeforeValidation  )
           , do' (va' . validate              t   mkey') `ifCancel` (\x -> afterValidationFailed t mkey' x >>= cancelRtn' OnValidation)
           , doAlways $ afterValidation       t   mkey'
           , do' (ba' . beforeSave            t   mkey') `ifCancel` (cancelRtn' OnBeforeSave        )
           , do' (ba' . beforeCorM' t .           arg' ) `ifCancel` (cancelRtn' $ On beforeStepFlag')
           ] >>== doImpl')
{-
  afterCommit :: DB.Table m -> DB.Entity m -> IO (DB.Entity m)
  afterRollback :: DB.Table m -> DB.Entity m -> IO (DB.Entity m)
-}                   
    case save_result of
      SaveSuccess e@(k,v) -> (if isNothing mkey' then afterCreated else afterModified) t e
        >>= afterSaved t . (,) k
        >>= return . SaveSuccess . (,) k
      SaveFailed v results -> afterCorMFailed' t v
        >>= afterSaveFailed t mkey'
        >>= (\v -> return $ SaveFailed (arg' v) results)
      Canceled _ _         -> return save_result
  where
    beforeValidation' = ba' . beforeValidation t mkey'
    cancelRtn' on = (return . Canceled on . arg') -- :: BeforeActionStep m -> (v -> IO SaveResult (BeforeActionStep (BeforeModify | BeforeCreate)) (DB.Entity m | m))
    ba' = (return . BeforeAction =<<) -- :: (IO BeforeActionResult -> IO HookResult)
    va' = (return . Validation   =<<) -- :: (IO ValidationResult   -> IO HookResult)
    arg' = kv_to_arg' ek'  -- :: (v -> arg)
    mkey' = arg_to_mkey' e -- :: Maybe argk
    ek' = k_getter' e -- :: argk
    ev' = v_getter' e -- :: arg
    get'        = DB.get t -- :: (DB.Key m -> DB.Enitity m)
    doFirst' v' = insert_or_update' t ek' v' -- :: (m -> IO (DB.Key m))
    doImpl'  v' = doFirst' v' >>= get' >>= return . maybe (SaveFailed (kv_to_arg' ek' v') []) SaveSuccess -- :: (m -> IO SaveResult)

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
