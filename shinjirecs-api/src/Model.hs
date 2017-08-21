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
--  ,HookAction(..)
--  ,(>>==)
--  ,(|||)
--  ,ifCancel
--  ,ifInvalid
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

--data HookResult a = Validation (ValidationResult a) | BeforeAction (BeforeActionResult a) | AfterAction (AfterActionResult a)
--data ValidationResult   a = ValidationResult   a
--data BeforeActionResult a = BeforeActionResult a
--data AfterActionResult  a = AfterActionResult  a

data BeforeActionStep a = OnBeforeValidation | OnValidation | OnBeforeSave | On a

-- data SaveResult s f c on_x rl = SaveSuccess s | SaveFailed f [(ColumnName, FailedReason)] | Canceled (BeforeActionStep on_x) c | Rollbacked rl
data SaveResult v a ba_step = SaveSuccess (DB.Entity v) | SaveFailed a [(ColumnName, FailedReason)] Bool | Canceled (BeforeActionStep ba_step) a | Rollbacked a

data BeforeCreate = BeforeCreate
data BeforeModify = BeforeModify

-- SaveSuccess (DB.Entity a) | SaveFailed a [(ColumnName, FailedReason)] | Canceled (OnValidation | OnBeforeSave | On BeforeCreate) a | Rollbacked
-- type CreateResult a = SaveResult (DB.Entity a)            a             a  BeforeCreate            a
type CreateResult a = SaveResult a            a  BeforeCreate

-- SaveSuccess (DB.Entity a) | SaveFailed (DB.Entity a) [(ColumnName, FailedReason)] | Canceled (OnValidation | OnBeforeSave | On BeforeModify) (DB.Entity a)
-- type ModifyResult a = SaveResult (DB.Entity a) (DB.Entity a) (DB.Entity a) BeforeModify (DB.Entity a)
type ModifyResult a = SaveResult a (DB.Entity a) BeforeModify
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
  
  afterCreated :: DB.Table m -> DB.Entity m -> IO (AfterActionResult m)
  afterCreated _ = commit . snd

  afterModified :: DB.Table m -> DB.Entity m -> IO (AfterActionResult m)
  afterModified _ = commit . snd
    
  afterSaved :: DB.Table m -> DB.Entity m -> IO (AfterActionResult m)
  afterSaved _ = commit . snd
  
  afterSaveFailed :: DB.Table m -> Maybe (DB.Key m) -> m -> IO (AfterActionResult m)
  afterSaveFailed _ _ = rollback

  afterModifyFailed :: DB.Table m -> DB.Entity m -> IO (AfterActionResult m)
  afterModifyFailed _ = rollback . snd

  afterCreateFailed :: DB.Table m -> m -> IO (AfterActionResult m)
  afterCreateFailed _ = rollback
  
  beforeDestroy :: DB.Table m -> DB.Entity m -> IO (BeforeActionResult m)
  beforeDestroy _ = go . snd
  
  afterDestroyed :: DB.Table m -> DB.Entity m -> IO (AfterActionResult m)
  afterDestroyed _ = commit . snd
  
  afterDestroyFailed :: DB.Table m -> DB.Entity m -> IO (AfterActionResult m)
  afterDestroyFailed _ = rollback . snd
  
  afterCommit :: DB.Table m -> Maybe (DB.Key m) -> m -> IO m
  afterCommit _ _ = doNothing'
  
  afterRollback :: DB.Table m -> Maybe (DB.Key m) -> m -> IO m
  afterRollback _ _ = doNothing'

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

--(>>==) :: Either (IO end) (IO next) -> (next -> Either (IO end) (IO next2))  -> Either (IO end) (IO next2)
--(>>==) earg f = impl' earg
--  where
--    impl' (Left  ioarg) = Left ioarg
--    impl' (Right ioarg) = do
--      arg <- ioargf
--      f arg

--(>>==) (Left  ioarg) f = Left ioarg
--(>>==) (Right ioarg) f = ioarg >>= (\arg -> f arg)
--infixl 7 >>==

(.>>==) :: (arg -> IO (Either end next)) -> (next -> IO (Either end next2)) -> (arg -> IO (Either end next2))
(.>>==) f fnext = impl'
  where
    impl' arg = f arg
      >>= (\x -> case x of
              Right next -> fnext next
              Left  y    -> return $ Left y)

infixl 7 .>>==

data FindResult a b = Found a | NotFound b

maybe2FindResult' :: b -> (z -> a) -> Maybe z -> FindResult a b
maybe2FindResult' ifNothing f Nothing  = NotFound ifNothing
maybe2FindResult' ifNothing f (Just x) = Found $ f x

finish :: (arg -> IO (Either end next)) -> (next -> IO end) -> (arg -> IO end)
finish f tailf = impl'
  where
    impl' arg = do
      x <- f arg
      case x of
        Left  y -> return y
        Right y -> tailf  y

infixl 6 `finish`

 
create :: (ModelClass m) => DB.Table m -> m -> IO (CreateResult m)
create t v =
  doBeforeValidation'
  .>>== doValidation'
  .>>== doAfterValidation'
  .>>== doBeforeSave'
  .>>== doBeforeCreate'
  .>>== doCreate'
  `finish` doAllAfterActions' $ v
  where
    toE' x' = x'
    toK'    = Nothing
    step' = BeforeCreate

    doBeforeValidation'            x  = return . Right =<< beforeValidation t toK' x
    doValidation'         (Go      x) = return . Right =<< validate t toK' x
    doValidation'         (Cancel  x) = return $ Left $ Canceled OnBeforeValidation $ toE' x
    
    doAfterValidation'    (Valid   x  ) = return . Right =<< afterValidation t toK' x
    doAfterValidation'    (Invalid x _) = return . Left . Canceled OnValidation . toE' =<< afterValidationFailed t toK' x
    
    doBeforeSave'                  x  = return . Right =<< beforeSave t toK' x
    
    doBeforeCreate'        (Go     x) = return . Right =<< beforeCreate t (toE' x)
    doBeforeCreate'        (Cancel x) = return $ Left $ Canceled OnBeforeSave $ toE' x

    doCreate'              (Go     x) = return . Right . maybe2FindResult' x (\y -> y) =<< DB.get t =<< DB.insert t x
    doCreate'              (Cancel x) = return $ Left  $ Canceled (On step') $ toE' x

    doAllAfterActions' (NotFound    v) = doAfterCreateFailed' v >>= doAfterSaveFailed'
    doAllAfterActions' (Found e@(k,v)) = doAfterCreate'       e >>= doAfterSave' k

    doAfterCreate'            e@(k,v) = afterCreated t e
    doAfterCreateFailed'     v = afterCreateFailed t v
    doAfterSave'       k (Rollback v) = doRollback' k v
    doAfterSave'       k (Commit   v) = afterSaved t (k,v) >>=  doCommitOrRollback' k
    doCommitOrRollback' k (Rollback v) = doRollback' k v
    doCommitOrRollback' k (Commit   v) = doCommit'   k v

    doCommitOrRollbackAfterFailed' (Rollback v) = do
      doRollbackAfterFailed' v
      return $ SaveFailed (toE' v) [] False
    doCommitOrRollbackAfterFailed' (Commit   v) = do
      doCommitAfterFailed'   v
      return $ SaveFailed (toE' v) [] True
      
    doAfterSaveFailed'  (Commit   v) = afterSaveFailed t toK' v >>= doCommitOrRollbackAfterFailed'
    doAfterSaveFailed'  (Rollback v) = doRollbackAfterFailed' v
    
    doCommit'                     k v = afterCommit   t (Just k) v >>= return . SaveSuccess . (,) k
    doRollback'                   k v = afterRollback t (Just k) v >>= return . Rollbacked . toE'
    doCommitAfterFailed'            v = afterCommit   t toK' v >>= (\x -> return $ SaveFailed (toE' x) [] True)
    doRollbackAfterFailed'          v = afterRollback t toK' v >>= (\x -> return $ SaveFailed (toE' x) [] False)

modify :: (ModelClass m) => DB.Table m -> (DB.Entity m) -> IO (ModifyResult m)
modify t e@(k,v) =
  doBeforeValidation'
  .>>== doValidation'
  .>>== doAfterValidation'
  .>>== doBeforeSave'
  .>>== doBeforeModify'
  .>>== doModify'
  `finish` doAllAfterActions' $ v
  where
    toE' x' = (k,x')
    toK'    = Just k
    step' = BeforeModify
--    beforeCreateOrModify' = 
    
    doBeforeValidation'            x  = return . Right =<< beforeValidation t toK' x
    doValidation'         (Go      x) = return . Right =<< validate t toK' x
    doValidation'         (Cancel  x) = return $ Left $ Canceled OnBeforeValidation $ toE' x
    
    doAfterValidation'    (Valid   x  ) = return . Right =<< afterValidation t toK' x
    doAfterValidation'    (Invalid x _) = return . Left . Canceled OnValidation . toE' =<< afterValidationFailed t toK' x
    
    doBeforeSave'                  x  = return . Right =<< beforeSave t toK' x

--    doBeforeCreate'        (Go     x) = return . Right =<< beforeCreate t x
--    doBeforeCreate'        (Cancel x) = return $ Left $ Canceled OnBeforeSave $ toE' x
    
    doBeforeModify'        (Go     x) = return . Right =<< beforeModify t (toE' x)
    doBeforeModify'        (Cancel x) = return $ Left $ Canceled OnBeforeSave $ toE' x

    doModify'              (Go     x) = return . Right . maybe2FindResult' x (\y -> y) =<< (\_ -> DB.get t k) =<< DB.repsert t k x
    doModify'              (Cancel x) = return $ Left  $ Canceled (On step') $ toE' x

    doAllAfterActions' (NotFound    v) = doAfterModifyFailed' v >>= doAfterSaveFailed'
    doAllAfterActions' (Found e@(k,v)) = doAfterModify'       e >>= doAfterSave' k

    doAfterModify'            e@(k,v) = afterModified t e
    doAfterModifyFailed'           v  = afterModifyFailed t (k,v)
    doAfterSave'       k (Rollback v) = doRollback' k v
    doAfterSave'       k (Commit   v) = afterSaved t (k,v) >>=  doCommitOrRollback' k
    doCommitOrRollback' k (Rollback v) = doRollback' k v
    doCommitOrRollback' k (Commit   v) = doCommit'   k v

    doCommitOrRollbackAfterFailed' (Rollback v) = do
      doRollbackAfterFailed' v
      return $ SaveFailed (toE' v) [] False
    doCommitOrRollbackAfterFailed' (Commit   v) = do
      doCommitAfterFailed'   v
      return $ SaveFailed (toE' v) [] True
      
    doAfterSaveFailed'  (Commit   v) = afterSaveFailed t toK' v >>= doCommitOrRollbackAfterFailed'
    doAfterSaveFailed'  (Rollback v) = doRollbackAfterFailed' v
    
    doCommit'                     k v = afterCommit   t (Just k) v >>= return . SaveSuccess . (,) k
    doRollback'                   k v = afterRollback t (Just k) v >>= return . Rollbacked . toE'
    doCommitAfterFailed'            v = afterCommit   t toK' v >>= (\x -> return $ SaveFailed (toE' x) [] True)
    doRollbackAfterFailed'          v = afterRollback t toK' v >>= (\x -> return $ SaveFailed (toE' x) [] False)

-- save' :: (ModelClass m) => DB.Table m -> (DB.Entity m) -> IO (ModifyResult m)
-- SaveResult v a ba_step
{-
save' :: (ModelClass m) => DB.Table m -> arg -> (arg -> Maybe (DB.Key m)) -> ba_step-> IO (SaveResult m arg ba_step)
save' t e@(k,v) =
  doBeforeValidation'
  .>>== doValidation'
  .>>== doAfterValidation'
  .>>== doBeforeSave'
  .>>== doBeforeModify'
  .>>== doModify'
  `finish` doAllAfterActions' $ v
  where
    toE' x' = (k,x')
    toK'    = Just k
    step' = BeforeModify
--    beforeCreateOrModify' = 
    
    doBeforeValidation'            x  = return . Right =<< beforeValidation t toK' x
    doValidation'         (Go      x) = return . Right =<< validate t toK' x
    doValidation'         (Cancel  x) = return $ Left $ Canceled OnBeforeValidation $ toE' x
    
    doAfterValidation'    (Valid   x  ) = return . Right =<< afterValidation t toK' x
    doAfterValidation'    (Invalid x _) = return . Left . Canceled OnValidation . toE' =<< afterValidationFailed t toK' x
    
    doBeforeSave'                  x  = return . Right =<< beforeSave t toK' x

--    doBeforeCreate'        (Go     x) = return . Right =<< beforeCreate t x
--    doBeforeCreate'        (Cancel x) = return $ Left $ Canceled OnBeforeSave $ toE' x
    
    doBeforeModify'        (Go     x) = return . Right =<< beforeModify t (toE' x)
    doBeforeModify'        (Cancel x) = return $ Left $ Canceled OnBeforeSave $ toE' x

    doModify'              (Go     x) = return . Right . maybe2FindResult' x (\y -> y) =<< (\_ -> DB.get t k) =<< DB.repsert t k x
    doModify'              (Cancel x) = return $ Left  $ Canceled (On step') $ toE' x

    doAllAfterActions' (NotFound    v) = doAfterModifyFailed' v >>= doAfterSaveFailed'
    doAllAfterActions' (Found e@(k,v)) = doAfterModify'       e >>= doAfterSave' k

    doAfterModify'            e@(k,v) = afterModified t e
    doAfterModifyFailed'           v  = afterModifyFailed t (k,v)
    doAfterSave'       k (Rollback v) = doRollback' k v
    doAfterSave'       k (Commit   v) = afterSaved t (k,v) >>=  doCommitOrRollback' k
    doCommitOrRollback' k (Rollback v) = doRollback' k v
    doCommitOrRollback' k (Commit   v) = doCommit'   k v

    doCommitOrRollbackAfterFailed' (Rollback v) = do
      doRollbackAfterFailed' v
      return $ SaveFailed (toE' v) [] False
    doCommitOrRollbackAfterFailed' (Commit   v) = do
      doCommitAfterFailed'   v
      return $ SaveFailed (toE' v) [] True
      
    doAfterSaveFailed'  (Commit   v) = afterSaveFailed t toK' v >>= doCommitOrRollbackAfterFailed'
    doAfterSaveFailed'  (Rollback v) = doRollbackAfterFailed' v
    
    doCommit'                     k v = afterCommit   t (Just k) v >>= return . SaveSuccess . (,) k
    doRollback'                   k v = afterRollback t (Just k) v >>= return . Rollbacked . toE'
    doCommitAfterFailed'            v = afterCommit   t toK' v >>= (\x -> return $ SaveFailed (toE' x) [] True)
    doRollbackAfterFailed'          v = afterRollback t toK' v >>= (\x -> return $ SaveFailed (toE' x) [] False)
-}
  
save :: (ModelClass m) => DB.Table m -> Maybe (DB.Key m) -> m -> IO (Either (CreateResult m) (ModifyResult m))
save t Nothing  v = create t    v  >>= return . Left
save t (Just k) v = modify t (k,v) >>= return . Right

destroy :: (ModelClass m) => DB.Table m -> (DB.Entity m) -> IO (DestroyResult (DB.Entity m))
destroy table e@(key, v) = do
  me <- get' key
  case me of
    Nothing -> return RecordNotFound -- maybe already deleted
    Just _ -> delete' key >> get' key >>= afterDelete' v
  where
    get'    = DB.get    table
    delete' = DB.delete table
    afterDelete' v' Nothing  = afterSuccess' v'
    afterDelete' v' (Just _) = afterFailed'  v'
    afterSuccess' e' = afterDestroyed     table (key,e') >>= afterHookCommon' >>= return . DeleteSuccess . (,) key
    afterFailed'  e' = afterDestroyFailed table (key,e') >>= afterHookCommon' >>= return . DeleteFailed  . (,) key
    doCommit'   = return ()
    doRollback' = return ()
    afterHookCommon' (Commit   v') = doCommit'   >> return v'
    afterHookCommon' (Rollback v') = doRollback' >> return v'
    
    
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
