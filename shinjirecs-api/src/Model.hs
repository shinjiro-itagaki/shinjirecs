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
  ,HookActionStep(..)
  ,SaveResult(..)
  ,Create(..)
  ,Modify(..)
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
  ,save
  ,destroy
  ) where
import qualified DB
import Data.Int(Int64)
import Data.Maybe(isNothing)
import Control.Monad(sequence)
import Control.Monad.IO.Class(liftIO)
import DB.Types(TransactionRequest(TransactionRequest),TransactionResult(TransactionResult,UnknownError), CommitOrRollback(Commit,Rollback), ActionState(NoProblem, Canceled, Failed))
import Data.Aeson(ToJSON,FromJSON)

type ColumnName = String
data FailedReason = TooLarge | TooSmall | NotNull | ReferenceNotFound | Others

data ValidationResult   a = Valid   a | Invalid a [(ColumnName, FailedReason)]
data BeforeActionResult a = Go      a | Cancel Bool a -- commit or not
type AfterActionResult a = CommitOrRollback a a
data HookActionStep a = OnBeforeValidation | OnValidation | OnBeforeSave | OnBefore a | OnAfter a | OnAfterSave 

data SaveResult v a pos = SaveSuccess (DB.Entity v) | SaveFailed a [(ColumnName, FailedReason)] Bool | SaveCanceled (HookActionStep pos) a | Rollbacked a

data Create = Create
data Modify = Modify
type CreateResult a = SaveResult a            a  Create
type ModifyResult a = SaveResult a (DB.Entity a) Modify
data DestroyResult a = DeleteSuccess a | RecordNotFound | DeleteFailed a | DeleteCanceled a

valid :: a -> IO (ValidationResult a)
valid = return . Valid

invalid :: a -> [(ColumnName, FailedReason)] -> IO (ValidationResult a)
invalid x ys = return $ Invalid x ys

go :: a -> IO (BeforeActionResult a)
go = return . Go

cancel :: a -> IO (BeforeActionResult a)
cancel = return . Cancel False

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

class (FromJSON m, ToJSON m, ToJSON (DB.Entity m)) => ModelClass m where
  
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

(.>>==) :: (arg -> IO (Either end next)) -> (next -> DB.Query (Either end next2)) -> (arg -> DB.Query (Either end next2))
(.>>==) f fnext = impl'
  where
    impl' arg = (liftIO $ f arg)
      >>= (\x -> case x of
              Right next -> fnext next
              Left  y    -> return $ Left y)

infixr 7 .>>==

data FindResult a b = Found a | NotFound b

maybe2FindResult' :: b -> (z -> a) -> Maybe z -> FindResult a b
maybe2FindResult' ifNothing f Nothing  = NotFound ifNothing
maybe2FindResult' ifNothing f (Just x) = Found $ f x

finish :: (arg -> DB.Query (Either end next)) -> (next -> IO end) -> (arg -> DB.Query end)
finish f tailf = impl'
  where
    impl' arg = do
      x <- f arg
      liftIO $ case x of
        Left  y -> return y
        Right y -> tailf  y

infixl 6 `finish`

 
create :: (ModelClass m) => DB.Table m -> m -> IO (CreateResult m)
create t arg = do
  save' t arg actionImpl' toArg' toK' action' beforeAction' afterAction' afterActionFailed' getV'  
  where
    actionImpl' x = DB.insertQuery t x >>= DB.getQuery t
    toArg' x = x
    toK'    = Nothing
    getV'   x = x
    argv'   = getV' arg
    action' = Create
    beforeAction' = beforeCreate
    afterAction'  = afterCreated
    afterActionFailed' = afterCreateFailed    

modify :: (ModelClass m) => DB.Table m -> (DB.Entity m) -> IO (ModifyResult m)
modify t arg@(k,v) = do
  save' t arg actionImpl' toArg' toK' action' beforeAction' afterAction' afterActionFailed' getV'
  where
    actionImpl' x = do
      DB.repsertQuery t k x
      DB.getQuery t k
    toArg' x = (k,x)
    getV'   = snd
    argv'   = getV' arg
    toK'    = Just k
    action' = Modify
    beforeAction' = beforeModify
    afterAction'  = afterModified
    afterActionFailed' = afterModifyFailed

save' :: (ModelClass m) => DB.Table m -> arg -> (m -> DB.Query (Maybe (DB.Entity m))) -> (m -> arg) -> Maybe (DB.Key m) -> typ -> (DB.Table m -> arg -> IO (BeforeActionResult m)) -> (DB.Table m -> DB.Entity m -> IO (AfterActionResult m)) -> (DB.Table m -> arg -> IO (AfterActionResult m)) -> (arg -> m) ->  IO (SaveResult m arg typ)
save' t arg actionImpl' toArg' toK' action' beforeAction' afterAction' afterActionFailed' getV' = do
  res <- DB.transaction (DB.connection t) impl'
  case res of
    TransactionResult (NoProblem         (k',v')) -> afterCommit   t (Just k') v' >>= return . SaveSuccess . (,) k'
    TransactionResult (Canceled (Commit   v') on) -> afterCommit   t toK' v' >>= return . SaveCanceled on . toArg'
    TransactionResult (Canceled (Rollback v') on) -> afterRollback t toK' v' >>= return . SaveCanceled on . toArg'
    TransactionResult (Failed   (Commit   v')   ) -> afterCommit   t toK' v' >>= return . (\x -> SaveFailed  (toArg' x) [] True )
    TransactionResult (Failed   (Rollback v')   ) -> afterRollback t toK' v' >>= return . (\x -> SaveFailed  (toArg' x) [] False)
    UnknownError                                  -> afterRollback t toK' argv'  >>= return . (\x -> SaveFailed  (toArg' x) [] False)
  where
    argv' = getV' arg
    {- 
    actionImpl' x = do
      DB.repsert t k x
      DB.get t k
    toArg' x = (k,x)
    toK'    = Just k
    action' = Modify
    beforeAction' = beforeModify
    afterAction'  = afterModified
    afterActionFailed' = afterModifyFailed
-}
    impl' = doBeforeValidation'
      .>>== doValidation'
      .>>== doAfterValidation'
      .>>== doBeforeSave'
      .>>== doBeforeAction'
      .>>== doAction'
      `finish` doAllAfterActions' $ getV' arg

    cancel2Req' True  x step = return $ Left $ TransactionRequest (Canceled (Commit   x) step)
    cancel2Req' False x step = return $ Left $ TransactionRequest (Canceled (Rollback x) step)

    doBeforeValidation'            x  = return . Right =<< beforeValidation t toK' x
    doValidation'         (Go      x) = return . Right =<< validate t toK' x
    doValidation'   (Cancel commit x) = cancel2Req' commit x OnBeforeValidation
    
    doAfterValidation'    (Valid   x  ) = return . Right =<< afterValidation t toK' x
    doAfterValidation'    (Invalid x _) = afterValidationFailed t toK' x >>= (\x2 -> return . Left . TransactionRequest $ Canceled (Rollback x2) OnValidation)
    
    doBeforeSave'                  x  = return . Right =<< beforeSave t toK' x
    
    doBeforeAction'        (Go     x) = return . Right =<< beforeAction' t (toArg' x)
    doBeforeAction' (Cancel commit x) = cancel2Req' commit x OnBeforeSave

    doAction'              (Go     x) = do
      return . Right . maybe2FindResult' x (\y -> y) =<< actionImpl' x
    doAction'       (Cancel commit x) = cancel2Req' commit x (OnBefore action')

    doAllAfterActions' (NotFound    v) = doAfterActionFailed' v >>= doAfterSaveFailed'
    doAllAfterActions' (Found e@(k,v)) = doAfterAction'       e >>= doAfterSave' k

    doAfterAction'             e@(k,v) = afterAction' t e
    doAfterActionFailed'            v  = afterActionFailed' t (toArg' v)
    doAfterSave'        k (Rollback v) = doRollback' k v (OnAfter action')
    doAfterSave'        k (Commit   v) = afterSaved t (k,v) >>=  doCommitOrRollback' k OnAfterSave
    doCommitOrRollback' k on (Rollback v) = doRollback' k v on
    doCommitOrRollback' k on (Commit   v) = doCommit'   k v

    doCommitOrRollbackAfterFailed' (Rollback v) = do
      doRollbackAfterFailed' v
    doCommitOrRollbackAfterFailed' (Commit   v) = do
      doCommitAfterFailed'   v
      
    doAfterSaveFailed'  (Commit   v) = afterSaveFailed t toK' v >>= doCommitOrRollbackAfterFailed'
    doAfterSaveFailed'  (Rollback v) = doRollbackAfterFailed' v

    doCommit'                     k v = return $ TransactionRequest $ NoProblem (k,v)
    doRollback'                k v on = return $ TransactionRequest $ Canceled (Rollback v) on
    doCommitAfterFailed'            v = return $ TransactionRequest $ Failed $ Commit   v
    doRollbackAfterFailed'          v = return $ TransactionRequest $ Failed $ Rollback v

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
