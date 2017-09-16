{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module DB.ORMLinker (
  Connection
  , connect
  , migrate
  , Key
  , ORM.Reservation(..)
--  , Reservation(..)
  , ORM.Channel(..)
--  , Channel(..)
  , ORM.Program(..)
--  , Program(..)
  , ORM.Session(..)
  , Update
  , Unique
  , Filter
  , SelectOpt
  , Entity
  , Table(..)
  , readTable
  , reservationsTable
  , channelsTable
  , programsTable
  , mkAsc, mkDesc, mkOffset, mkLimit, ORM.mkUnique
  , mkAndFilter, mkOrFilter
  , (.==), (.!=), (.<), (.>), (.<=), (.>=), (.||)
  , eq, neq, lt, gt, lte, gte, DB.ORMLinker.or
  , in_, notIn
  , EntityField
  , ORM.runQuery
  , transaction
  , Query
  ) where
import qualified DB.Persist as ORM
import Data.Int(Int64)
import DB.Config
import Control.Monad.Logger (runNoLoggingT)
import Control.Exception(Exception,catch,SomeException,onException,throw,finally)
import Control.Monad.IO.Class(liftIO)
import Data.Map(Map,fromList)
import Data.Text(Text,pack)
import Data.Maybe(isJust)
import qualified Data.Aeson as J
import DB.Types(TransactionRequest(TransactionRequest),TransactionResult(TransactionResult,UnknownError), CommitOrRollback(Commit,Rollback), ActionState(NoProblem, Canceled, Failed))
import Control.Monad.Trans.Resource (MonadBaseControl) -- resourcet
import Control.Monad.IO.Class(MonadIO) -- base
import Data.IORef(newIORef,readIORef,writeIORef)
type Connection = ORM.Connection__

-- connect :: (MonadIO m, MonadBaseControl IO m, MonadLogger m) => Config -> m Connection
connect :: DB.Config.Config -> IO Connection
connect = runNoLoggingT . ORM.connect

migrate :: DB.Config.Config -> IO ()
migrate = ORM.migrate

type Key    = ORM.Key__
type Update = ORM.Update__
type Unique = ORM.Unique__
type Filter = ORM.Filter__
type SelectOpt = ORM.SelectOpt__
mkAsc    = ORM.mkAsc
mkDesc   = ORM.mkDesc
mkOffset = ORM.mkOffsetBy
mkLimit  = ORM.mkLimitTo
mkAndFilter = ORM.mkAndFilter
mkOrFilter = ORM.mkOrFilter
type EntityField = ORM.EntityField__
type Entity record = (Key record,record)
type Reservation = ORM.Reservation
type Channel     = ORM.Channel
type Program     = ORM.Program

(.==) l r = (ORM.==.) l r
eq    l r = l .== r

(.!=) l r = (ORM.!=.) l r
neq   l r = l .!= r

(.<)  l r = (ORM.<.)  l r
lt    l r = l .< r

(.>)  l r = (ORM.>.)  l r
gt    l r = l .> r

(.<=) l r = (ORM.<=.) l r
lte   l r = l (.<=) r

(.>=) l r = (ORM.>=.) l r
gte   l r = l (.>=) r

infix 4 .==, .!=, .<, .>, .<=, .>=, `eq`, `neq`, `lt`, `gt`, `lte`, `gte`

in_    l r = (ORM.<-.)  l r
notIn  l r = (ORM./<-.) l r
infix 4 `in_`, `notIn`

(.||) l r = (ORM.||.) l r
or    l r = l .|| r
infixl 3 .||, `or`

type Query a = ORM.Query a

data PleaseRollback = PleaseRollback deriving Show
instance Exception PleaseRollback

transaction :: Connection -> Query (TransactionRequest a b c) -> IO (TransactionResult a b c)
transaction conn query = do
  ioref <- newIORef Nothing
  ( ORM.runQuery conn $ do
      tres <- query
      case tres of
        TransactionRequest (NoProblem             onSaved    ) -> return $ TransactionResult (NoProblem onSaved)
        TransactionRequest (Canceled (Commit   onNotSaved) on) -> return $ TransactionResult (Canceled (Commit onNotSaved) on)
        TransactionRequest (Canceled (Rollback onNotSaved) on) -> liftIO $ rollback' ioref $ TransactionResult (Canceled (Rollback onNotSaved) on)
        TransactionRequest (Failed   (Commit   onNotSaved)   ) -> return $ TransactionResult (Failed (Commit onNotSaved))
        TransactionRequest (Failed   (Rollback onNotSaved)   ) -> liftIO $ rollback' ioref $ TransactionResult (Failed (Rollback onNotSaved))
    ) `catch` (\ (ex :: SomeException) -> (readIORef ioref   ) >>= return . vToRtn')
  where
    rollback' ref' rtnval' = do
      writeIORef ref' $ Just rtnval'
      throw PleaseRollback
      return $ rtnval'  -- this line is dummy
    vToRtn' Nothing  = UnknownError
    vToRtn' (Just x) = x
  
data Table record = MkTable {
  connection :: Connection
--  ,transaction :: (MonadBaseControl IO m, MonadIO m) => Query m (TransactionRequest a) -> m (TransactionResult a)
  ,insert :: record -> IO (Key record)
  ,update :: Key record -> [Update record] -> IO record
  ,insertBy :: record -> IO (Either (Entity record) (Key record))
  ,updateWhere :: [Filter record] -> [Update record] -> IO ()
  ,repsert     :: Key record -> record -> IO ()
  ,delete      :: Key record -> IO ()
  ,deleteBy    :: Unique record -> IO ()
  ,deleteWhere :: [Filter record] -> IO ()
  ,get         :: Key record -> IO (Maybe (Entity record))
  ,find        :: Int64 -> IO (Maybe (Entity record))
  ,getBy       :: Unique record -> IO (Maybe (Entity record))
  ,select      :: [Filter record] -> [SelectOpt record] -> IO [Entity record]
--  ,selectKeys  :: [Filter record] -> [SelectOpt record] -> Source IO (Key record)
  ,count       :: [Filter record] -> IO Int
  ,checkUnique :: record -> IO (Maybe (Unique record))
  ,keyToStrings :: Key record -> [String]
  ,insertQuery      :: record -> Query (Key record)    
  ,updateQuery      :: Key record -> [Update record] -> Query record    
  ,insertByQuery    :: record -> Query (Either (Entity record) (Key record))    
  ,updateWhereQuery :: [Filter record] -> [Update record] -> Query ()
  ,repsertQuery     :: Key record -> record -> Query ()    
  ,deleteQuery      :: Key record -> Query ()    
  ,deleteByQuery    :: Unique record -> Query ()    
  ,deleteWhereQuery :: [Filter record] -> Query ()    
  ,getQuery         :: Key record -> Query (Maybe (Entity record))    
  ,findQuery        :: Int64 -> Query (Maybe (Entity record))    
  ,getByQuery       :: Unique record -> Query (Maybe (Entity record))    
  ,selectQuery      :: [Filter record] -> [SelectOpt record] -> Query [Entity record]    
  ,countQuery       :: [Filter record] -> Query Int    
  ,checkUniqueQuery :: record -> Query (Maybe (Unique record))
}

-- readTable :: Connection -> Table record
readTable conn = MkTable {
  connection   = conn
--  ,transaction = transaction_     conn
  ,insert      = ORM.insert       conn
  ,update      = ORM.update       conn
  ,insertBy    = ORM.insertBy     conn
  ,updateWhere = ORM.updateWhere  conn
  ,repsert     = ORM.repsert      conn
  ,delete      = ORM.delete       conn
  ,deleteBy    = ORM.deleteBy     conn
  ,deleteWhere = ORM.deleteWhere  conn
  ,get         = ORM.get          conn
  ,find        = ORM.find         conn
  ,getBy       = ORM.getBy        conn
  ,select      = ORM.select       conn
  ,count       = ORM.count        conn
  ,checkUnique = ORM.checkUnique  conn
  ,keyToStrings = ORM.keyToStrings
  ,insertQuery      = ORM.insertQuery
  ,updateQuery      = ORM.updateQuery
  ,insertByQuery    = ORM.insertByQuery
  ,updateWhereQuery = ORM.updateWhereQuery
  ,repsertQuery     = ORM.repsertQuery
  ,deleteQuery      = ORM.deleteQuery
  ,deleteByQuery    = ORM.deleteByQuery
  ,deleteWhereQuery = ORM.deleteWhereQuery
  ,getQuery         = ORM.getQuery
  ,findQuery        = ORM.findQuery
  ,getByQuery       = ORM.getByQuery
  ,selectQuery      = ORM.selectQuery
  ,countQuery       = ORM.countQuery
  ,checkUniqueQuery = ORM.checkUniqueQuery
  }

idToJSON :: [String] -> J.Value
idToJSON []     = J.Null
idToJSON (x:[]) = J.toJSON x
idToJSON xs     = J.toJSON xs

reservationsTable conn = readTable conn :: Table ORM.Reservation
channelsTable     conn = readTable conn :: Table ORM.Channel
programsTable     conn = readTable conn :: Table ORM.Program
