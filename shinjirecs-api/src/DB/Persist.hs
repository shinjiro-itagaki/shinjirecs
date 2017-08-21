{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ExistentialQuantification  #-}

module DB.Persist(
  migrate
  ,Reservation(..)
  ,Channel(..)
  ,Program(..)
  ,run
  ,createPool
  ,connect
  ,DB.Persist.insert
  ,insertQuery
  ,DB.Persist.insertBy
  ,insertByQuery
  ,DB.Persist.update
  ,updateQuery
  ,DB.Persist.updateWhere
  ,updateWhereQuery
  ,DB.Persist.repsert
  ,repsertQuery
  ,DB.Persist.delete
  ,deleteQuery
  ,DB.Persist.deleteBy
  ,deleteByQuery
  ,DB.Persist.deleteWhere
  ,deleteWhereQuery
  ,DB.Persist.get
  ,getQuery
  ,DB.Persist.getBy
  ,getByQuery
  ,DB.Persist.find
  ,findQuery
  ,DB.Persist.select
  ,selectQuery
  ,DB.Persist.selectKeys
  ,DB.Persist.count
  ,countQuery
  ,DB.Persist.checkUnique
  ,checkUniqueQuery
  ,Connection__
  ,Key__
  ,Update__
  ,Unique__
  ,Filter__
  ,SelectOpt__
  ,EntityField__
  ,mkAsc
  ,mkDesc
  ,mkOffsetBy
  ,mkLimitTo
  ,(==.)
  ,(!=.)
  ,(<.)
  ,(>.)
  ,(<=.)
  ,(>=.)
  ,(<-.)
  ,(/<-.)
  ,(||.)
  ,mkAndFilter
  ,mkOrFilter
  ,keyToStrings
  ,Query
  ) where
import qualified DB.Config
import qualified Data.Text as Text --text
import Data.Text (Text,pack) -- text
import Database.Persist(PersistField, PersistValue(PersistInt64), checkUnique, count, getBy, get, deleteWhere, deleteBy, selectKeys, selectList, delete, updateWhere, repsert, insertBy, updateGet, insert, PersistRecordBackend, Filter(Filter,FilterOr,FilterAnd), Update(..),PersistFilter(..),(==.),(!=.),(<.),(>.),(<=.),(>=.),(<-.),(/<-.),(||.)) -- persistent
import Database.Persist.Types(Entity(Entity), SelectOpt(..)) -- persistent
import Database.Persist.Sql (Connection, ConnectionPool ,runSqlConn , runSqlPool, SqlPersistT, IsSqlBackend, runSqlPersistMPool, runMigration) -- persistentget
import Database.Persist.MySQL (withMySQLConn, createMySQLPool) -- persistent-mysql
import Database.Persist.Sqlite (withSqliteConn, createSqlitePool) -- persistent-sqlite
import Database.Persist.Postgresql (withPostgresqlConn, createPostgresqlPool) -- persistent-postgresql
import Database.Persist.Sql.Types.Internal (SqlBackend)
import Database.Persist.Class (BaseBackend, IsPersistBackend, PersistEntity(..), Key(..), keyToValues) -- persistent
import Control.Monad.Trans.Resource (runResourceT, ResourceT, MonadBaseControl, MonadResource) -- resourcet
import Control.Monad.Logger (runNoLoggingT, NoLoggingT) -- monad-logger
import Control.Monad.IO.Class(liftIO,MonadIO) -- base
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (ReaderT, runReaderT, MonadReader) -- mtl
import DB.Types(AdapterType(..))
import DB.Config(Config(..),configToMySQLConnectInfo,configToPgSQLConnectionString,migrationFilePath)
import Config.Env(Env(..))
import Database.Persist.TH (share, mkMigrate, mkPersist, sqlSettings, persistFileWith, derivePersistField) -- persistent-template

import Database.Persist.Quasi (lowerCaseSettings, PersistSettings) -- persistent
import DB.Status(ReservationState(..))
import DB.Types(ChannelType(..))
import Data.Time -- time
import Data.ByteString -- bytestring
import Data.Word -- base
import Data.Text (Text,pack) -- text
import Data.Int(Int64)
import qualified Data.Text as Text --text
-- import DB.Class(Record(..))
import Data.Conduit(Source)
import DB.Status(ReservationState(..))
import DB.Types(ChannelType(..))
import qualified Data.Aeson as J

-- type Sql = SqlPersistT (ResourceT (NoLoggingT IO))

getSQLActionRunner' :: (BaseBackend backend ~ SqlBackend, IsPersistBackend backend, MonadBaseControl IO m1, MonadBaseControl IO m) =>
  ((backend -> m1 a1) -> ResourceT (NoLoggingT m) a)
  -> ReaderT backend m1 a1
  -> m a
getSQLActionRunner' func = runNoLoggingT . runResourceT . func . runSqlConn

createPool,connect :: (MonadIO m, MonadBaseControl IO m, MonadLogger m) => Config -> m ConnectionPool
-- createPool,connect :: (MonadIO m, MonadBaseControl IO m) => Config -> m ConnectionPool
createPool config = 
  case adapter config of
    MySQL      -> createMySQLPool      (configToMySQLConnectInfo      config) pool'
    PostgreSQL -> createPostgresqlPool (configToPgSQLConnectionString config) pool'
    SQLite3    -> createSqlitePool     (Data.Text.pack $ database     config) pool'
  where
    pool' = pool config :: Int

connect = createPool

-- connect :: Config -> IO (Sql a -> IO a)
-- connect config = 
--   return $ getSQLActionRunner' $
--   case adapter config of
--     MySQL      -> withMySQLConn      $ configToMySQLConnectInfo      config
--     PostgreSQL -> withPostgresqlConn $ configToPgSQLConnectionString config
--     SQLite3    -> withSqliteConn     $ Data.Text.pack $ database     config

run :: MonadIO m => ConnectionPool -> SqlPersistT IO a -> m a
run p action = liftIO $ runSqlPool action p


-- make following datas
-- Channel
-- Reservation
-- Program
-- ... and others
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
  $(persistFileWith lowerCaseSettings migrationFilePath)

migrate :: DB.Config.Config -> IO ()
-- migrate = runAction (\pool -> run pool $ runMigration migrateAll)
migrate config = (runNoLoggingT $ DB.Persist.createPool config) >>= (\pool -> run pool $ runMigration migrateAll)

type Connection__ = ConnectionPool
type Key__ = Key
type Update__ = Update
type Unique__ = Unique
type Filter__ = Filter
type SelectOpt__ = SelectOpt
type EntityField__ = EntityField

mkAsc      = Asc
mkDesc     = Desc
mkOffsetBy = OffsetBy
mkLimitTo  = LimitTo
--mkFilter :: forall v typ. PersistField typ => EntityField v typ -> typ -> Filter v
--mkFilter   = Filter

--type FilterOperatorType1 = ((forall v typ.  PersistField typ) => EntityField v typ ->  typ  -> Filter v)
--type FilterOperatorTypeN = ((forall v typ.  PersistField typ) => EntityField v typ -> [typ] -> Filter v)

--(|==|),(|!=|),(|<|),(|>|),(|<=|),(|>=|) :: (forall v typ.  PersistField typ) => EntityField v typ ->  typ  -> Filter v -- FilterOperatorType1
--in_, notIn :: (forall v typ.  PersistField typ) => EntityField v typ -> [typ] -> Filter v -- FilterOperatorTypeN
-- (|==|) = (==.)
-- (|!=|) = (!=.)
-- (|<|)  = (<.)
-- (|>|)  = (>.)
-- (|<=|) = (<=.)
-- (|>=|) = (>=.)
-- in_    = (<-.)
-- notIn  = (/<-.)
  
mkAndFilter = FilterAnd
mkOrFilter = FilterOr

{-
instance Record Reservation where
  new = Reservation {
    reservationChannelId = fromPersistValue
    ,reservationStartTime 
    ,reservationDuration = 1800
    ,reservationTitle = ""
    ,reservationDescription = ""
    ,reservationNext = 0
    ,reservationName = ""
    ,reservationCounter = 0
    ,reservationKeta = 2
    ,reservationVideoFileNameFormat = ""
    ,reservationXwday = 0
    ,reservationState = Waiting
    }
  
instance Record Program where
  new = Program {
    programChannelId
    ,programStartTime
    ,programDuration = 1800
    ,programTitle = ""
    ,programDescription = ""
    ,programName = ""
    }
  
instance Record Channel where
  new = Channel {
    channelNumber = "" 
    ,channelType = GR
    ,channelDisplayName = ""
    ,channelOrder = 0
    ,channelEnable = True
    }

-}

data TransactionResult = Commit | Rollback
type Query m a = ReaderT SqlBackend m a

--  runSqlPool :: (MonadBaseControl IO m, IsSqlBackend backend) => ReaderT backend m a -> Pool backend -> m a
-- get :: (MonadIO m, PersistRecordBackend record backend) => Key record -> ReaderT backend m (Maybe record)
transaction :: (MonadBaseControl IO m, MonadIO m) => Connection__ -> ReaderT SqlBackend m TransactionResult -> m TransactionResult
transaction conn act = runSqlPool act conn

insertQuery :: (MonadIO m, MonadBaseControl IO m, PersistRecordBackend record SqlBackend) => record -> Query m (Key record)
insertQuery = Database.Persist.insert

insert :: (MonadIO m, MonadBaseControl IO m, PersistRecordBackend record SqlBackend) => Connection__ -> record -> m (Key record)
insert connpool val = runSqlPool (insertQuery val) connpool

insertByQuery :: (MonadIO m, MonadBaseControl IO m, PersistRecordBackend record SqlBackend) => record -> Query m (Either (Key record,record) (Key record))
insertByQuery val = do
  res <- Database.Persist.insertBy val
  return $ case res of
    Left (Entity k v) -> Left (k,v)
    Right key         -> Right key  

insertBy :: (MonadIO m, MonadBaseControl IO m, PersistRecordBackend record SqlBackend) => Connection__ -> record -> m (Either (Key record,record) (Key record))
insertBy connpool val = runSqlPool (insertByQuery val) connpool

updateQuery :: (MonadIO m, MonadBaseControl IO m, PersistRecordBackend record SqlBackend) => Key record ->  [Update record] -> Query m record
updateQuery key updates = Database.Persist.updateGet key updates

update :: (MonadIO m, MonadBaseControl IO m, PersistRecordBackend record SqlBackend) => Connection__ -> Key record ->  [Update record] -> m record
update connpool key updates = runSqlPool (updateQuery key updates) connpool

updateWhereQuery :: (MonadIO m, MonadBaseControl IO m, PersistRecordBackend record SqlBackend) => [Filter record] -> [Update record] -> Query m ()
updateWhereQuery filters updates = Database.Persist.updateWhere filters updates

--
updateWhere :: (MonadIO m, MonadBaseControl IO m, PersistRecordBackend record SqlBackend) => Connection__ -> [Filter record] -> [Update record] -> m ()
updateWhere connpool filters updates = runSqlPool (Database.Persist.updateWhere filters updates) connpool

repsertQuery :: (MonadIO m, MonadBaseControl IO m, PersistRecordBackend record SqlBackend) => Key record -> record -> Query m ()
repsertQuery key val = Database.Persist.repsert key val

repsert :: (MonadIO m, MonadBaseControl IO m, PersistRecordBackend record SqlBackend) => Connection__ -> Key record -> record -> m ()
repsert connpool key val = runSqlPool (repsertQuery key val) connpool

deleteQuery :: (MonadIO m, MonadBaseControl IO m, PersistRecordBackend record SqlBackend) => Key record -> Query m ()
deleteQuery key = Database.Persist.delete key

delete :: (MonadIO m, MonadBaseControl IO m, PersistRecordBackend record SqlBackend) => Connection__ -> Key record -> m ()
delete connpool key = runSqlPool (deleteQuery key) connpool

deleteByQuery :: (MonadIO m, MonadBaseControl IO m, PersistRecordBackend record SqlBackend) => Unique record -> Query m ()
deleteByQuery unique = Database.Persist.deleteBy unique

deleteBy :: (MonadIO m, MonadBaseControl IO m, PersistRecordBackend record SqlBackend) => Connection__ -> Unique record -> m ()
deleteBy connpool unique = runSqlPool (deleteByQuery unique) connpool

deleteWhereQuery :: (MonadIO m, MonadBaseControl IO m, PersistRecordBackend record SqlBackend) => [Filter record] -> Query m ()
deleteWhereQuery filters = Database.Persist.deleteWhere filters

deleteWhere :: (MonadIO m, MonadBaseControl IO m, PersistRecordBackend record SqlBackend) => Connection__ -> [Filter record] -> m ()
deleteWhere connpool filters = runSqlPool (deleteWhereQuery filters) connpool

getQuery :: (MonadIO m, MonadBaseControl IO m, PersistRecordBackend record SqlBackend) => Key record -> Query m (Maybe (Key record, record))
getQuery key = Database.Persist.get key >>= return . (\mrec -> case mrec of
                                                                 Just r  -> Just (key, r)
                                                                 Nothing -> Nothing)

get :: (MonadIO m, MonadBaseControl IO m, PersistRecordBackend record SqlBackend) => Connection__ -> Key record -> m (Maybe (Key record, record))
get connpool key = runSqlPool (getQuery key) connpool

findQuery :: (MonadIO m, MonadBaseControl IO m, PersistRecordBackend record SqlBackend) => Int64 -> Query m (Maybe (Key record, record))
findQuery id =
  case keyFromValues [PersistInt64 id] of
    Left x -> return Nothing
    Right key -> DB.Persist.getQuery key

find :: (MonadIO m, MonadBaseControl IO m, PersistRecordBackend record SqlBackend) => Connection__ -> Int64 -> m (Maybe (Key record, record))
find connpool id = runSqlPool (findQuery id) connpool

getByQuery :: (MonadIO m, MonadBaseControl IO m, PersistRecordBackend record SqlBackend) => Unique record -> Query m (Maybe (Key record,record))
getByQuery unique = do
  res <- Database.Persist.getBy unique
  return $ case res of
    Just (Entity k v) -> Just (k,v)
    Nothing -> Nothing

getBy :: (MonadIO m, MonadBaseControl IO m, PersistRecordBackend record SqlBackend) => Connection__ -> Unique record -> m (Maybe (Key record,record))
getBy connpool unique = runSqlPool (getByQuery unique) connpool

selectQuery :: (MonadIO m, MonadBaseControl IO m, PersistRecordBackend record SqlBackend) => [Filter record] -> [SelectOpt record] -> Query m [(Key record, record)]
selectQuery filters opts = do
  res <- Database.Persist.selectList filters opts
  return $ Prelude.map (\(Entity k v) -> (k,v)) res

select :: (MonadIO m, MonadBaseControl IO m, PersistRecordBackend record SqlBackend) => Connection__ -> [Filter record] -> [SelectOpt record] -> m [(Key record, record)]
select connpool filters opts = runSqlPool (selectQuery filters opts) connpool

selectKeys :: (MonadResource m, MonadBaseControl IO m, PersistEntity record, BaseBackend (BaseBackend SqlBackend) ~ PersistEntityBackend record, MonadReader SqlBackend m) => Connection__ -> [Filter record] -> [SelectOpt record] -> Source m (Key record)
selectKeys connpool filters opts = Database.Persist.selectKeys filters opts

countQuery :: (MonadIO m, MonadBaseControl IO m, PersistRecordBackend record SqlBackend) => [Filter record] -> Query m Int
countQuery filters = Database.Persist.count filters

count :: (MonadIO m, MonadBaseControl IO m, PersistRecordBackend record SqlBackend) => Connection__ -> [Filter record] -> m Int
count connpool filters = runSqlPool (countQuery filters) connpool

checkUniqueQuery :: (MonadIO m, MonadBaseControl IO m, PersistRecordBackend record SqlBackend) => record -> Query m (Maybe (Unique record))
checkUniqueQuery record = Database.Persist.checkUnique record

checkUnique :: (MonadIO m, MonadBaseControl IO m, PersistRecordBackend record SqlBackend) => Connection__ -> record -> m (Maybe (Unique record))
checkUnique connpool record = runSqlPool (checkUniqueQuery record) connpool

keyToStrings :: (PersistEntity record) => Key record -> [String]
keyToStrings key = Prelude.map show $ keyToValues key
