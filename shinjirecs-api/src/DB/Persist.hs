{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE UndecidableInstances       #-}

module DB.Persist(
  migrate
  ,Reservation(..)
  ,Channel(..)
  ,Program(..)
  ,Session(..)
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
--  ,DB.Persist.selectKeys
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
  ,mkUnique
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
  ,runQuery
--  ,transaction
  ) where
import qualified DB.Config
import qualified Data.Text as Text --text
import Data.Text (Text,pack) -- text
import Database.Persist(PersistField, PersistValue(PersistInt64), checkUnique, count, getBy, get, deleteWhere, deleteBy, selectKeys, selectList, delete, updateWhere, repsert, insertBy, updateGet, insert, PersistRecordBackend, Filter(Filter,FilterOr,FilterAnd), Update(..),PersistFilter(..),(==.),(!=.),(<.),(>.),(<=.),(>=.),(<-.),(/<-.),(||.),PersistEntity(persistUniqueKeys)) -- persistent
import Database.Persist.Types(Entity(Entity), SelectOpt(..)) -- persistent
import Database.Persist.Sql (Connection, ConnectionPool ,runSqlConn , runSqlPool, SqlPersistT, IsSqlBackend, runSqlPersistMPool, runMigration) -- persistentget
import Database.Persist.MySQL (withMySQLConn, createMySQLPool) -- persistent-mysql
import Database.Persist.Sqlite (withSqliteConn, createSqlitePool) -- persistent-sqlite
import Database.Persist.Postgresql (withPostgresqlConn, createPostgresqlPool) -- persistent-postgresql
import Database.Persist.Sql.Types.Internal (SqlBackend)
import Database.Persist.Class (BaseBackend, IsPersistBackend, PersistEntity(..), Key(..), keyToValues, entityIdToJSON,entityIdFromJSON) -- persistent
import Control.Monad.Trans.Resource (runResourceT, ResourceT, MonadBaseControl, MonadResource) -- resourcet
import Control.Monad.Logger (runNoLoggingT, NoLoggingT) -- monad-logger
import Control.Monad.IO.Class(liftIO,MonadIO) -- base
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (ReaderT, runReaderT, MonadReader) -- mtl
import DB.Types(AdapterType(..))
import DB.Config(Config(..),configToMySQLConnectInfo,configToPgSQLConnectionString,migrationFilePath,migrationSessionTableFilePath)
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
import Data.Aeson(ToJSON(toJSON), FromJSON(parseJSON))
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
share [mkPersist sqlSettings, mkMigrate "migrateModels"]
  $(persistFileWith lowerCaseSettings migrationFilePath)

share [mkPersist sqlSettings, mkMigrate "migrateSession"]
  $(persistFileWith lowerCaseSettings migrationSessionTableFilePath)
  
migrate :: DB.Config.Config -> IO ()
-- migrate = runAction (\pool -> run pool $ runMigration migrateAll)
migrate config = do
  pool <- runNoLoggingT $ DB.Persist.createPool config
  run pool $ runMigration migrateModels
  run pool $ runMigration migrateSession

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

mkUnique :: (PersistEntity val) => val -> [Unique val]
mkUnique = persistUniqueKeys
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

type Query a = ReaderT SqlBackend IO a

--runQuery :: (MonadBaseControl IO m, MonadIO m) => Connection__ -> Query a -> a
runQuery :: Connection__ -> Query a -> IO a
runQuery conn act = runSqlPool act conn

--transaction :: (MonadBaseControl IO m, MonadIO m) => Connection__ -> Query m TransactionResult -> m TransactionResult
--transaction conn act = runSqlPool act conn

insertQuery :: (PersistRecordBackend record SqlBackend) => record -> Query (Key record)
insertQuery = Database.Persist.insert

insert :: (PersistRecordBackend record SqlBackend) => Connection__ -> record -> IO (Key record)
insert connpool val = runSqlPool (insertQuery val) connpool

insertByQuery :: (PersistRecordBackend record SqlBackend) => record -> Query (Either (Key record,record) (Key record))
insertByQuery val = do
  res <- Database.Persist.insertBy val
  return $ case res of
    Left (Entity k v) -> Left (k,v)
    Right key         -> Right key  

insertBy :: (PersistRecordBackend record SqlBackend) => Connection__ -> record -> IO (Either (Key record,record) (Key record))
insertBy connpool val = runSqlPool (insertByQuery val) connpool

updateQuery :: (PersistRecordBackend record SqlBackend) => Key record ->  [Update record] -> Query record
updateQuery key updates = Database.Persist.updateGet key updates

update :: (PersistRecordBackend record SqlBackend) => Connection__ -> Key record ->  [Update record] -> IO record
update connpool key updates = runSqlPool (updateQuery key updates) connpool

updateWhereQuery :: (PersistRecordBackend record SqlBackend) => [Filter record] -> [Update record] -> Query ()
updateWhereQuery filters updates = Database.Persist.updateWhere filters updates

--
updateWhere :: (PersistRecordBackend record SqlBackend) => Connection__ -> [Filter record] -> [Update record] -> IO ()
updateWhere connpool filters updates = runSqlPool (Database.Persist.updateWhere filters updates) connpool

repsertQuery :: (PersistRecordBackend record SqlBackend) => Key record -> record -> Query ()
repsertQuery key val = Database.Persist.repsert key val

repsert :: (PersistRecordBackend record SqlBackend) => Connection__ -> Key record -> record -> IO ()
repsert connpool key val = runSqlPool (repsertQuery key val) connpool

deleteQuery :: (PersistRecordBackend record SqlBackend) => Key record -> Query ()
deleteQuery key = Database.Persist.delete key

delete :: (PersistRecordBackend record SqlBackend) => Connection__ -> Key record -> IO ()
delete connpool key = runSqlPool (deleteQuery key) connpool

deleteByQuery :: (PersistRecordBackend record SqlBackend) => Unique record -> Query ()
deleteByQuery unique = Database.Persist.deleteBy unique

deleteBy :: (PersistRecordBackend record SqlBackend) => Connection__ -> Unique record -> IO ()
deleteBy connpool unique = runSqlPool (deleteByQuery unique) connpool

deleteWhereQuery :: (PersistRecordBackend record SqlBackend) => [Filter record] -> Query ()
deleteWhereQuery filters = Database.Persist.deleteWhere filters

deleteWhere :: (PersistRecordBackend record SqlBackend) => Connection__ -> [Filter record] -> IO ()
deleteWhere connpool filters = runSqlPool (deleteWhereQuery filters) connpool

--getQuery :: (MonadIO m, MonadBaseControl IO m, PersistRecordBackend record SqlBackend) => Key record -> Query m (Maybe (Key record, record))
getQuery :: (PersistRecordBackend record SqlBackend) => Key record -> Query (Maybe (Key record, record))
getQuery key = Database.Persist.get key >>= return . (\mrec -> case mrec of
                                                                 Just r  -> Just (key, r)
                                                                 Nothing -> Nothing)

get :: (PersistRecordBackend record SqlBackend) => Connection__ -> Key record -> IO (Maybe (Key record, record))
get connpool key = runSqlPool (getQuery key) connpool

findQuery :: (PersistRecordBackend record SqlBackend) => Int64 -> Query (Maybe (Key record, record))
findQuery id =
  case keyFromValues [PersistInt64 id] of
    Left x -> return Nothing
    Right key -> DB.Persist.getQuery key

find :: (PersistRecordBackend record SqlBackend) => Connection__ -> Int64 -> IO (Maybe (Key record, record))
find connpool id = runSqlPool (findQuery id) connpool

getByQuery :: (PersistRecordBackend record SqlBackend) => Unique record -> Query (Maybe (Key record,record))
getByQuery unique = do
  res <- Database.Persist.getBy unique
  return $ case res of
    Just (Entity k v) -> Just (k,v)
    Nothing -> Nothing

getBy :: (PersistRecordBackend record SqlBackend) => Connection__ -> Unique record -> IO (Maybe (Key record,record))
getBy connpool unique = runSqlPool (getByQuery unique) connpool

selectQuery :: (PersistRecordBackend record SqlBackend) => [Filter record] -> [SelectOpt record] -> Query [(Key record, record)]
selectQuery filters opts = do
  res <- Database.Persist.selectList filters opts
  return $ Prelude.map (\(Entity k v) -> (k,v)) res

select :: (PersistRecordBackend record SqlBackend) => Connection__ -> [Filter record] -> [SelectOpt record] -> IO [(Key record, record)]
select connpool filters opts = runSqlPool (selectQuery filters opts) connpool

--selectKeys :: (PersistEntity record, BaseBackend (BaseBackend SqlBackend) ~ PersistEntityBackend record, MonadReader SqlBackend IO) => Connection__ -> [Filter record] -> [SelectOpt record] -> Source IO (Key record)
--selectKeys connpool filters opts = Database.Persist.selectKeys filters opts

countQuery :: (PersistRecordBackend record SqlBackend) => [Filter record] -> Query Int
countQuery filters = Database.Persist.count filters

count :: (PersistRecordBackend record SqlBackend) => Connection__ -> [Filter record] -> IO Int
count connpool filters = runSqlPool (countQuery filters) connpool

checkUniqueQuery :: (PersistRecordBackend record SqlBackend) => record -> Query (Maybe (Unique record))
checkUniqueQuery record = Database.Persist.checkUnique record

checkUnique :: (PersistRecordBackend record SqlBackend) => Connection__ -> record -> IO (Maybe (Unique record))
checkUnique connpool record = runSqlPool (checkUniqueQuery record) connpool

keyToStrings :: (PersistEntity record) => Key record -> [String]
keyToStrings key = Prelude.map show $ keyToValues key

--  The resulting JSON looks like {"id": 1, "name": ...}.
instance (PersistEntity record, ToJSON record, ToJSON (Key record)) => ToJSON (Entity record) where
  toJSON r = entityIdToJSON r

-- The input JSON looks like {"id": 1, "name": ...}.
instance (PersistEntity record, FromJSON record, FromJSON (Key record)) => FromJSON (Entity record) where
  parseJSON = entityIdFromJSON
