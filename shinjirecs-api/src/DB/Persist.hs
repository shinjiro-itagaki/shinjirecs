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
  ,DB.Persist.insertBy
  ,DB.Persist.update
  ,DB.Persist.updateWhere
  ,DB.Persist.repsert
  ,DB.Persist.delete
  ,DB.Persist.deleteBy
  ,DB.Persist.deleteWhere
  ,DB.Persist.get
  ,DB.Persist.getBy
  ,DB.Persist.find
  ,DB.Persist.select
  ,DB.Persist.selectKeys
  ,DB.Persist.count
  ,DB.Persist.checkUnique
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
  ) where
import qualified DB.Config
import qualified Data.Text as Text --text
import Data.Text (Text,pack) -- text
import Database.Persist(PersistField, PersistValue(PersistInt64), checkUnique, count, getBy, get, deleteWhere, deleteBy, selectKeys, selectList, delete, updateWhere, repsert, insertBy, updateGet, insert, PersistRecordBackend, Filter(Filter,FilterOr,FilterAnd), Update(..),PersistFilter(..),(==.),(!=.),(<.),(>.),(<=.),(>=.),(<-.),(/<-.),(||.)) -- persistent
import Database.Persist.Types(Entity(Entity), SelectOpt(..)) -- persistent
import Database.Persist.Sql (Connection, ConnectionPool ,runSqlConn , runSqlPool, SqlPersistT, IsSqlBackend, runSqlPersistMPool, runMigration) -- persistent
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

insert :: (MonadIO m, MonadBaseControl IO m, PersistRecordBackend record SqlBackend) => Connection__ -> record -> m (Key record)
insert connpool val = runSqlPool (Database.Persist.insert val) connpool

insertBy :: (MonadIO m, MonadBaseControl IO m, PersistRecordBackend record SqlBackend) => Connection__ -> record -> m (Either (Key record,record) (Key record))
insertBy connpool val = do
  res <- runSqlPool (Database.Persist.insertBy val) connpool
  return $ case res of
    Left (Entity k v) -> Left (k,v)
    Right key         -> Right key

update :: (MonadIO m, MonadBaseControl IO m, PersistRecordBackend record SqlBackend) => Connection__ -> Key record ->  [Update record] -> m record
update connpool key updates = runSqlPool (Database.Persist.updateGet key updates) connpool

updateWhere :: (MonadIO m, MonadBaseControl IO m, PersistRecordBackend record SqlBackend) => Connection__ -> [Filter record] -> [Update record] -> m ()
updateWhere connpool filters updates = runSqlPool (Database.Persist.updateWhere filters updates) connpool

repsert :: (MonadIO m, MonadBaseControl IO m, PersistRecordBackend record SqlBackend) => Connection__ -> Key record -> record -> m ()
repsert connpool key val = runSqlPool (Database.Persist.repsert key val) connpool

delete :: (MonadIO m, MonadBaseControl IO m, PersistRecordBackend record SqlBackend) => Connection__ -> Key record -> m ()
delete connpool key = runSqlPool (Database.Persist.delete key) connpool

deleteBy :: (MonadIO m, MonadBaseControl IO m, PersistRecordBackend record SqlBackend) => Connection__ -> Unique record -> m ()
deleteBy connpool unique = runSqlPool (Database.Persist.deleteBy unique) connpool

deleteWhere :: (MonadIO m, MonadBaseControl IO m, PersistRecordBackend record SqlBackend) => Connection__ -> [Filter record] -> m ()
deleteWhere connpool filters = runSqlPool (Database.Persist.deleteWhere filters) connpool

get :: (MonadIO m, MonadBaseControl IO m, PersistRecordBackend record SqlBackend) => Connection__ -> Key record -> m (Maybe record)
get connpool key = runSqlPool (Database.Persist.get key) connpool

find :: (MonadIO m, MonadBaseControl IO m, PersistRecordBackend record SqlBackend) => Connection__ -> Int64 -> m (Maybe (Key record, record))
find connpool id =
  case keyFromValues [PersistInt64 id] of
    Left x -> return Nothing
    Right key -> do
      mval <- DB.Persist.get connpool key
      return $ case mval of
        Just val -> Just (key,val)
        Nothing  -> Nothing

getBy :: (MonadIO m, MonadBaseControl IO m, PersistRecordBackend record SqlBackend) => Connection__ -> Unique record -> m (Maybe (Key record,record))
getBy connpool unique = do
  res <- runSqlPool (Database.Persist.getBy unique) connpool
  return $ case res of
    Just (Entity k v) -> Just (k,v)
    Nothing -> Nothing

select :: (MonadIO m, MonadBaseControl IO m, PersistRecordBackend record SqlBackend) => Connection__ -> [Filter record] -> [SelectOpt record] -> m [(Key record, record)]
select connpool filters opts = do
  res <- runSqlPool (Database.Persist.selectList filters opts) connpool
  return $ Prelude.map (\(Entity k v) -> (k,v)) res
  
selectKeys :: (MonadResource m, MonadBaseControl IO m, PersistEntity record, BaseBackend (BaseBackend SqlBackend) ~ PersistEntityBackend record, MonadReader SqlBackend m) => Connection__ -> [Filter record] -> [SelectOpt record] -> Source m (Key record)
selectKeys connpool filters opts = Database.Persist.selectKeys filters opts

count :: (MonadIO m, MonadBaseControl IO m, PersistRecordBackend record SqlBackend) => Connection__ -> [Filter record] -> m Int
count connpool filters = runSqlPool (Database.Persist.count filters) connpool

checkUnique :: (MonadIO m, MonadBaseControl IO m, PersistRecordBackend record SqlBackend) => Connection__ -> record -> m (Maybe (Unique record))
checkUnique connpool record = runSqlPool (Database.Persist.checkUnique record) connpool

keyToStrings :: (PersistEntity record) => Key record -> [String]
keyToStrings key = Prelude.map show $ keyToValues key
