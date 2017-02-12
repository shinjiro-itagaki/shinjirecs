{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE FunctionalDependencies #-}
-- {-# LANGUAGE ConstraintKinds #-}

module Model where
import Control.Monad.IO.Class(MonadIO,liftIO) -- base
import qualified Database.Persist.Class as PS
import Database.Persist.Sql(ConnectionPool, SqlPersistT, runSqlPool)  --persistent
import Database.Persist.Sql.Types.Internal (SqlBackend)
import Database.Persist.Types (Update,Entity,Filter,SelectOpt)
import qualified DB
import Web.Scotty (ActionM)
import Control.Monad.Trans.Resource(MonadResource) -- resourcet
import Control.Monad.Reader(ReaderT) -- mtl
import Control.Monad.Reader.Class(MonadReader) -- mtl
import Data.Acquire(Acquire) -- resourcet
import Data.Conduit(Source) --- conduit
-- import Data.Aeson.Types(ToJSON, FromJSON) -- aeson

runDB :: MonadIO m => ConnectionPool -> SqlPersistT IO a -> m a
runDB p action = liftIO $ runSqlPool action p

class (PS.PersistEntity record
      , PS.ToBackendKey SqlBackend record
      , PS.PersistRecordBackend record SqlBackend
      ) => ModelClass record where
  get                :: (MonadIO m) => ConnectionPool -> PS.Key record -> m (Maybe record)
  get conn key                  = do { runDB conn $ PS.get key }
  insert             :: (MonadIO m) => ConnectionPool -> record -> m (PS.Key record)
  insert conn record            = do { runDB conn $ PS.insert record }
  insert_            :: (MonadIO m) => ConnectionPool -> record -> m ()
  insert_ conn record           = do { runDB conn $ PS.insert_ record }
  insertMany         :: (MonadIO m) => ConnectionPool -> [record] -> m [PS.Key record]
  insertMany conn records       = do { runDB conn $ PS.insertMany records }
  insertMany_        :: (MonadIO m) => ConnectionPool -> [record] -> m ()
  insertMany_ conn records      = do { runDB conn $ PS.insertMany_ records }
  insertEntityMany   :: (MonadIO m) => ConnectionPool -> [Entity record] -> m ()
  insertEntityMany conn records = do { runDB conn $ PS.insertEntityMany records }
  insertKey          :: (MonadIO m) => ConnectionPool -> PS.Key record -> record -> m ()
  insertKey conn key record     = do { runDB conn $ PS.insertKey key record }
  repsert            :: (MonadIO m) => ConnectionPool -> PS.Key record -> record -> m ()
  repsert conn key record       = do { runDB conn $ PS.repsert key record }
  replace            :: (MonadIO m) => ConnectionPool -> PS.Key record -> record -> m ()
  replace conn key record       = do { runDB conn $ PS.replace key record }
  delete             :: (MonadIO m) => ConnectionPool -> PS.Key record -> m ()
  delete conn key               = do { runDB conn $ PS.delete key }
  update             :: (MonadIO m) => ConnectionPool -> PS.Key record -> [Update record] -> m ()
  update conn key conds         = do { runDB conn $ PS.update key conds }
  updateGet          :: (MonadIO m) => ConnectionPool -> PS.Key record -> [Update record] -> m record
  updateGet conn key conds      = do { runDB conn $ PS.updateGet key conds }
  getJust            :: (MonadIO m) => ConnectionPool -> PS.Key record -> m record
  getJust conn key              = do { runDB conn $ PS.getJust key }
  belongsTo          :: (MonadIO m, PS.PersistEntity record2, PS.ToBackendKey SqlBackend record2) => ConnectionPool -> (record -> Maybe (PS.Key record2)) -> record -> m (Maybe record2)
  belongsTo conn idfunc r       = do { runDB conn $ PS.belongsTo idfunc r }
  belongsToJust      :: (MonadIO m, PS.PersistEntity record2, PS.ToBackendKey SqlBackend record2) => ConnectionPool -> (record -> PS.Key record2) -> record -> m record2
  belongsToJust conn idfunc r   = do { runDB conn $ PS.belongsToJust idfunc r }
  insertEntity       :: (MonadIO m) => ConnectionPool -> record -> m (Entity record)
  insertEntity conn r           = do { runDB conn $ PS.insertEntity r }
  getBy              :: (MonadIO m) => ConnectionPool -> PS.Unique record -> m (Maybe (Entity record))
  getBy conn uniq               = do { runDB conn $ PS.getBy uniq }
  deleteBy           :: (MonadIO m) => ConnectionPool -> PS.Unique record -> m ()
  deleteBy conn uniq            = do { runDB conn $ PS.deleteBy uniq }
  insertUnique       :: (MonadIO m) => ConnectionPool -> record -> m (Maybe (PS.Key record))
  insertUnique conn r           = do { runDB conn $ PS.insertUnique r }
  upsert             :: (MonadIO m) => ConnectionPool -> record -> [Update record] -> m (Entity record)
  upsert conn r updates         = do { runDB conn $ PS.upsert r updates }
  upsertBy           :: (MonadIO m) => ConnectionPool -> PS.Unique record -> record -> [Update record] -> m (Entity record)
  upsertBy conn uniq r updates  = do { runDB conn $ PS.upsertBy uniq r updates }
  getByValue         :: (MonadIO m) => ConnectionPool -> record -> m (Maybe (Entity record))
  getByValue conn r             = do { runDB conn $ PS.getByValue r }
  insertBy           :: (MonadIO m) => ConnectionPool -> record -> m (Either (Entity record) (PS.Key record))
  insertBy conn r               = do { runDB conn $ PS.insertBy r }
  checkUnique        :: (MonadIO m) => ConnectionPool -> record -> m (Maybe (PS.Unique record))
  checkUnique conn r            = do { runDB conn $ PS.checkUnique r }
  onlyUnique         :: (MonadIO m) => ConnectionPool -> record -> m (PS.Unique record)
  onlyUnique conn r             = do { runDB conn $ PS.onlyUnique r }
  selectSourceRes    :: (MonadIO m1, MonadIO m2) => ConnectionPool -> [Filter record] -> [SelectOpt record] -> m1 (Acquire (Source m2 (Entity record)))
  selectSourceRes conn filters conds = do { runDB conn $ PS.selectSourceRes filters conds }
  selectFirst        :: (MonadIO m) => ConnectionPool -> [Filter record] -> [SelectOpt record] -> m (Maybe (Entity record))
  selectFirst conn filters conds     = do { runDB conn $ PS.selectFirst filters conds }
  selectKeysRes      :: (MonadIO m1, MonadIO m2) => ConnectionPool -> [Filter record] -> [SelectOpt record] -> m1 (Acquire (Source m2 (PS.Key record)))
  selectKeysRes conn filters conds   = do { runDB conn $ PS.selectKeysRes filters conds }
  count              :: (MonadIO m) => ConnectionPool -> [Filter record] -> m Int
  count conn filters                 = do { runDB conn $ PS.count filters }
  updateWhere        :: (MonadIO m) => ConnectionPool -> [Filter record] -> [Update record] -> m ()
  updateWhere conn filters updates   = do { runDB conn $ PS.updateWhere filters updates }
  deleteWhere        :: (MonadIO m) => ConnectionPool -> [Filter record] -> m ()
  deleteWhere conn filters           = do { runDB conn $ PS.deleteWhere filters }
  selectSource       :: (PS.PersistQueryRead (PS.BaseBackend backend), MonadResource m, PS.PersistEntityBackend record ~ PS.BaseBackend (PS.BaseBackend backend), MonadReader backend m, PS.HasPersistBackend backend) => [Filter record] -> [SelectOpt record] -> Source m (Entity record)
  selectSource filters conds    = PS.selectSource filters conds
  selectKeys         :: (PS.PersistQueryRead (PS.BaseBackend backend), MonadResource m, PS.BaseBackend (PS.BaseBackend backend) ~ PS.PersistEntityBackend record, MonadReader backend m, PS.HasPersistBackend backend) => [Filter record] -> [SelectOpt record] -> Source m (PS.Key record)
  selectKeys filters conds      = PS.selectKeys filters conds
  selectList         :: (MonadIO m) => ConnectionPool -> [Filter record] -> [SelectOpt record] -> m [Entity record]
  selectList conn filters conds      = do { runDB conn $ PS.selectList filters conds }
  selectKeysList     :: (MonadIO m) => ConnectionPool -> [Filter record] -> [SelectOpt record] -> m [PS.Key record]
  selectKeysList conn filters conds  = do { runDB conn $ PS.selectKeysList filters conds }


class (ModelClass record, PS.DeleteCascade record SqlBackend) => CascadeDeletableModel record where
  deleteCascade      :: (MonadIO m) => ConnectionPool -> PS.Key record -> m ()
  deleteCascade conn key             = do { runDB conn $ PS.deleteCascade key }
  deleteCascadeWhere :: (MonadIO m) => ConnectionPool -> [Filter record] -> m ()
  deleteCascadeWhere conn filters    = do { runDB conn $ PS.deleteCascadeWhere filters }

class (ModelClass record, Eq record, Eq (PS.Unique record) )=> UniqueReplaceableModel record where
  replaceUnique :: (MonadIO m) => ConnectionPool -> PS.Key record -> record -> m (Maybe (PS.Unique record))
  replaceUnique conn key r = do { runDB conn $ PS.replaceUnique key r }
  
data (ModelClass record) => Model record = Model {
                                                 }

data Models = Models {
  channels :: Model DB.Channel
  }

getModel :: (PS.PersistEntity record) => ConnectionPool -> Model record
getModel conn = Model {
--  finder = runDB conn
  }

getModels :: ConnectionPool -> Models
getModels conn = Models {
  channels = getModel conn
  }
