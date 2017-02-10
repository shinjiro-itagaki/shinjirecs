{-# LANGUAGE OverloadedStrings #-}
module Controllers.Channels where
-- import Data.Eq (Eq)
import Controller(Controller(..), ControllerAction(..))
import Controller(ActionSymbol(List), toActionSym)

import Web.Scotty (ActionM)
import Control.Monad.IO.Class(MonadIO,liftIO) -- base
import qualified Database.Persist as P --persistent
import qualified DB
import qualified Database.Persist.Class as PS
import Database.Persist.Sql(ConnectionPool, SqlPersistT, runSqlPool)  --persistent

-- MonadIO m => Sql.ConnectionPool -> Sql.SqlPersistT IO a -> m a
-- PS.ToBackendKey SqlBackend record
data (PS.PersistEntity record) => Model record = Model {
  find :: (SqlPersistT IO (ActionM (Maybe record)) -> ActionM (ActionM (Maybe record)))
  }

-- class (PS.PersistEntity record) => ModelClass record where
--   finder :: (MonadIO m) => ConnectionPool -> (SqlPersistT IO (m (Maybe record)) -> m (m (Maybe record)))

-- instance ModelClass DB.Channel where
--   finder conn = runDB conn

data Models = Models {
  channels :: Model DB.Channel
  }

getModel :: (PS.PersistEntity record) => ConnectionPool -> Model record
getModel conn = Model {
  find = runDB conn
  }

getModels :: ConnectionPool -> Models
getModels conn = Models {
  channels = getModel conn
  }

data Channels = Channels { conn :: ConnectionPool, models :: Models }

-- before :: Channels -> DefaultActionSymbol -> (Bool, Channels)
before :: Channels -> ActionSymbol -> (Bool, Channels)
before c List = (True, c)
before c _    = (True, c)

runDB :: MonadIO m => ConnectionPool -> SqlPersistT IO a -> m a
runDB p action = liftIO $ runSqlPool action p

instance Controller Channels where
  new conn' = Channels { conn = conn', models = getModels conn' }
  beforeAction i c = do
    return $ before c $ toActionSym i
    
list = ControllerAction (fromEnum List) (
  \c -> do
    let filter = [] :: [P.Filter DB.Channel]
        opt    = [] :: [P.SelectOpt DB.Channel]
--    records <- db $ map P.entityVal <$> P.selectList filter opt
--    json records
    return (c :: Channels)
  )

