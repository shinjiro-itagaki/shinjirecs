module Controller where
import DB
import Web.Scotty (ActionM)
import Control.Monad.IO.Class(liftIO,MonadIO) -- base
import Database.Persist.Sql(ConnectionPool,SqlPersistT, runSqlPool)  --persistent
import Database.Persist (PersistEntity (..)) --persistent
import qualified Database.Persist.Class as PS
import Data.Enumerator (Enumerator) -- enumerator
-- class Eq a => ActionSymbol a
data ActionSymbol = Index | List | Get | Read | Modify | Edit | Create | New | Delete | Destroy deriving Enum
toActionSym i = toEnum i :: ActionSymbol

-- instance ActionSymbol DefaultActionSymbol

--findRecord :: (Monad m, PS.ToBackendKey SqlBackend record) => (SqlPersistT IO (Maybe record) -> m (Maybe record)) -> PS.Key record -> m (Maybe record)
--findRecord db key = do
--  db $ PS.get key -- Sql.SqlPersistT IO (Maybe record)

runDB :: MonadIO m => ConnectionPool -> SqlPersistT IO a -> m a
runDB p action = liftIO $ runSqlPool action p

class Controller a where
  new :: ConnectionPool -> a
  beforeAction :: Int -> a -> ActionM (Bool, a)
  beforeAction sym c = do
    return (True, c)
  afterAction :: Int -> a -> ActionM ()
  afterAction sym c = do
    return ()

data (Controller c) => ControllerAction c = ControllerAction Int (c -> ActionM c)

