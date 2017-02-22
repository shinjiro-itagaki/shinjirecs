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
  conn :: a -> ConnectionPool
  db :: (MonadIO m) => a -> (SqlPersistT IO b -> m b)
  db = runDB . conn
  beforeAction :: ActionSymbol -> a -> ActionM (Bool, a)
  beforeAction sym c = return (True, c)
  afterAction :: ActionSymbol -> a -> ActionM ()
  afterAction sym c = return ()

data (Controller c) => ControllerAction c = ControllerAction ActionSymbol (c -> ActionM c)

def :: (Controller c) => ActionSymbol -> (c -> ActionM c) -> ControllerAction c
def sym impl = ControllerAction sym impl

