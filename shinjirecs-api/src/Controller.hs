module Controller where
import DB
import Web.Scotty (ActionM)
import Control.Monad.IO.Class(MonadIO) -- base
import Database.Persist.Sql(ConnectionPool,SqlPersistT)  --persistent

-- class Eq a => ActionSymbol a
data ActionSymbol = Index | List | Get | Read | Modify | Edit | Create | New | Delete | Destroy deriving Enum
toActionSym i = toEnum i :: ActionSymbol

-- instance ActionSymbol DefaultActionSymbol

class Controller a where
  new :: ConnectionPool -> a
  beforeAction :: Int -> a -> ActionM (Bool, a)
  beforeAction sym c = do
    return (True, c)
  afterAction :: Int -> a -> ActionM ()
  afterAction sym c = do
    return ()

-- data (Controller c) => ControllerAction c = ControllerAction Int (c -> ActionM c) | Direct (ActionM ())
data (Controller c) => ControllerAction c = ControllerAction Int (c -> ActionM c)

