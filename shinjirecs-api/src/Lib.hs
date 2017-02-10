module Lib where
import Data.Eq (Eq)
import Web.Scotty (ScottyM, scotty, status, json, get, patch, delete , post, options, ActionM, param, jsonData, addroute, setHeader,middleware, RoutePattern)
import Control.Monad.IO.Class(MonadIO) -- base
import qualified Database.Persist.Sql as Sql --persistent

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- class Eq a => ActionSymbol a
data ActionSymbol = Index | List | Get | Read | Modify | Edit | Create | New | Delete | Destroy deriving Enum
toActionSym i = toEnum i :: ActionSymbol

-- instance ActionSymbol DefaultActionSymbol

class Controller a where
  new :: a
  beforeAction :: Int -> a -> ActionM (Bool, a)
  beforeAction sym c = do
    return (True, c)
  afterAction :: Int -> a -> ActionM ()
  afterAction sym c = do
    return ()

-- data (Controller c) => ControllerAction c = ControllerAction Int (c -> ActionM c) | Direct (ActionM ())
data (Controller c) => ControllerAction c = ControllerAction Int (c -> ActionM c)

