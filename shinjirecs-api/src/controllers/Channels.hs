{-# LANGUAGE OverloadedStrings #-}
module Controllers.Channels where
-- import Data.Eq (Eq)
import Controller(Controller(..), ControllerAction(..))
import Controller(ActionSymbol(List), toActionSym)

-- import Web.Scotty (ScottyM, scotty, status, json, get, patch, delete , post, options, ActionM, param, jsonData, addroute, setHeader,middleware, RoutePattern)
import Web.Scotty (ActionM)
import Control.Monad.IO.Class(MonadIO) -- base
import qualified Database.Persist as P --persistent
import qualified DB
import qualified Database.Persist.Class as PS
import Database.Persist.Sql(ConnectionPool)  --persistent

-- MonadIO m => Sql.ConnectionPool -> Sql.SqlPersistT IO a -> m a
-- PS.ToBackendKey SqlBackend record
-- data (MonadIO m, PS.ToBackendKey SqlBackend record) => Channels = Channels {db :: (Sql.SqlPersistT IO (ActionM (Maybe record)) -> m (ActionM (Maybe record))) }
data Channels = Channels { conn :: ConnectionPool }

-- before :: Channels -> DefaultActionSymbol -> (Bool, Channels)
before :: Channels -> ActionSymbol -> (Bool, Channels)
before c List = (True, c)
before c _    = (True, c)

instance Controller Channels where
  new conn' = Channels { conn = conn'}
  beforeAction i c = do
    return $ before c $ toActionSym i
    
list = ControllerAction (fromEnum List) (
  \c@Channels { conn = _ } -> do
    let filter = [] :: [P.Filter DB.Channel]
        opt    = [] :: [P.SelectOpt DB.Channel]
--    records <- db $ map P.entityVal <$> P.selectList filter opt
--    json records
    return c
  )

