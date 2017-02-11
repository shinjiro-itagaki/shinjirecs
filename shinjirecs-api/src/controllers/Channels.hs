{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
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
import Database.Persist.Sql.Types.Internal (SqlBackend)
import Model (ModelClass, Model, Models, getModel, getModels)

data Channels = Channels { conn :: ConnectionPool, models :: Models }

-- before :: Channels -> DefaultActionSymbol -> (Bool, Channels)
before :: Channels -> ActionSymbol -> (Bool, Channels)
before c List = (True, c)
before c _    = (True, c)

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

