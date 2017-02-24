{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Controllers.Programs where
import Controller(Controller(..), DefaultActionSymbol(..), def, ActionSymbol(..), ToJsonResponse(..), toJsonResponseME, ResponseType(..), findRecord)

import Data.Bool(bool)
import Data.Maybe(maybe, fromMaybe, isJust, isNothing, fromJust) -- !!!
import Web.Scotty(json,param,jsonData, ActionM, status)
import Network.HTTP.Types (status200, status201, status400, status404, StdMethod(..))
import Control.Monad.IO.Class(MonadIO,liftIO) -- base
import qualified Database.Persist as P --persistent
import Database.Persist.Types (Entity(entityVal))
import qualified DB
import qualified Database.Persist.Class as PS
import Database.Persist.Sql(ConnectionPool, SqlPersistT, runSqlPool)  --persistent
import Database.Persist.Sql.Types.Internal (SqlBackend)
import Model (find, saveE,saveR ,ToMaybeEntity(..))
import qualified Model as M
import Models.Program
import Control.Monad.Reader(ReaderT) -- mtl

data ProgramsController = ProgramsController { conn_ :: ConnectionPool }

instance (Controller DefaultActionSymbol) ProgramsController where
  new  _              = ProgramsController
  conn _              = conn_
  beforeAction List c = return (True, c)
  beforeAction _    c = return (True, c)

-- クラスに記載された関数を実行したら、インスタンスの候補が複数存在するとしてエラーになるので以下のように戻り値の型を明示した関数を作成した
toMaybeEntity' x = toMaybeEntity x :: Maybe (Entity DB.Program)

list, get, modify, create, destroy :: (DefaultActionSymbol, (ProgramsController -> ActionM ProgramsController))

list = def List list'
  where
    filter = [] :: [P.Filter DB.Program]
    opt    = [] :: [P.SelectOpt DB.Program]
    list' :: ProgramsController -> ActionM ProgramsController
    list' c = (db List c $ P.selectList filter opt) >>= json . map P.entityVal >> return c

get = def Get impl'
  where
    impl' :: ProgramsController -> ActionM ProgramsController
    impl' c = do
      mEntity <- findRecord "id" Get c :: ActionM (Maybe (Entity DB.Program))
      toJsonResponseME FindR mEntity >> return c

modify = def Modify impl'
  where
    impl' :: ProgramsController -> ActionM ProgramsController
    impl' c = do
      mEntity <- findRecord "id" Get c :: ActionM (Maybe (Entity DB.Program))
      newrec <- (jsonData :: ActionM DB.Program)
      case mEntity of
        Just e  -> (db Get c $ saveE $ e {entityVal = newrec}) >>= return . toMaybeEntity' >>= toJsonResponseME SaveR >> return c
        Nothing -> return c

create = def Create impl'
  where
    impl' :: ProgramsController -> ActionM ProgramsController
    impl' c = do
      newrec <- (jsonData :: ActionM DB.Program)
      (db Create c $ saveR newrec) >>= return . toMaybeEntity' >>= toJsonResponseME SaveR >> return c

destroy = def Destroy impl'
  where
    destroy' :: Entity DB.Program -> ReaderT SqlBackend IO (Bool, (Entity DB.Program), PS.Key DB.Program)
    destroy' e = M.destroy e
    findRecord' c = findRecord "id" Destroy c :: ActionM (Maybe (Entity DB.Program))
    impl' :: ProgramsController -> ActionM ProgramsController
    impl' c = do
      findRecord' c >>= maybe
        (status status404 >> return c)
        (\e -> do
            (b, e2, k) <- db Destroy c $ destroy' e
            status $ bool status201 status400 b
            return c
        )
