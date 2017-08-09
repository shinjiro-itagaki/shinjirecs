{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
-- {-# LANGUAGE ScopedTypeVariables #-}

module DB (
  module DB.Config
  ,module DB.Types
  ,module DB
  , ORM.Reservation(..)
  , ORM.Channel(..)
  , ORM.Program(..)
  , Entity(..)
  ) where
import DB.Config
import qualified DB.Persist as ORM
-- import qualified DB.HDBC as ORM
import DB.Types(AdapterType(..))
-- import DB.Class(Record(..))

-- import Data.ByteString -- bytestring
-- import Data.Pool(Pool) -- base
import Database.Persist.Sql(ConnectionPool)
import Data.Enumerator(Enumerator, (==<<),run)
import Data.Enumerator.List(consume)
import Control.Monad(liftM)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Trans.Resource (MonadBaseControl) -- resourcet
import Control.Monad.IO.Class(liftIO,MonadIO) -- base
import Data.Conduit(Source)
import DB.Status(ReservationState(..))

type Connection = ORM.Connection__

migrate :: DB.Config.Config -> IO ()
migrate = ORM.migrate

-- type Query = ORM.Query

connect :: (MonadIO m, MonadBaseControl IO m, MonadLogger m) => Config -> m Connection
connect = ORM.connect

-- data Condition :: (Record r) = MkCondition r
-- type Record = ORM.Record

notImplemented = error "not implemented"

type Key    = ORM.Key__
type Update = ORM.Update__
type Unique = ORM.Unique__
type Filter = ORM.Filter__
type Entity = ORM.Entity__
{-
data Entity record = Entity {
  entityKey :: Key record,
  entityVal :: record
  }
-}
type SelectOpt = ORM.SelectOpt__

data Table record = MkTable {
  connection :: Connection
  ,insert :: record -> IO (Key record)
  ,update :: Key record -> [Update record] -> IO record
  ,insertBy :: record -> IO (Either (Entity record) (Key record))
  ,updateWhere :: [Filter record] -> [Update record] -> IO ()
  ,delete      :: Key record -> IO ()
  ,deleteBy    :: Unique record -> IO ()
  ,deleteWhere :: [Filter record] -> IO ()
  ,get         :: Key record -> IO (Maybe record)
  ,getBy       :: Unique record -> IO (Maybe (Entity record))
  ,select      :: [Filter record] -> [SelectOpt record] -> IO [Entity record]
--  ,selectKeys  :: [Filter record] -> [SelectOpt record] -> Source IO (Key record)
  ,count       :: [Filter record] -> IO Int
  ,checkUnique :: record -> IO (Maybe (Unique record))
}


-- mkTable :: Connection -> Table record
mkTable conn = MkTable {
  connection   = conn
  ,insert      = ORM.insert       conn
  ,update      = ORM.update       conn
  ,insertBy    = ORM.insertBy     conn
  ,updateWhere = ORM.updateWhere  conn
  ,delete      = ORM.delete       conn
  ,deleteBy    = ORM.deleteBy     conn
  ,deleteWhere = ORM.deleteWhere  conn
  ,get         = ORM.get          conn
  ,getBy       = ORM.getBy        conn
  ,select      = ORM.select       conn
  ,count       = ORM.count        conn
  ,checkUnique = ORM.checkUnique  conn
  }

reservationsTable conn = mkTable conn :: Table ORM.Reservation
channelsTable     conn = mkTable conn :: Table ORM.Channel
programsTable     conn = mkTable conn :: Table ORM.Program
