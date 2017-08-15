{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}

module DB.ORMLinker (
  Connection
  , connect
  , migrate
  , Key
  , ORM.Reservation(..)
  , ORM.Channel(..)
  , ORM.Program(..)
  , Update
  , Unique
  , Filter
  , SelectOpt
  , Entity
  , Table(..)
  , readTable
  , ReservationsTable
  , ChannelsTable
  , ProgramsTable
  , reservationsTable
  , channelsTable
  , programsTable
--  , keyToStrings
  ) where
import qualified DB.Persist as ORM
import Data.Int(Int64)
import DB.Config
import Control.Monad.Logger (runNoLoggingT)
import Data.Map(Map,fromList)
import Data.Text(Text,pack)
import qualified Data.Aeson as J

type Connection = ORM.Connection__

-- connect :: (MonadIO m, MonadBaseControl IO m, MonadLogger m) => Config -> m Connection
connect :: DB.Config.Config -> IO Connection
connect = runNoLoggingT . ORM.connect

migrate :: DB.Config.Config -> IO ()
migrate = ORM.migrate

type Key    = ORM.Key__
type Update = ORM.Update__
type Unique = ORM.Unique__
type Filter = ORM.Filter__
type SelectOpt = ORM.SelectOpt__
type Entity record = (Key record,record)

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
  ,find        :: Int64 -> IO (Maybe (Entity record))
  ,getBy       :: Unique record -> IO (Maybe (Entity record))
  ,select      :: [Filter record] -> [SelectOpt record] -> IO [(Key record, record)]
--  ,selectKeys  :: [Filter record] -> [SelectOpt record] -> Source IO (Key record)
  ,count       :: [Filter record] -> IO Int
  ,checkUnique :: record -> IO (Maybe (Unique record))
}

-- readTable :: Connection -> Table record
readTable conn = MkTable {
  connection   = conn
  ,insert      = ORM.insert       conn
  ,update      = ORM.update       conn
  ,insertBy    = ORM.insertBy     conn
  ,updateWhere = ORM.updateWhere  conn
  ,delete      = ORM.delete       conn
  ,deleteBy    = ORM.deleteBy     conn
  ,deleteWhere = ORM.deleteWhere  conn
  ,get         = ORM.get          conn
  ,find        = ORM.find         conn
  ,getBy       = ORM.getBy        conn
  ,select      = ORM.select       conn
  ,count       = ORM.count        conn
  ,checkUnique = ORM.checkUnique  conn
  }

type ReservationsTable = Table ORM.Reservation
type ChannelsTable     = Table ORM.Channel
type ProgramsTable     = Table ORM.Program

reservationsTable conn = readTable conn :: ReservationsTable
channelsTable     conn = readTable conn :: ChannelsTable
programsTable     conn = readTable conn :: ProgramsTable

--keyToStrings :: Key record -> [String]
--keyToStrings key = ORM.keyToStrings key

-- entityToJSON :: Entity record -> (J.Value, J.Value)
-- entityToJSON = ORM.entityToJSON

{-
instance (J.ToJSON record) => J.ToJSON (Key record) where
  toJSON k = id' $ keyToStrings k
    where
      id' :: [String] -> J.Value
      id' []     = J.Null
      id' (x:[]) = J.toJSON x
      id' xs     = J.toJSON xs

instance (J.ToJSON record) => J.ToJSON (Entity record) where
  toJSON (k,v) = J.toJSON $ fromList [(pack "id") (id' $ keyToStrings k)), (pack "values", J.toJSON v)]
    where
      id' :: [String] -> J.Value
      id' []     = J.Null
      id' (x:[]) = J.toJSON x
      id' xs     = J.toJSON xs
-}
