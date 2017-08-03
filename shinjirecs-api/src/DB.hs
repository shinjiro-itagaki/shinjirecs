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
  ) where
import DB.Config
-- import qualified DB.Persist as ORM
import qualified DB.HDBC as ORM
import DB.Types(AdapterType(..))
-- import DB.Class(Record)

-- import Data.ByteString -- bytestring
-- import Data.Pool(Pool) -- base
import Database.Persist.Sql(ConnectionPool)

type MyConnection = ORM.Connection__

migrate :: DB.Config.Config -> IO ()
migrate = ORM.migrate

type Query = ORM.Query

connect :: Config -> IO MyConnection
connect = ORM.connect


-- data Condition :: (Record r) = MkCondition r
-- type Record = ORM.Record

-- insert :: PersistEntity val => val -> m (Key val)
-- update :: PersistEntity val => Key val -> [Update val] -> m ()
-- updateWhere :: PersistEntity val => [Filter val] -> [Update val] -> m ()
-- delete :: PersistEntity val => Key val -> m ()
-- deleteBy :: PersistEntity val => Unique val -> m ()
-- deleteWhere :: PersistEntity val => [Filter val] -> m ()
-- get :: PersistEntity val => Key val -> m (Maybe val)
-- getBy :: PersistEntity val => Unique val -> m (Maybe (Key val, val))
-- selectSource :: PersistEntity val => [Filter val] -> [Order val]
--   -> Int --  limit
--   -> Int --  offset
--   -> Enumerator (Key val, val) m a
-- selectKeys :: PersistEntity val => [Filter val] -> Enumerator (Key val) m a
-- count :: PersistEntity val => [Filter val] -> m Int
