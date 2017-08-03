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
import DB.Class(Key,Record(..),Filter,Order)

-- import Data.ByteString -- bytestring
-- import Data.Pool(Pool) -- base
import Database.Persist.Sql(ConnectionPool)
import Data.Enumerator(Enumerator, (==<<),run)
import Data.Enumerator.List(consume)
import Control.Monad(liftM)

type MyConnection = ORM.Connection__

migrate :: DB.Config.Config -> IO ()
migrate = ORM.migrate

-- type Query = ORM.Query

connect :: Config -> IO MyConnection
connect = ORM.connect

-- data Condition :: (Record r) = MkCondition r
-- type Record = ORM.Record

notImplemented = error "not implemented"

insert      :: (Record val, Monad m) => MyConnection -> val -> m (Key val)
update      :: (Record val, Monad m) => MyConnection -> Key val -> [Update val] -> m ()
updateWhere :: (Record val, Monad m) => MyConnection -> [Filter val] -> [Update val] -> m ()
delete      :: (Record val, Monad m) => MyConnection -> Key val -> m ()
deleteBy    :: (Record val, Monad m) => MyConnection -> Unique val -> m ()
deleteWhere :: (Record val, Monad m) => MyConnection -> [Filter val] -> m ()
get,find2   :: (Record val, Monad m) => MyConnection -> Key val -> m (Maybe val)
getBy       :: (Record val, Monad m) => MyConnection -> Unique val -> m (Maybe (Key val, val))
select      :: (Record val, Monad m) => MyConnection -> [Filter val] -> [Order val] -> Int -> Int -> Enumerator (Key val, val) m a
selectKeys  :: (Record val, Monad m) => MyConnection -> [Filter val] -> Enumerator (Key val) m a
count       :: (Record val, Monad m) => MyConnection -> [Filter val] -> m Int

insert      = ORM.insert
update      = ORM.update
updateWhere = ORM.updateWhere
delete      = ORM.delete
deleteBy    = ORM.deleteBy
deleteWhere = ORM.deleteWhere
get         = ORM.get
getBy       = ORM.getBy
select      = ORM.select
selectKeys  = ORM.selectKeys
count       = ORM.count

find2 = get

selectList :: (Record val, Monad m) => MyConnection -> [Filter val] -> [Order val] -> Int -> Int -> m [(Key val, val)]
selectList conn a b c d = do
  res <- run $ select conn a b c d ==<< consume
  case res of
    Left e -> error $ show e
    Right x -> return x
  
insertBy :: (Record v, Monad m) => MyConnection -> v -> m (Either (Key v, v) (Key v))
insertBy conn val =
  go $ recordUniqueKeys val
  where
    go [] = Right `liftM` insert conn val
    go (x:xs) = do
      y <- getBy conn x
      case y of
        Nothing -> go xs
        Just z -> return $ Left z


checkUnique :: (Record val, Monad m) => MyConnection -> val -> m Bool
checkUnique conn val =
  go $ recordUniqueKeys val
  where
    go [] = return True
    go (x:xs) = do
      y <- getBy conn x
      case y of
        Nothing -> go xs
        Just _ -> return False
