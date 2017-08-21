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
--  , Reservation(..)
  , ORM.Channel(..)
--  , Channel(..)
  , ORM.Program(..)
--  , Program(..)
  , Update
  , Unique
  , Filter
  , SelectOpt
  , Entity
  , Table(..)
  , readTable
  , reservationsTable
  , channelsTable
  , programsTable
  , mkAsc, mkDesc, mkOffset, mkLimit
  , mkAndFilter, mkOrFilter
  , (.==), (.!=), (.<), (.>), (.<=), (.>=), (.||)
  , eq, neq, lt, gt, lte, gte, DB.ORMLinker.or
  , in_, notIn
  , EntityField
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
mkAsc    = ORM.mkAsc
mkDesc   = ORM.mkDesc
mkOffset = ORM.mkOffsetBy
mkLimit  = ORM.mkLimitTo
mkAndFilter = ORM.mkAndFilter
mkOrFilter = ORM.mkOrFilter
type EntityField = ORM.EntityField__
type Entity record = (Key record,record)
type Reservation = ORM.Reservation
type Channel     = ORM.Channel
type Program     = ORM.Program

(.==) l r = (ORM.==.) l r
eq    l r = l .== r

(.!=) l r = (ORM.!=.) l r
neq   l r = l .!= r

(.<)  l r = (ORM.<.)  l r
lt    l r = l .< r

(.>)  l r = (ORM.>.)  l r
gt    l r = l .> r

(.<=) l r = (ORM.<=.) l r
lte   l r = l (.<=) r

(.>=) l r = (ORM.>=.) l r
gte   l r = l (.>=) r

infix 4 .==, .!=, .<, .>, .<=, .>=, `eq`, `neq`, `lt`, `gt`, `lte`, `gte`

in_    l r = (ORM.<-.)  l r
notIn  l r = (ORM./<-.) l r
infix 4 `in_`, `notIn`

(.||) l r = (ORM.||.) l r
or    l r = l .|| r
infixl 3 .||, `or`

type Query m a = ORM.Query m a
  
data Table record = MkTable {
  connection :: Connection
  ,insert :: record -> IO (Key record)
  ,update :: Key record -> [Update record] -> IO record
  ,insertBy :: record -> IO (Either (Entity record) (Key record))
  ,updateWhere :: [Filter record] -> [Update record] -> IO ()
  ,repsert     :: Key record -> record -> IO ()
  ,delete      :: Key record -> IO ()
  ,deleteBy    :: Unique record -> IO ()
  ,deleteWhere :: [Filter record] -> IO ()
  ,get         :: Key record -> IO (Maybe (Entity record))
  ,find        :: Int64 -> IO (Maybe (Entity record))
  ,getBy       :: Unique record -> IO (Maybe (Entity record))
  ,select      :: [Filter record] -> [SelectOpt record] -> IO [Entity record]
--  ,selectKeys  :: [Filter record] -> [SelectOpt record] -> Source IO (Key record)
  ,count       :: [Filter record] -> IO Int
  ,checkUnique :: record -> IO (Maybe (Unique record))
  ,keyToStrings :: Key record -> [String]
  ,entityToJSON :: Entity record -> J.Value
  ,insertQuery      :: record -> Query IO (Key record)    
  ,updateQuery      :: Key record -> [Update record] -> Query IO record    
  ,insertByQuery    :: record -> Query IO (Either (Entity record) (Key record))    
  ,updateWhereQuery :: [Filter record] -> [Update record] -> Query IO ()    
  ,repsertQuery     :: Key record -> record -> Query IO ()    
  ,deleteQuery      :: Key record -> Query IO ()    
  ,deleteByQuery    :: Unique record -> Query IO ()    
  ,deleteWhereQuery :: [Filter record] -> Query IO ()    
  ,getQuery         :: Key record -> Query IO (Maybe (Entity record))    
  ,findQuery        :: Int64 -> Query IO (Maybe (Entity record))    
  ,getByQuery       :: Unique record -> Query IO (Maybe (Entity record))    
  ,selectQuery      :: [Filter record] -> [SelectOpt record] -> Query IO [Entity record]    
  ,countQuery       :: [Filter record] -> Query IO Int    
  ,checkUniqueQuery :: record -> Query IO (Maybe (Unique record))
}

-- readTable :: Connection -> Table record
readTable conn = MkTable {
  connection   = conn
  ,insert      = ORM.insert       conn
  ,update      = ORM.update       conn
  ,insertBy    = ORM.insertBy     conn
  ,updateWhere = ORM.updateWhere  conn
  ,repsert     = ORM.repsert      conn
  ,delete      = ORM.delete       conn
  ,deleteBy    = ORM.deleteBy     conn
  ,deleteWhere = ORM.deleteWhere  conn
  ,get         = ORM.get          conn
  ,find        = ORM.find         conn
  ,getBy       = ORM.getBy        conn
  ,select      = ORM.select       conn
  ,count       = ORM.count        conn
  ,checkUnique = ORM.checkUnique  conn
  ,keyToStrings = ORM.keyToStrings
  ,entityToJSON = (\(k,v) -> J.toJSON $ fromList [("id", idToJSON $ ORM.keyToStrings k ),("values",J.toJSON v)])
  ,insertQuery      = ORM.insertQuery
  ,updateQuery      = ORM.updateQuery
  ,insertByQuery    = ORM.insertByQuery
  ,updateWhereQuery = ORM.updateWhereQuery
  ,repsertQuery     = ORM.repsertQuery
  ,deleteQuery      = ORM.deleteQuery
  ,deleteByQuery    = ORM.deleteByQuery
  ,deleteWhereQuery = ORM.deleteWhereQuery
  ,getQuery         = ORM.getQuery
  ,findQuery        = ORM.findQuery
  ,getByQuery       = ORM.getByQuery
  ,selectQuery      = ORM.selectQuery
  ,countQuery       = ORM.countQuery
  ,checkUniqueQuery = ORM.checkUniqueQuery
  }

idToJSON :: [String] -> J.Value
idToJSON []     = J.Null
idToJSON (x:[]) = J.toJSON x
idToJSON xs     = J.toJSON xs

reservationsTable conn = readTable conn :: Table ORM.Reservation
channelsTable     conn = readTable conn :: Table ORM.Channel
programsTable     conn = readTable conn :: Table ORM.Program

instance J.ToJSON (Table record, Entity record) where
  toJSON (t, e) = (entityToJSON t) e
