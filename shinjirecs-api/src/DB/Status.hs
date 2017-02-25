{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric   #-}
module DB.Status where
import Database.Persist.TH
import qualified Data.Aeson as A
import qualified Data.Text as T
import GHC.Generics
import Database.Persist.Sql(PersistFieldSql(sqlType))
import Database.Persist(SqlType(SqlInt32))
data ReservationState =  Waiting | Recording | Success | Failed deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)
derivePersistField "ReservationState"
instance A.ToJSON ReservationState
instance A.FromJSON ReservationState
