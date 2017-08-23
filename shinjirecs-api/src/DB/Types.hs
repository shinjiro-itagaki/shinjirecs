{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric   #-}

{-# LANGUAGE ExistentialQuantification #-}

module DB.Types where
import Database.Persist.TH
import qualified Data.Aeson as A
import qualified Data.Text as T
import GHC.Generics
import Database.Persist.Sql(PersistFieldSql(sqlType))
import Database.Persist(SqlType(SqlInt32))
import Control.Exception(Exception,throw)
import Data.Typeable(Typeable)

data ChannelType =  GR | BS | CS deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)
derivePersistField "ChannelType"

instance A.ToJSON ChannelType
instance A.FromJSON ChannelType

data AdapterType = MySQL | PostgreSQL | SQLite3 deriving Show
stringToAdapterType :: String -> Maybe AdapterType
stringToAdapterType str =
  case str of
    "mysql"      -> return MySQL
    "postgresql" -> return PostgreSQL
    "sqlite3"    -> return SQLite3
    _            -> Nothing


data CommitOrRollback onCommit onRollback = Commit onCommit | Rollback onRollback

data ActionState onSaved onNotSaved cancelOn = NoProblem onSaved | Canceled (CommitOrRollback onNotSaved onNotSaved) cancelOn | Failed (CommitOrRollback onNotSaved onNotSaved)

-- TransactionRequest ifSaved ifNotSaved = TransactionRequest (NoProblem onSaved | Cancel (Commit | Rollback onNotSaved) | Failed (Commit onNotSaved | Rollback onNotSaved))
data TransactionRequest ifSaved ifNotSaved cancelOn = TransactionRequest (ActionState ifSaved ifNotSaved cancelOn)

-- TransactionResult ifSaved ifNotSaved = TransactionResult (NoProblem onSaved | Cancel (Commit onNotSaved| Rollback onNotSaved) | Failed (Commit onNotSaved | Rollback onNotSaved)) | UnknownError
data TransactionResult  ifSaved ifNotSaved cancelOn = TransactionResult (ActionState ifSaved ifNotSaved cancelOn) | UnknownError
