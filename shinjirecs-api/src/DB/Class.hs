{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-} -- forall
{-# LANGUAGE AllowAmbiguousTypes #-}

module DB.Class where
-- import Language.Haskell.TH.Syntax
import Data.Time (Day, TimeOfDay, UTCTime)
import Data.ByteString.Char8 (ByteString, unpack)
import Control.Applicative
import Data.Typeable (Typeable)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)

data SaveError = MkSaveError
data DestroyError = MkDestroyError

data (Record r) => SaveResult    r = SaveSuccess    r | SaveFailed SaveError
data (Record r) => DestroyResult r = DestroySuccess r | DestroyFailed DestroyError

data TableDef
  = TabledDef
    { tableName    :: String
    , tableAttribs :: [String]
    , tableColumns :: [(String, String, [String])]
    , tableUniques :: [(String, [String])]
    , tableDerives :: [String]
    }
  deriving Show

data RecordValue = RecordString String
                 | RecordByteString ByteString
                 | RecordInt64 Int64
                 | RecordDouble Double
                 | RecordBool Bool
                 | RecordDay Day
                 | RecordTimeOfDay TimeOfDay
                 | RecordUTCTime UTCTime
                 | RecordNull
                 deriving (Show, Read, Eq, Typeable)

data RecordOrder = Asc | Desc
    deriving (Read, Show)


data RecordUpdate = Update | Add | Subtract | Multiply | Divide
    deriving (Read, Show)

data SqlType = SqlString
             | SqlInt32
             | SqlInt64
             | SqlReal
             | SqlBool
             | SqlDay
             | SqlTime
             | SqlDayTime
             | SqlBlob
             deriving (Show, Read, Eq, Typeable)

class RecordField a where
  toRecordValue :: a -> RecordValue
  fromRecordValue :: RecordValue -> Either String a
  sqlType :: a -> SqlType
  isNullable :: a -> Bool

data SomeRecordField = forall a. RecordField a => SomeRecordField a

data RecordFilter = Eq | Ne | Gt | Lt | Ge | Le | In | NotIn
                   deriving (Read, Show)

class Record r where
  data Key    r
  data Update r
  data Filter r
  data Order  r
  data Unique r
  
  recordDef :: r -> TableDef
  toRecordFields :: r -> [SomeRecordField]
  fromRecordValues :: [RecordValue] -> Either String r
  halfDefined :: r
  toRecordKey :: Int64 -> Key r
  fromRecordKey :: Key r -> Int64
  showRecordKey :: Key r -> String

  recordFilterToFieldName :: Filter r -> String
  recordFilterToFilter :: Filter r -> RecordFilter
  recordFilterToValue :: Filter r -> Either RecordValue [RecordValue]

  recordOrderToFieldName :: Order r -> String
  recordOrderToOrder :: Order r -> RecordOrder

  recordUpdateToFieldName :: Update r -> String
  recordUpdateToUpdate :: Update r -> RecordUpdate
  recordUpdateToValue :: Update r -> RecordValue

  recordUniqueToFieldNames :: Unique r -> [String]
  recordUniqueToValues :: Unique r -> [RecordValue]
  recordUniqueKeys :: r -> [Unique r]
