{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Class.String where
import Data.Char(toLower)
import System.Process(CreateProcess,createProcess,proc)
import Data.Dates(WeekDay(..),weekdayNumber,intToWeekday) -- dates
import Data.Time.Clock(UTCTime(..),addUTCTime,NominalDiffTime,getCurrentTime)
import Data.Word(Word)
import Control.Monad.IO.Class(MonadIO) -- base
import Data.Time.Calendar(fromGregorian,toGregorian)
import Data.Text as T
import Data.Text.Lazy as TL
import Data.ByteString as B
import Data.ByteString.Char8 as BC
import Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 as BLC

class StringClass a where
  toString      :: a -> Prelude.String
  toText        :: a -> T.Text
  toTextL       :: a -> TL.Text
  toByteString  :: a -> B.ByteString
  toByteStringL :: a -> BL.ByteString
  toStrings      :: [a] -> [Prelude.String]
  toTexts        :: [a] -> [T.Text]
  toTextLs       :: [a] -> [TL.Text]
  toByteStrings  :: [a] -> [B.ByteString]
  toByteStringLs :: [a] -> [BL.ByteString]
  toTextL       = TL.fromStrict . toText
  toByteStringL = BL.fromStrict . toByteString
  toText        = TL.toStrict   . toTextL
  toByteString  = BL.toStrict   . toByteStringL
  toStrings      = Prelude.map toString
  toTexts        = Prelude.map toText
  toTextLs       = Prelude.map toTextL
  toByteStrings  = Prelude.map toByteString
  toByteStringLs = Prelude.map toByteStringL

instance StringClass Prelude.String where
  toString    a = a
  toText        = T.pack
  toByteString  = BC.pack

instance StringClass T.Text where
  toString      = T.unpack
  toText      a = a
  toByteString  = BC.pack . toString

instance StringClass TL.Text where
  toString      = TL.unpack
  toTextL     a = a
  toByteString  = BC.pack . toString

instance StringClass B.ByteString where
  toString       = BC.unpack
  toText         = toText . toString
  toByteString a = a

instance StringClass BL.ByteString where
  toString        = toString . toByteString
  toText          = toText . BL.toStrict
  toByteStringL a = a
