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
import qualified Data.String.Utils as Utils
import Data.Aeson(Value,decode)

class (Eq a) => StringClass a where
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
  join           ::  a  -> [a] -> a
  split          ::  a  -> a -> [a]
  (+++)          ::  a  -> a -> a
  replace        ::  a  -> a -> a -> a
  toTextL       = TL.fromStrict . toText
  toByteStringL = BL.fromStrict . toByteString
  toText        = TL.toStrict   . toTextL
  toByteString  = BL.toStrict   . toByteStringL
  toStrings      = Prelude.map toString
  toTexts        = Prelude.map toText
  toTextLs       = Prelude.map toTextL
  toByteStrings  = Prelude.map toByteString
  toByteStringLs = Prelude.map toByteStringL
  join  y (x:[]) = y +++ x
  join  y (x:xs) = x +++ y +++ (join y xs)
  replace old new l = Class.String.join new $ Class.String.split old $ l

-- if head is matched -> Just matched head deleted list
-- if head is not matched -> Nothing
deleteMatchedHead :: (Eq a) => [a] -> [a] -> Maybe [a]
--deleteMatchedHead = cond txt = impl' cond txt
deleteMatchedHead []      txt = Just txt -- head is matched
deleteMatchedHead cond     [] = Nothing
deleteMatchedHead cond@(x:xs) txt@(y:ys) = if x == y then deleteMatchedHead xs ys else Nothing

splitByList :: (Eq a) => [a] -> [a] -> [[a]]
splitByList delim txt@(x:xs) = impl' [[]] [] txt
  where
    -- return value without tail -> tail of return value -> rest -> return value
--    impl' :: [[a]] -> [a] -> [a] -> [[a]]
    impl' rtnval tail          [] = rtnval ++ [tail]
    impl' rtnval tail rest@(y:ys) = case deleteMatchedHead delim rest of
      Just newrest -> impl' (rtnval ++ [tail]) []            newrest
      Nothing      -> impl' rtnval             (tail ++ [y]) ys
      

instance StringClass Prelude.String where
  toString    a = a
  toText        = T.pack
  toByteString  = BC.pack
  (+++)     x y = Prelude.concat [x,y]
  split   x txt = splitByList x txt

instance StringClass T.Text where
  toString      = T.unpack
  toText      a = a
  toByteString  = BC.pack . toString
  (+++)     x y = T.concat [x,y]
  split         = T.splitOn

instance StringClass TL.Text where
  toString      = TL.unpack
  toTextL     a = a
  toByteString  = BC.pack . toString
  (+++)     x y = TL.concat [x,y]
  split         = TL.splitOn

instance StringClass B.ByteString where
  toString       = BC.unpack
  toText         = toText . toString
  toByteString a = a
  (+++)      x y = B.concat [x,y]
  split    x txt = Prelude.map B.pack $ splitByList (B.unpack x) (B.unpack txt)

instance StringClass BL.ByteString where
  toString        = toString . toByteString
  toText          = toText . BL.toStrict
  toByteStringL a = a
  (+++)       x y = BL.concat [x,y]
  split     x txt = Prelude.map BL.pack $ splitByList (BL.unpack x) (BL.unpack txt)

toJSON :: (StringClass s) => s -> Maybe Value
toJSON = decode . toByteStringL
