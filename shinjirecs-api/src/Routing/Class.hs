{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Routing.Class where
import Data.Int(Int64)
import Data.Map(Map(..), empty, fromList)
import Data.ByteString(ByteString)
import Controller(Action(..), ActionType)

type Path = ByteString
type PathPattern = ByteString
type RawPathParamKey = String
type RawPathParamVal = String
type RawPathParam    = (RawPathParamKey, RawPathParamVal)
type RawPathParams   = [RawPathParam]



class PathParamList a where
  rawPathParamsToArgs :: RawPathParams -> Either RawPathParams a
  rawPathParamsToArgs others = Left others
  toAction :: ActionType a -> Action



rawPathParamsToArg' :: (Read a) => RawPathParams -> Either RawPathParams a
rawPathParamsToArg' (x:yy) = Right $ read $ snd x
rawPathParamsToArg' others = Left others

rawPathParamsToArgsXX' :: (Read a, Read b) => RawPathParams -> Either RawPathParams (a,b)
rawPathParamsToArgsXX' (x0:x1:xx) = Right (read $ snd x0, read $ snd x1)
rawPathParamsToArgsXX' others = Left others

rawPathParamsToArgsXXX' :: (Read a, Read b, Read c) => RawPathParams -> Either RawPathParams (a,b,c)
rawPathParamsToArgsXXX' (x0:x1:x2:xx) = Right (read $ snd x0, read $ snd x1, read $ snd $ x2)
rawPathParamsToArgsXXX' others = Left others

instance PathParamList () where
  rawPathParamsToArgs = rawPathParamsToArg'
  toAction = Action_N
  
instance PathParamList String where
  rawPathParamsToArgs = rawPathParamsToArg'
  toAction = Action_S

instance PathParamList Int64 where
  rawPathParamsToArgs = rawPathParamsToArg'
  toAction = Action_I

instance PathParamList (Map String String) where
  rawPathParamsToArgs = Right . fromList
  toAction = Action_SMap

instance PathParamList (Map String Int64) where
  rawPathParamsToArgs = Right . fromList . map (\(k,v) -> (k, read v))
  toAction = Action_IMap

instance PathParamList (String ,String) where
  rawPathParamsToArgs = rawPathParamsToArgsXX'
  toAction = Action_SS
  
instance PathParamList (Int64 ,Int64) where
  rawPathParamsToArgs = rawPathParamsToArgsXX'
  toAction = Action_II
  
instance PathParamList (Int64 ,String) where
  rawPathParamsToArgs = rawPathParamsToArgsXX'
  toAction = Action_IS
  
instance PathParamList (String , Int64) where
  rawPathParamsToArgs = rawPathParamsToArgsXX'
  toAction = Action_SI
  
instance PathParamList (Int64 , Int64 , Int64) where
  rawPathParamsToArgs = rawPathParamsToArgsXXX'
  toAction = Action_III
  
instance PathParamList (String , Int64 , Int64) where
  rawPathParamsToArgs = rawPathParamsToArgsXXX'
  toAction = Action_SII
  
instance PathParamList (Int64,String,Int64) where
  rawPathParamsToArgs = rawPathParamsToArgsXXX'
  toAction = Action_ISI
  
instance PathParamList (Int64,Int64,String) where
  rawPathParamsToArgs = rawPathParamsToArgsXXX'
  toAction = Action_IIS
  
instance PathParamList (String,String,Int64) where
  rawPathParamsToArgs = rawPathParamsToArgsXXX'
  toAction = Action_SSI  
  
instance PathParamList (Int64,String,String) where
  rawPathParamsToArgs = rawPathParamsToArgsXXX'
  toAction = Action_ISS  
  
instance PathParamList (String,Int64,String) where
  rawPathParamsToArgs = rawPathParamsToArgsXXX'
  toAction = Action_SIS
  
instance PathParamList (String,String,String) where
  rawPathParamsToArgs = rawPathParamsToArgsXXX'
  toAction = Action_SSS  
