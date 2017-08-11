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
  path2args :: RawPathParams -> Either RawPathParams a
  path2args others = Left others
  toAction :: ActionType a -> Action



path2arg' :: (Read a) => RawPathParams -> Either RawPathParams a
path2arg' (x:yy) = Right $ read $ snd x
path2arg' others = Left others

path2argsXX' :: (Read a, Read b) => RawPathParams -> Either RawPathParams (a,b)
path2argsXX' (x0:x1:xx) = Right (read $ snd x0, read $ snd x1)
path2argsXX' others = Left others

path2argsXXX' :: (Read a, Read b, Read c) => RawPathParams -> Either RawPathParams (a,b,c)
path2argsXXX' (x0:x1:x2:xx) = Right (read $ snd x0, read $ snd x1, read $ snd $ x2)
path2argsXXX' others = Left others

instance PathParamList () where
  path2args = path2arg'
  toAction = Action_N
  
instance PathParamList String where
  path2args = path2arg'
  toAction = Action_S

instance PathParamList Int64 where
  path2args = path2arg'
  toAction = Action_I

instance PathParamList (Map String String) where
  path2args = Right . fromList
  toAction = Action_SMap

instance PathParamList (Map String Int64) where
  path2args = Right . fromList . map (\(k,v) -> (k, read v))
  toAction = Action_IMap

instance PathParamList (String ,String) where
  path2args = path2argsXX'
  toAction = Action_SS
  
instance PathParamList (Int64 ,Int64) where
  path2args = path2argsXX'
  toAction = Action_II
  
instance PathParamList (Int64 ,String) where
  path2args = path2argsXX'
  toAction = Action_IS
  
instance PathParamList (String , Int64) where
  path2args = path2argsXX'
  toAction = Action_SI
  
instance PathParamList (Int64 , Int64 , Int64) where
  path2args = path2argsXXX'
  toAction = Action_III
  
instance PathParamList (String , Int64 , Int64) where
  path2args = path2argsXXX'
  toAction = Action_SII
  
instance PathParamList (Int64,String,Int64) where
  path2args = path2argsXXX'
  toAction = Action_ISI
  
instance PathParamList (Int64,Int64,String) where
  path2args = path2argsXXX'
  toAction = Action_IIS
  
instance PathParamList (String,String,Int64) where
  path2args = path2argsXXX'
  toAction = Action_SSI  
  
instance PathParamList (Int64,String,String) where
  path2args = path2argsXXX'
  toAction = Action_ISS  
  
instance PathParamList (String,Int64,String) where
  path2args = path2argsXXX'
  toAction = Action_SIS
  
instance PathParamList (String,String,String) where
  path2args = path2argsXXX'
  toAction = Action_SSS  
