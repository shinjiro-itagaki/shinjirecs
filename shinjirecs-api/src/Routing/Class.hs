{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Routing.Class where
import Data.Int(Int64)
import Data.Map(Map(..), empty, fromList)
import Data.ByteString(ByteString)
import Controller.Types(ActionWrapper(..), Action, ParamGivenAction)
import Class.String(StringClass,toByteString,toString)
import Network.HTTP.Types.Method(StdMethod(GET,POST,HEAD,PUT,DELETE,TRACE,CONNECT,OPTIONS,PATCH))

type Path = ByteString

toPath :: (StringClass a) => a -> Path
toPath = toByteString

type PathPattern = ByteString

toPathPattern :: (StringClass a) => a -> PathPattern
toPathPattern = toByteString

type RawPathParamKey = String
toRawPathParamKey :: (StringClass a) => a -> RawPathParamKey
toRawPathParamKey = toString

type RawPathParamVal = String
toRawPathParamVal :: (StringClass a) => a -> RawPathParamVal
toRawPathParamVal = toString

type RawPathParam = (RawPathParamKey, RawPathParamVal)
toRawPathParam :: (StringClass a, StringClass b) => (a,b) -> RawPathParam
toRawPathParam (x,y) = (toString x, toString y)

type RawPathParams = [RawPathParam]
toRawPathParams :: (StringClass a, StringClass b) => [(a,b)] -> RawPathParams
toRawPathParams = Prelude.map toRawPathParam

data RouteNotFoundError = PathNotFound | PathFoundButMethodUnmatch String | UnknownMethod deriving (Ord,Show)
instance Eq RouteNotFoundError where
  (==) PathNotFound PathNotFound = True
  (==) (PathFoundButMethodUnmatch _) (PathFoundButMethodUnmatch _) = True -- ignore msg
  (==) UnknownMethod UnknownMethod  = True
  (==) _ _ = False


data Route = MkRoute [StdMethod] PathPattern ActionWrapper

data RawPathParamsError = BadParamTypes [String] | BadRouteDefinition

data RoutingError = RouteNotFound RouteNotFoundError | BadPathParams RawPathParamsError

class PathParamList a where
  rawPathParamsToArgs :: RawPathParams -> Either RawPathParamsError a
  rawPathParamsToArgs others = Left BadRouteDefinition
  toActionWrapper :: Action a -> ActionWrapper

toParamGivenAction :: ActionWrapper -> RawPathParams -> Either RawPathParamsError ParamGivenAction
toParamGivenAction action params =
  case action of
    Action_N    f -> impl' f params -- (ActionType ()                     )
    Action_S    f -> impl' f params -- (ActionType String                 )
    Action_I    f -> impl' f params -- (ActionType Int64                    )
    Action_SMap f -> impl' f params -- (ActionType (Map String String)    )
    Action_IMap f -> impl' f params -- (ActionType (Map String Int64)       )
    Action_SS   f -> impl' f params -- (ActionType (String,String)        )
    Action_II   f -> impl' f params -- (ActionType (Int64,Int64)              )
    Action_IS   f -> impl' f params -- (ActionType (Int64,String)           )
    Action_SI   f -> impl' f params -- (ActionType (String,Int64)           )
    Action_III  f -> impl' f params -- (ActionType (Int64,Int64,Int64)          )
    Action_SII  f -> impl' f params -- (ActionType (String,Int64,Int64)       )
    Action_ISI  f -> impl' f params -- (ActionType (Int64,String,Int64)       )
    Action_IIS  f -> impl' f params -- (ActionType (Int64,Int64,String)       )
    Action_SSI  f -> impl' f params -- (ActionType (String,String,Int64)    )
    Action_ISS  f -> impl' f params -- (ActionType (Int64,String,String)    )
    Action_SIS  f -> impl' f params -- (ActionType (String,Int64,String)    )
    Action_SSS  f -> impl' f params -- (ActionType (String,String,String) )
  where
    impl' :: (PathParamList a) => Action a -> RawPathParams -> Either RawPathParamsError ParamGivenAction
    impl' f params = case rawPathParamsToArgs params of
                       Right args -> Right (f args)
                       Left x     -> Left x
    

rawPathParamsToArg' :: (Read a) => RawPathParams -> Either RawPathParamsError a
rawPathParamsToArg' (x:yy) = Right $ read $ snd x
rawPathParamsToArg' _      = Left BadRouteDefinition

rawPathParamsToArgsXX' :: (Read a, Read b) => RawPathParams -> Either RawPathParamsError (a,b)
rawPathParamsToArgsXX' (x0:x1:xx) = Right (read $ snd x0, read $ snd x1)
rawPathParamsToArgsXX' _          = Left BadRouteDefinition

rawPathParamsToArgsXXX' :: (Read a, Read b, Read c) => RawPathParams -> Either RawPathParamsError (a,b,c)
rawPathParamsToArgsXXX' (x0:x1:x2:xx) = Right (read $ snd x0, read $ snd x1, read $ snd $ x2)
rawPathParamsToArgsXXX' _             = Left BadRouteDefinition

rawPathParamsToArgsMap' :: (Read a) => RawPathParams -> Either RawPathParamsError (Map String a)
rawPathParamsToArgsMap' xs = Right $ fromList $ map (\(k,v) -> (k, read v)) xs

instance PathParamList () where
  rawPathParamsToArgs = rawPathParamsToArg'
  toActionWrapper = Action_N
  
instance PathParamList String where
  rawPathParamsToArgs = rawPathParamsToArg'
  toActionWrapper = Action_S

instance PathParamList Int64 where
  rawPathParamsToArgs = rawPathParamsToArg'
  toActionWrapper = Action_I

instance PathParamList (Map String String) where
  rawPathParamsToArgs = rawPathParamsToArgsMap'
  toActionWrapper = Action_SMap

instance PathParamList (Map String Int64) where
  rawPathParamsToArgs = rawPathParamsToArgsMap'
  toActionWrapper = Action_IMap

instance PathParamList (String ,String) where
  rawPathParamsToArgs = rawPathParamsToArgsXX'
  toActionWrapper = Action_SS
  
instance PathParamList (Int64 ,Int64) where
  rawPathParamsToArgs = rawPathParamsToArgsXX'
  toActionWrapper = Action_II
  
instance PathParamList (Int64 ,String) where
  rawPathParamsToArgs = rawPathParamsToArgsXX'
  toActionWrapper = Action_IS
  
instance PathParamList (String , Int64) where
  rawPathParamsToArgs = rawPathParamsToArgsXX'
  toActionWrapper = Action_SI
  
instance PathParamList (Int64 , Int64 , Int64) where
  rawPathParamsToArgs = rawPathParamsToArgsXXX'
  toActionWrapper = Action_III
  
instance PathParamList (String , Int64 , Int64) where
  rawPathParamsToArgs = rawPathParamsToArgsXXX'
  toActionWrapper = Action_SII
  
instance PathParamList (Int64,String,Int64) where
  rawPathParamsToArgs = rawPathParamsToArgsXXX'
  toActionWrapper = Action_ISI
  
instance PathParamList (Int64,Int64,String) where
  rawPathParamsToArgs = rawPathParamsToArgsXXX'
  toActionWrapper = Action_IIS
  
instance PathParamList (String,String,Int64) where
  rawPathParamsToArgs = rawPathParamsToArgsXXX'
  toActionWrapper = Action_SSI
  
instance PathParamList (Int64,String,String) where
  rawPathParamsToArgs = rawPathParamsToArgsXXX'
  toActionWrapper = Action_ISS
  
instance PathParamList (String,Int64,String) where
  rawPathParamsToArgs = rawPathParamsToArgsXXX'
  toActionWrapper = Action_SIS
  
instance PathParamList (String,String,String) where
  rawPathParamsToArgs = rawPathParamsToArgsXXX'
  toActionWrapper = Action_SSS
