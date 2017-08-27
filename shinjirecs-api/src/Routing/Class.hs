{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Routing.Class where
import Data.Int(Int64)
import Data.Map(Map(..), empty, fromList)
import Data.ByteString(ByteString)
import Data.Maybe(catMaybes)
import Controller.Types(ActionWrapper(..), Action, ParamGivenAction)
import Class.String(StringClass,toByteString,toString)
import Network.HTTP.Types.Method(StdMethod(GET,POST,HEAD,PUT,DELETE,TRACE,CONNECT,OPTIONS,PATCH))
import Text.Read(readMaybe)
import Model(ModelClass)

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

instance Show Route where
  show (MkRoute stdmethods pathpattern _) = (show stdmethods) ++ " " ++ (show pathpattern)


data RawPathParamsError = BadParamTypes [String] | BadRouteDefinition | ResourceNotFound Int64

data RoutingError = RouteNotFound RouteNotFoundError | BadPathParams RawPathParamsError

class PathParamList a where
  rawPathParamsToArgs :: RawPathParams -> Either RawPathParamsError a
  rawPathParamsToArgs others = Left BadRouteDefinition
  toActionWrapper :: Action a -> ActionWrapper

toParamGivenAction :: ActionWrapper -> RawPathParams -> Either RawPathParamsError ParamGivenAction
toParamGivenAction action params =
  case action of
    Action_N    f -> impl' f params -- (Action ()                     )
    Action_S    f -> impl' f params -- (Action String                 )
    Action_I    f -> impl' f params -- (Action Int64                    )
    Action_SMap f -> impl' f params -- (Action (Map String String)    )
    Action_IMap f -> impl' f params -- (Action (Map String Int64)       )
    Action_SS   f -> impl' f params -- (Action (String,String)        )
    Action_II   f -> impl' f params -- (Action (Int64,Int64)              )
    Action_IS   f -> impl' f params -- (Action (Int64,String)           )
    Action_SI   f -> impl' f params -- (Action (String,Int64)           )
    Action_III  f -> impl' f params -- (Action (Int64,Int64,Int64)          )
    Action_SII  f -> impl' f params -- (Action (String,Int64,Int64)       )
    Action_ISI  f -> impl' f params -- (Action (Int64,String,Int64)       )
    Action_IIS  f -> impl' f params -- (Action (Int64,Int64,String)       )
    Action_SSI  f -> impl' f params -- (Action (String,String,Int64)    )
    Action_ISS  f -> impl' f params -- (Action (Int64,String,String)    )
    Action_SIS  f -> impl' f params -- (Action (String,Int64,String)    )
    Action_SSS  f -> impl' f params -- (Action (String,String,String) )
  where
    impl' :: (PathParamList a) => Action a -> RawPathParams -> Either RawPathParamsError ParamGivenAction
    impl' f params = case rawPathParamsToArgs params of
                       Right args -> Right $ f args
                       Left x     -> Left x
    

rawPathParamsToArg' :: (Read a) => RawPathParams -> Either RawPathParamsError a
rawPathParamsToArg' (x@(k,v):yy) = case readMaybe v of
                               Just x -> Right x
                               Nothing -> Left (BadParamTypes [k])
rawPathParamsToArg' _ = Left BadRouteDefinition

rawPathParamsToRecord' :: (Read a) => RawPathParams -> Either RawPathParamsError a
rawPathParamsToRecord' (x@(k,v):yy) = case readMaybe v of
                               Just x -> Right x
                               Nothing -> Left (BadParamTypes [k])

getReadMaybeFailedKey' :: (String,Maybe a) -> Maybe String
getReadMaybeFailedKey' (k, (Just _)) = Nothing
getReadMaybeFailedKey' (k,  Nothing) = Just k

rawPathParamsToArgsXX' :: (Read a, Read b) => RawPathParams -> Either RawPathParamsError (a,b)
rawPathParamsToArgsXX' ((k0,v0):(k1,v1):xx) =
  case (readMaybe v0, readMaybe v1) of
    (Just arg0, Just arg1) -> Right (arg0, arg1)
    (marg0    , marg1    ) -> Left $ BadParamTypes $ catMaybes
                              $ [ getReadMaybeFailedKey' (k0, marg0)
                                , getReadMaybeFailedKey' (k1, marg1)
                                ]
rawPathParamsToArgsXX' _ = Left BadRouteDefinition

rawPathParamsToArgsXXX' :: (Read a, Read b, Read c) => RawPathParams -> Either RawPathParamsError (a,b,c)
rawPathParamsToArgsXXX' ((k0,v0):(k1,v1):(k2,v2):xx) =
  case (readMaybe v0, readMaybe v1, readMaybe v2) of
    (Just arg0, Just arg1, Just arg2) -> Right (arg0, arg1, arg2)
    (marg0    , marg1    , marg2    ) -> Left $ BadParamTypes $ catMaybes
                                         $ [ getReadMaybeFailedKey' (k0, marg0)
                                           , getReadMaybeFailedKey' (k1, marg1)
                                           , getReadMaybeFailedKey' (k2, marg2)
                                           ]
rawPathParamsToArgsXXX' _ = Left BadRouteDefinition

rawPathParamsToArgsMap' :: (Read a) => RawPathParams -> Either RawPathParamsError (Map String a)
rawPathParamsToArgsMap' xs = impl' xs [] []
  where
    impl' :: (Read a) => RawPathParams -> [String] -> [(String, a)] -> Either RawPathParamsError (Map String a)
    impl' []               []          rightParams' = Right $ fromList rightParams'
    impl' []               failedKeys' rightParams' = Left $ BadParamTypes failedKeys'
    impl' (p'@(k',v'):ps') failedKeys' rightParams' = case readMaybe v' of
      Just arg' -> impl' ps'  failedKeys'          (rightParams' ++ [(k',arg')])
      Nothing   -> impl' ps' (failedKeys' ++ [k']) rightParams'

instance PathParamList () where
  rawPathParamsToArgs _ = Right ()
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
