{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Routing where
import Network.Wai (Request(..))

import Network.HTTP.Types.Method(StdMethod, parseMethod)
import Data.List(find)
import Data.ByteString(ByteString)
import Data.ByteString.Char8(split,null)

import Controller.Types(ParamGivenAction)

import Routing.Class(Route(MkRoute),Path,PathPattern,RawPathParam,RawPathParams,toParamGivenAction, RouteNotFoundError(PathNotFound,PathFoundButMethodUnmatch,UnknownMethod), RoutingError(RouteNotFound,BadPathParams))
import Routing.Map(routingMap)
import Class.String(toString, toByteString)
import Data.Maybe(fromJust,isJust)
import Data.Word(Word8)
import System.IO(putStrLn)

matchStdMethods :: Route -> StdMethod -> Bool
matchStdMethods (MkRoute methods _ _ ) method = elem method methods

matchPathElements :: [String] -> [String] -> Maybe RawPathParams -> Maybe RawPathParams
matchPathElements _      _      Nothing = Nothing -- protect error by fromJust
matchPathElements []     (y:ys) _       = Nothing -- mismatch count of elements
matchPathElements (x:xs) []     _       = Nothing -- mismatch count of elements
matchPathElements ptn@(x@(':':sym):xs) pathelems@(y:ys) params = matchPathElements xs ys $ Just $ (fromJust params) ++ [(sym, y)]
matchPathElements ptn@(x:xs) pathelems@(y:ys) params =
  if x == y
  then matchPathElements xs ys params -- match and do next
  else Nothing -- unmatch
matchPathElements [] [] x = x -- finished
-- matchPathElements _ _ _ = Nothing -- error

pathToPieces :: Path -> [String]
pathToPieces path = Prelude.map toString $ filter (not . Data.ByteString.Char8.null) $ Data.ByteString.Char8.split '/' $ toByteString path

getMaybeRawPathParamsFromPatternAndPath :: PathPattern -> Path -> Maybe RawPathParams
getMaybeRawPathParamsFromPatternAndPath ptn path =
  matchPathElements ptn_pieces' path_pieces' (Just [])
  where
    ptn_pieces'  = pathToPieces ptn
    path_pieces' = pathToPieces path

-- Nothing means unmatch path
getMaybeRawPathParams :: Route -> Path -> Maybe RawPathParams
getMaybeRawPathParams (MkRoute _ ptn _ ) path = getMaybeRawPathParamsFromPatternAndPath ptn path

matchPath :: Path -> Route -> Bool
matchPath p r = isJust $ getMaybeRawPathParams r p
    
getRawPathParams :: Route -> Path -> RawPathParams
getRawPathParams r p = case getMaybeRawPathParams r p of
  Just params -> params
  Nothing     -> []

findPathMatchedRoutes :: Path -> [Route] -> [(Route, RawPathParams)]
findPathMatchedRoutes path [] = []
findPathMatchedRoutes path (r:rs) = case getMaybeRawPathParams r path of
  Just p  -> [(r, p)] ++ findPathMatchedRoutes path rs
  Nothing -> findPathMatchedRoutes path rs

findMethodMatchedRoute :: StdMethod -> [(Route, RawPathParams)] -> Maybe (Route, RawPathParams)
findMethodMatchedRoute _ [] = Nothing
findMethodMatchedRoute stdmethod ((route@(MkRoute methods ptn actionWrapper),params):xs) =
  if matchStdMethods route stdmethod
  then Just (route, params)
  else findMethodMatchedRoute stdmethod xs

findRoute :: StdMethod -> Path -> Either RouteNotFoundError (Route, RawPathParams)
findRoute stdmethod path =
  case findPathMatchedRoutes path routingMap of
    [] -> Left PathNotFound
    xs -> case find (\(route, params) -> matchStdMethods route stdmethod) xs of
      Just x  -> Right x
      Nothing ->
        Left $ PathFoundButMethodUnmatch $ Prelude.concat $ Prelude.map (showRoute' . fst) xs
  where
    showRoute' :: Route -> String
    showRoute' (MkRoute methods ptn _ ) = Prelude.concat [show methods, " ", toString ptn]

run :: Request -> Either RoutingError (StdMethod, ParamGivenAction, Route)
run req =
  case parseMethod $ requestMethod req of
    Left _ -> Left $ RouteNotFound UnknownMethod
    Right stdmethod -> case findRoute stdmethod (rawPathInfo req) of
      Right (route@(MkRoute methods ptn actionWrapper), rawPathParams) ->
        case toParamGivenAction actionWrapper rawPathParams of
          Left x -> Left $ BadPathParams x
          Right action -> Right (stdmethod, action, route)
      Left x -> Left $ RouteNotFound x
