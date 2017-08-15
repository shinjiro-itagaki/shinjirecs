{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Controller where
import Data.Aeson(ToJSON(..), Result(Error,Success),fromJSON)
import Network.Wai (Request(..))
import Network.HTTP.Types (Status, status200, status201, status400, status404, StdMethod(..))
import qualified DB
import Class.Castable(Castable(..))
import Class.String(StringClass(toByteStringL,toTextL))
import Routing.Class(RawPathParams, PathParamList(..))
import Controller.Types(ControllerResponse(..), ActionWrapper(..), Action, Body, ParamGivenAction)
import Data.Aeson(parseJSON,toJSON,ToJSON)
import Data.Aeson.Types(parseMaybe)
import qualified Data.Text.Lazy as TL
import Data.Int(Int64)
import Data.Maybe(isJust)

defaultControllerResponse :: ControllerResponse
defaultControllerResponse = MkControllerResponse {
  contentType = "application/json"
  ,body       = ""
  ,status     = status200
  }

toBodyFromTextL :: TL.Text -> Body
toBodyFromTextL = toByteStringL

class ToBody a where
  toBody :: a -> Body

instance (ToJSON a) => ToBody a where
  toBody x = toBodyFromTextL $ case (fromJSON $ toJSON x) :: Result TL.Text of
    Error msg -> toTextL msg
    Success x -> x

instance (ToJSON a) => ToBody (DB.Entity a) where
  toBody x = toBody $ snd x

responseRecordNotFound :: Int64 -> IO ControllerResponse
responseRecordNotFound id = return defaultControllerResponse {
  body    = toBody $ "id " ++ (show id) ++ "was not found"
  ,status = status404
  }

getRecords :: (ToBody t) => [DB.Filter record] -> [DB.SelectOpt record] -> DB.Table record -> ([DB.Entity record] -> t) -> IO ControllerResponse
getRecords filters opts table f = do
  records <- DB.select table filters opts
  return $ defaultControllerResponse {
    body = toBody $ f records
    }  

getRecord :: (ToBody t) => Int64 -> DB.Table record -> (DB.Entity record -> t) -> IO ControllerResponse
getRecord id table f = do
  mrec <- find' id
  case mrec of
    Nothing -> responseRecordNotFound id -- record not found
    Just x  -> return $ defaultControllerResponse {
    body = toBody $ f x
    }
  where
    find' = DB.find table

destroyRecord :: Int64 -> DB.Table record -> IO ControllerResponse
destroyRecord id table = do
  mrec <- find' id
  case mrec of
    Nothing -> responseRecordNotFound id -- record not found
    Just (key,_) -> do
      delete' key
      mrec2 <- find' id
      return $ defaultControllerResponse {
        body = toBody $ isJust mrec2
        }  
  where
    delete' = DB.delete table
    find'   = DB.find table
