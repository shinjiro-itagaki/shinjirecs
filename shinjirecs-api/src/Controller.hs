{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Controller where
import Data.Aeson(ToJSON(..), Result(Error,Success),FromJSON, fromJSON,decode, parseJSON,toJSON,ToJSON,Value(Object), encode)
import Data.Aeson.Types(parseMaybe)
import Network.Wai (Request(..))
import Network.HTTP.Types (Status, status200, status201, status400, status404, StdMethod(..))
import qualified DB
import Class.Castable(Castable(..))
import Class.String(StringClass(toByteStringL,toTextL,(+++), toText),toJSON)
import Routing.Class(RawPathParams, PathParamList(..))
import Controller.Types(ControllerResponse(..), ActionWrapper(..), Action, Body, ParamGivenAction)
import qualified Data.Text.Lazy as TL
import Data.Int(Int64)
import Data.Maybe(isJust)
import Data.Text(Text)
import Model(ModelClass, SaveResult(SaveSuccess, SaveFailed, SaveCanceled, Rollbacked), create, CreateResult, modify, ModifyResult)
import Routing.Types(Resource(Resource,listAction,getAction,modifyAction,createAction,destroyAction))

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
  toBody = encode

fromRequest :: (FromJSON a) => Request -> IO (Either ControllerResponse a)
fromRequest req = do
  body <- requestBody req
  return $ case Class.String.toJSON body of
    Nothing -> Left $ responseBadRequest body -- parse error
    Just x -> case fromJSON x of
      Error   _ -> Left $ responseBadRequest body -- maybe lack some attributes
      Success y -> Right y

responseBadRequest :: (StringClass s) => s -> ControllerResponse
responseBadRequest txt = defaultControllerResponse {
  body    = toBody $ (toText txt) +++ (" is not found" :: Text)
  ,status = status400
  }

doIfRecordFound :: Int64 -> DB.Table m -> (DB.Entity m -> IO ControllerResponse) -> IO ControllerResponse
doIfRecordFound id t f = do
  mrec <- DB.find t id
  case mrec of
    Nothing -> responseRecordNotFound id
    Just  x -> f x

doIfValidInputJSON :: (FromJSON m) => Request -> (m -> IO ControllerResponse) -> IO ControllerResponse
doIfValidInputJSON req f = do
  erec <- fromRequest req
  case erec of
    Left res -> return res
    Right  x -> f x

doIfModifiable :: (ModelClass m) => Int64 -> DB.Table m -> Request -> (DB.Entity m -> IO ControllerResponse) -> IO ControllerResponse
doIfModifiable id t req f = doIfRecordFound id t $ ifRecordFound'
  where
    ifRecordFound' e@(k,v) = doIfValidInputJSON req $ (\inputV -> f (k,inputV) )

doIfCreatable :: (ModelClass m) => DB.Table m -> Request -> (m -> IO ControllerResponse) -> IO ControllerResponse
doIfCreatable t req f = doIfValidInputJSON req f

modifyCommon :: (ModelClass m, FromJSON m, ToJSON m, ToJSON (DB.Entity m)) => Int64 -> DB.Table m -> Request -> IO ControllerResponse
modifyCommon id t req = doIfModifiable id t req impl'
  where
    -- impl' :: DB.Entity m -> IO ControllerResponse
    impl' e = modify t e >>= return . saveResultToControllerResponse

createCommon :: (ModelClass m) => DB.Table m -> Request -> IO ControllerResponse
createCommon t req = doIfCreatable t req impl'
  where
    -- impl' :: m -> IO ControllerResponse
    impl' v = create t v >>= return . saveResultToControllerResponse

saveResultToControllerResponse :: (ModelClass record) => SaveResult record rore typ -> ControllerResponse
saveResultToControllerResponse (SaveSuccess e) = responseSaved e
saveResultToControllerResponse (SaveFailed e results committed) = responseBadRequest ("failed" :: String)
saveResultToControllerResponse (SaveCanceled pos e) = responseBadRequest ("canceled" :: String)
saveResultToControllerResponse (Rollbacked e)       = responseBadRequest ("rollbacked" :: String)

responseSaved :: (ModelClass record) => DB.Entity record -> ControllerResponse
responseSaved e = defaultControllerResponse {
  body    = toBody e
  ,status = status201
  }
  
responseRecordNotFound :: Int64 -> IO ControllerResponse
responseRecordNotFound id = return defaultControllerResponse {
  body    = toBody $ "id " ++ (show id) ++ "was not found"
  ,status = status404
  }

getRecords :: (ToJSON (DB.Entity record)) => [DB.Filter record] -> [DB.SelectOpt record] -> DB.Table record -> IO ControllerResponse
getRecords filters opts table = do
  records <- DB.select table filters opts
  return $ defaultControllerResponse {
    body = toBody records
    }  

getRecord :: (ToJSON (DB.Entity record)) => Int64 -> DB.Table record -> IO ControllerResponse
getRecord id table = do
  mrec <- find' id
  case mrec of
    Nothing -> responseRecordNotFound id -- record not found
    Just x  -> return $ defaultControllerResponse {
    body = toBody x
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
  

defaultListAction :: (ModelClass record) => (DB.Connection -> DB.Table record) -> Action ()
defaultListAction f _ method conn req = getRecords filters' opts' (f conn)
  where
    filters' = [] -- :: [DB.Filter DB.Channel]
    opts'    = [] -- :: [DB.SelectOpt DB.Channel]
    
defaultGetAction :: (ModelClass record) => (DB.Connection -> DB.Table record) -> Action Int64
defaultGetAction f id method conn req = getRecord id (f conn)

defaultModifyAction :: (ModelClass record) => (DB.Connection -> DB.Table record) -> Action Int64
defaultModifyAction f id method conn req = modifyCommon id (f conn) req
    
defaultCreateAction :: (ModelClass record) => (DB.Connection -> DB.Table record) -> Action ()
defaultCreateAction f _ method conn req = createCommon (f conn) req

defaultDestroyAction :: (ModelClass record) => (DB.Connection -> DB.Table record) -> Action Int64
defaultDestroyAction f id method conn req = destroyRecord id (f conn)

mkDefaultResource :: (ModelClass record) => (DB.Connection -> DB.Table record) -> Resource
mkDefaultResource tg = Resource {
  listAction     = defaultListAction    tg
  ,getAction     = defaultGetAction     tg
  ,modifyAction  = defaultModifyAction  tg
  ,createAction  = defaultCreateAction  tg
  ,destroyAction = defaultDestroyAction tg
  }
