{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Controllers.ChannelsController where
import Data.Bool(bool)
import Data.Maybe(maybe, fromMaybe, isJust, isNothing, fromJust) -- !!!
import Network.HTTP.Types (status200, status201, status400, status404, StdMethod(..))
-- import Control.Monad.IO.Class(MonadIO,liftIO) -- base
import qualified DB
-- import Model (find, saveE,saveR ,ToMaybeEntity(..))
-- import qualified Model as M
import Models.Channel
-- import Control.Monad.Reader(ReaderT) -- mtl
import Controller.Types(ActionWrapper(..), Action, Symbol, ControllerResponse(..))
import Controller(defaultControllerResponse,toBody)
import Class.String(StringClass(..))
import Data.Aeson(parseJSON,toJSON,ToJSON)
import Data.Aeson.Types(parseMaybe)
import Data.Text(Text)
import Network.Wai(Request)
import Data.Int(Int64)

-- data ChannelsController = ChannelsController { conn_ :: ConnectionPool }

{-
instance Controller ChannelsController where
  new                 = ChannelsController
  conn                = conn_
  beforeAction List c = return (True, c)
  beforeAction _    c = return (True, c)
-}
-- クラスに記載された関数を実行したら、インスタンスの候補が複数存在するとしてエラーになるので以下のように戻り値の型を明示した関数を作成した
-- toMaybeEntity' x = toMaybeEntity x :: Maybe (Entity DB.Channel)

toJsonText' :: (ToJSON a) => Maybe a -> Text
toJsonText' Nothing  = ""
toJsonText' (Just x) = case parseMaybe (parseJSON . toJSON) x of
                        Just x  -> x
                        Nothing -> ""

list :: Action () -- (() -> Connection -> StdMethod -> Request -> IO ControllerResponse)
list _ method conn req = do
  channels <- (DB.select $ DB.channelsTable conn) filters opts
  return $ defaultControllerResponse {
    body = toBody $ toJsonText' $ Just channels -- resText
  }
  where
    filters = [] -- :: [DB.Filter DB.Channel]
    opts    = [] -- :: [DB.SelectOpt DB.Channel]

get :: Action Int64
get id method conn req = do
  mchannel <- (DB.find $ DB.channelsTable conn) id
  return $ defaultControllerResponse {
    body = toBody $ toJsonText' mchannel -- resText
  }

modify :: Action Int64
modify id method conn req = do
  mchannel <- (DB.find $ DB.channelsTable conn) id
  return $ defaultControllerResponse {
    body = toBody $ toJsonText' mchannel -- resText
  }

create :: Action ()
create _ method conn req = do
  mchannel <- (DB.find $ DB.channelsTable conn) 1
  return $ defaultControllerResponse {
    body = toBody $ toJsonText' mchannel -- resText    
  }

destroy :: Action Int64
destroy id method conn req = do
--  x <- (DB.delete $ DB.channelsTable conn) id)
  return $ defaultControllerResponse {
    body = "" -- toBody $ toJsonText' mchannel -- resText    
  }
