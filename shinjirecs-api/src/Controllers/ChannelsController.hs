{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Controllers.ChannelsController where
import qualified DB
import Models.Channel
import Controller.Types(Action, ControllerResponse(body))
import Controller(defaultControllerResponse,ToBody(toBody), getRecords, getRecord, destroyRecord)
import Data.Int(Int64)

list :: Action () -- (() -> Connection -> StdMethod -> Request -> IO ControllerResponse)
list _ method conn req = getRecords filters' opts' table' (map snd)
  where
    table' = DB.readTable conn :: DB.Table DB.Channel
    filters' = [] -- :: [DB.Filter DB.Channel]
    opts'    = [] -- :: [DB.SelectOpt DB.Channel]

get :: Action Int64
get id method conn req = getRecord id table' snd
  where
    table' = DB.readTable conn :: DB.Table DB.Channel

modify :: Action Int64
modify id method conn req = do
  mchannel <- (DB.find $ DB.channelsTable conn) id
  return $ defaultControllerResponse {
    body = toBody mchannel -- resText
    }

create :: Action ()
create _ method conn req = do
  mchannel <- (DB.find $ DB.channelsTable conn) (1 :: Int64)
  return $ defaultControllerResponse {
    body = toBody mchannel -- resText
    }
    
destroy :: Action Int64
destroy id method conn req = destroyRecord id $ (DB.readTable conn :: DB.Table DB.Channel)
