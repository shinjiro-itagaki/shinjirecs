{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Controllers.ChannelsController where
import qualified DB
import Models.Channel
import Controller.Types(Action, ControllerResponse(body))
import Controller(defaultControllerResponse,ToBody(toBody), getRecords, getRecord, destroyRecord, fromRequest, modifyCommon,createCommon,defaultListAction, defaultGetAction, defaultModifyAction, defaultCreateAction, defaultDestroyAction, mkDefaultResource)
import Data.Int(Int64)

tableGetter conn = DB.readTable conn :: DB.Table DB.Channel

resource = mkDefaultResource tableGetter

{-
list :: Action () -- (() -> Connection -> StdMethod -> Request -> IO ControllerResponse)
list = defaultListAction tableGetter

get :: Action Int64
get = defaultGetAction tableGetter

modify :: Action Int64
modify = defaultModifyAction tableGetter

create :: Action ()
create = defaultCreateAction tableGetter
    
destroy :: Action Int64
destroy = defaultDestroyAction tableGetter
-}
