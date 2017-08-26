{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Controllers.ProgramsController where
import qualified DB
import Models.Program
import Controller.Types(Action, ControllerResponse(body))
import Controller(defaultControllerResponse,ToBody(toBody), getRecords, getRecord, destroyRecord, fromRequest, modifyCommon,createCommon,defaultListAction, defaultGetAction, defaultModifyAction, defaultCreateAction, defaultDestroyAction, mkDefaultResource)
import Data.Int(Int64)

tableGetter conn = DB.readTable conn :: DB.Table DB.Program

resource = mkDefaultResource tableGetter

{- 
list :: Action () -- (() -> Connection -> StdMethod -> Request -> IO ControllerResponse)
list = defaultListAction getTable

get :: Action Int64
get = defaultGetAction getTable

modify :: Action Int64
modify = defaultModifyAction getTable

create :: Action ()
create = defaultCreateAction getTable
    
destroy :: Action Int64
destroy = defaultDestroyAction getTable
-}
