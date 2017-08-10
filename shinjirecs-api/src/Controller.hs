{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Controller where
import Data.Aeson(ToJSON(..))
import Data.ByteString as B
import Data.ByteString.Lazy as L
import Network.Wai (Request(..))
import Network.HTTP.Types (Status, status200, status201, status400, status404, StdMethod(..))
import DB(Connection)
import Class.Castable(Castable(..))
import Data.Map(Map(..), empty, fromList)
import Data.Int(Int64)

type ContentType = B.ByteString
type Body = L.ByteString

data ControllerResponse = MkControllerResponse {
  contentType :: ContentType
  ,body       :: Body
  ,status     :: Status
  }
  
defaultControllerResponse :: ControllerResponse
defaultControllerResponse = MkControllerResponse {
  contentType = "application/json"
  ,body       = ""
  ,status     = status200
  }
  
type Symbol = String
type ActionType a = (Connection -> Request -> a -> IO ControllerResponse)

data Action = Action_N    (ActionType ()                     )
            | Action_S    (ActionType String                 )
            | Action_I    (ActionType Int64                    )
            | Action_SMap (ActionType (Map String String)    )
            | Action_IMap (ActionType (Map String Int64)       )
            | Action_SS   (ActionType (String,String)        )
            | Action_II   (ActionType (Int64,Int64)              )
            | Action_IS   (ActionType (Int64,String)           )
            | Action_SI   (ActionType (String,Int64)           )
            | Action_III  (ActionType (Int64,Int64,Int64)          )
            | Action_SII  (ActionType (String,Int64,Int64)       )
            | Action_ISI  (ActionType (Int64,String,Int64)       )
            | Action_IIS  (ActionType (Int64,Int64,String)       )
            | Action_SSI  (ActionType (String,String,Int64)    )
            | Action_ISS  (ActionType (Int64,String,String)    )
            | Action_SIS  (ActionType (String,Int64,String)    )
            | Action_SSS  (ActionType (String,String,String) )
