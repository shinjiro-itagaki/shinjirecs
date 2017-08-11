{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Controller.Types where
import Data.Int(Int64)
import Data.Map(Map(..), empty, fromList)
import Data.ByteString as B
import Data.ByteString.Lazy as L
import DB(Connection)
import Network.Wai (Request(..))
import Network.HTTP.Types (Status)

type ContentType = B.ByteString
type Body = L.ByteString

data ControllerResponse = MkControllerResponse {
  contentType :: ContentType
  ,body       :: Body
  ,status     :: Status
  }

type Symbol = String
type ParamGivenActionType = (Connection -> Request -> IO ControllerResponse)
type ActionType a = (a -> Connection -> Request -> IO ControllerResponse)

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

