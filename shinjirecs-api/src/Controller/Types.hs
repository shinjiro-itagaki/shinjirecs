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
import Network.HTTP.Types.Method(StdMethod)

type ContentType = B.ByteString
type Body = L.ByteString

data ControllerResponse = MkControllerResponse {
  contentType :: ContentType
  ,body       :: Body
  ,status     :: Status
  }

type ParamGivenAction = (StdMethod -> Connection -> Request -> IO ControllerResponse)
type Action a    = (a -> ParamGivenAction)

data ActionWrapper = Action_N    (Action ())
                   | Action_S    (Action String)
                   | Action_I    (Action Int64)
                   | Action_SMap (Action (Map String String))
                   | Action_IMap (Action (Map String Int64))
                   | Action_SS   (Action (String,String))
                   | Action_II   (Action (Int64,Int64))
                   | Action_IS   (Action (Int64,String))
                   | Action_SI   (Action (String,Int64))
                   | Action_III  (Action (Int64,Int64,Int64))
                   | Action_SII  (Action (String,Int64,Int64))
                   | Action_ISI  (Action (Int64,String,Int64))
                   | Action_IIS  (Action (Int64,Int64,String))
                   | Action_SSI  (Action (String,String,Int64))
                   | Action_ISS  (Action (Int64,String,String))
                   | Action_SIS  (Action (String,Int64,String))
                   | Action_SSS  (Action (String,String,String))

