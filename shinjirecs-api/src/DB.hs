{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
-- {-# LANGUAGE ScopedTypeVariables #-}

module DB (
  module DB.Migration
--  ,module DB.Persist
  ,module ORM
  ,module DB.Config
  ,module DB.Types
  ,Connection 
  ) where
import DB.Config
import DB.Migration
-- import qualified DB.Persist as ORM
import qualified DB.HDBC as ORM
import DB.Types(AdapterType(..))

import Data.ByteString -- bytestring
import Data.Pool(Pool) -- base
import Database.Persist.Sql(ConnectionPool)

type Connection = ORM.Connection__
