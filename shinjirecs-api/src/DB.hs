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
  ,module DB.Config
  ,module DB.Types
  ,Connection 
  ) where
import DB.Config
import DB.Migration
import DB.Persist
import DB.HDBC
import DB.Types(AdapterType(..))

import Data.ByteString -- bytestring
import Data.Pool(Pool) -- base
import Database.Persist.Sql(ConnectionPool)

type Connection = ConnectionPool
