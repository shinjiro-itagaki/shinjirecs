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
  module DB.Config
  ,module DB.Types
  ,module DB
  , ORM.Reservation(..)
  , ORM.Channel(..)
  , ORM.Program(..)
  ) where
import DB.Config
import qualified DB.Persist as ORM
-- import qualified DB.HDBC as ORM
import DB.Types(AdapterType(..))

import Data.ByteString -- bytestring
import Data.Pool(Pool) -- base
import Database.Persist.Sql(ConnectionPool)

type Connection = ORM.Connection__

migrate :: DB.Config.Config -> IO ()
migrate = ORM.migrate
