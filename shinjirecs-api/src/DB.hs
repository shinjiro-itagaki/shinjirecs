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
  ,module DB.Persist
  ,module DB.Config
  ,module DB.Types
  ) where
import DB.Config
import DB.Migration
import DB.Persist
import DB.Types(AdapterType(..))

import Data.ByteString -- bytestring
import Data.Pool(Pool) -- base
import Database.HDBC.Sqlite3 (connectSqlite3)
import Database.HDBC
import Database.HDBC.Types
