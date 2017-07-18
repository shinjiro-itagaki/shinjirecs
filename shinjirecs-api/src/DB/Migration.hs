{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
-- {-# LANGUAGE ScopedTypeVariables #-}

module DB.Migration where
import Database.Persist.TH (share, mkMigrate, mkPersist, sqlSettings, persistFileWith, derivePersistField) -- persistent-template
import Database.Persist.Quasi (lowerCaseSettings, PersistSettings) -- persistent
import DB.Status(ReservationState(..))
import DB.Types(ChannelType(..))
import Data.Time -- time
import Data.ByteString -- bytestring
import Data.Word -- base
import Data.Text (Text,pack) -- text
import qualified Data.Text as Text --text
-- make following datas
-- Channel
-- Reservation
-- Program
-- ... and others
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
  $(persistFileWith lowerCaseSettings "config/models")
