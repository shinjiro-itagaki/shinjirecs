{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- {-# LANGUAGE TypeFamilies, FlexibleContexts, GADTs #-}

module DB where
-- import qualified Database.Persist.Sqlite as P
import Control.Monad.IO.Class(liftIO,MonadIO) -- base
import Data.Text(Text) -- text
import Data.Time -- time
import Database.Persist -- persistent
import Database.Persist.Types -- persistent
import Database.Persist.Quasi (lowerCaseSettings, PersistSettings) -- persistent
import Database.Persist.Sql (ConnectionPool, runSqlPool, SqlPersistT) -- persistent
import Database.Persist.TH (share, mkMigrate, mkPersist, sqlSettings, persistFileWith) -- persistent-template

import Database.Persist.MySQL (createMySQLPool) -- persistent-mysql
import Database.Persist.Sqlite (createSqlitePool) -- persistent-sqlite
import Database.Persist.Postgresql (createPostgresqlPool) -- persistent-postgresql

data AdapterType = MySQL | PostgreSQL | SQLite3 | Unsupported deriving Show

-- findAdapter :: AdapterType -> Maybe

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
  $(persistFileWith lowerCaseSettings "config/models")

runDB :: MonadIO m => ConnectionPool -> SqlPersistT IO a -> m a
runDB p action = liftIO $ runSqlPool action p

