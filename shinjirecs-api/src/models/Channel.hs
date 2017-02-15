{-# LANGUAGE OverloadedStrings #-}
module Models.Channel where
import DB(Channel)
import Model(ActiveRecord(..))
instance ActiveRecord Channel where
  beforeSave _ = return . Just

-- instance UniqueReplaceableModel Channel
-- instance CascadeDeletableModel Channel


