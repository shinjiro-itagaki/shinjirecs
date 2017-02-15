{-# LANGUAGE OverloadedStrings #-}
module Models.Channel where
import DB(Channel)
import Model(ActiveRecord(..))
instance ActiveRecord Channel where
  beforeSave _ = Just

-- instance UniqueReplaceableModel Channel
-- instance CascadeDeletableModel Channel


