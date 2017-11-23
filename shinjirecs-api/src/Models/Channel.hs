{-# LANGUAGE OverloadedStrings #-}
module Models.Channel where
import DB(Channel)
import Model(ModelClass(..))

instance ModelClass DB.Channel
