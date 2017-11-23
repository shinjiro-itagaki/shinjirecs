{-# LANGUAGE OverloadedStrings #-}
module Models.Program where
import DB(Program)
import Model(ModelClass(..))

instance ModelClass DB.Program
