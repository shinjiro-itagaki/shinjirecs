{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Routing.Types where
import Controller.Types(Action)
import Data.Int(Int64)
data Resource = Resource {
  listAction :: Action ()
  ,getAction :: Action Int64
  ,modifyAction :: Action Int64
  ,createAction :: Action ()
  ,destroyAction :: Action Int64
  }
