{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Config.Class where
import Data.Text (Text, pack, unpack)
import Class.Castable(Castable(from))
import Data.List.Extra (lower) -- extra

data Env = Production | Development | Test deriving Show

class ConfigClass a where
  defaultConfig :: IO a

instance Castable String Env where
  from "production" = Production
  from "test"       = Test
  from _            = Development

envToText :: Env -> Data.Text.Text
envToText = Data.Text.pack . lower . show

(|||) :: Maybe a -> a -> a
(|||) (Just x) _ = x
(|||) Nothing  x = x
infixl 1 |||
