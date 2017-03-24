{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Config.Class where
import Data.Text (Text, pack, unpack)
import Class.Castable(Castable(from))
import Data.List.Extra (lower) -- extra
import qualified Data.Yaml as Y (decodeFile, FromJSON, Object, Value(Object, String, Number))
import Data.HashMap.Strict as M
import qualified DB -- (AdapterType(..), Config(Config), adapter, database, pool, timeout) as
import Data.Scientific (Scientific(..), coefficient)

data Env = Production | Development | Test deriving Show

class ConfigClass a where
  defaultConfig :: IO a
  readYaml :: FilePath -> Env -> a

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

readInclude :: Y.Object -> Maybe String
readInclude config =
  case M.lookup "<<" config of
    Just (Y.String s) -> Just $ Data.Text.unpack s
    Nothing           -> Nothing
    _                 -> fail "Invalid type for: <<"
    
lookupInt :: String -> Y.Object -> Maybe (Int)
lookupInt key config = lookupInteger (Data.Text.pack key) config >>= return . fromInteger

lookupInteger :: Text -> Y.Object -> Maybe Integer
lookupInteger k config =
  case M.lookup k config of
    Just (Y.Number t) -> Just (coefficient t)
    Nothing           -> Nothing
    _                 -> fail $ "Invalid type (not integer) for: " ++ (Data.Text.unpack k)

lookupText :: Text -> Y.Object -> Maybe Text
lookupText k config =
  case M.lookup k config of
    Just (Y.String t) -> Just t
    Nothing           -> Nothing
    _                 -> fail $ "Invalid type (not string) for: " ++ (Data.Text.unpack k)

lookupString :: Text -> Y.Object -> Maybe String
lookupString k config = lookupText k config >>= return . Data.Text.unpack
