{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Config.Class where
import Data.Text (Text, pack, unpack)
import Class.Castable(Castable(from))
import Data.List.Extra (lower) -- extra
import Data.Yaml(decodeFile, FromJSON, Object, Value(Object, Number))
import qualified Data.Yaml as Y
import Data.HashMap.Strict as M
-- import qualified DB -- (AdapterType(..), Config(Config), adapter, database, pool, timeout) as
import Data.Scientific (Scientific(..), coefficient)
import Data.Maybe(fromMaybe)
import Data.List(elem)
import Helper((|||))
import Config.Env(Env(..))

-- 引数で指定したキーを持つオブジェクトを返す
getObject :: Text -> Value -> Maybe (Object)
getObject env v = do
  envs <- fromObject v
  case M.lookup env envs of
    Just (Object o) -> Just o
    _               -> Nothing

fromObject :: Value -> Maybe (Object)
fromObject m = do
  case m of
    Object o -> Just o
    _        -> Nothing

class ConfigClass a where
  defaultConfig :: Env -> IO a
  objectToConfig :: Object -> a -> a
  mor :: a -> Object -> (a -> Maybe b) -> (Object -> Maybe b) -> Maybe b
  mor dflt obj dfltf objf = (objf obj) ||| (dfltf dflt)
  or :: a -> Object -> (a -> b) -> (Object -> Maybe b) -> b
  or dflt obj dfltf objf = fromMaybe (dfltf dflt) (objf obj)
  readYaml :: FilePath -> Env -> IO (Maybe a)
  readYaml path env = do
    putStrLn $ "filepath: " ++ path
    mallconfigs <- decodeFile path
    putStrLn $ show mallconfigs
    defaultConfig' <- defaultConfig env
    return $ (mallconfigs >>= (\all -> Just $ initImportOtherConfig' all defaultConfig'))
    where
      envstr' = Data.Text.pack $ lower $ show env
      initImportOtherConfig' allconfigs' dflt' = importOtherConfig allconfigs' [] envstr' (getObject envstr' allconfigs') dflt'

importOtherConfig :: ConfigClass a => Value -> [Text] -> Text -> Maybe Object -> a -> a
importOtherConfig _          _        _        Nothing        dflt = dflt
importOtherConfig allconfigs imported selfname (Just confobj) dflt =
  if elem selfname imported
  then dflt
  else onlySelfOrWithInc' $ readInclude confobj
  where
    imported' = imported ++ [selfname]
    onlySelfOrWithInc' Nothing         = objectToConfig confobj dflt
    onlySelfOrWithInc' (Just incText') = objectToConfig confobj $ importOtherConfig allconfigs imported' incText' (getObject incText' allconfigs) dflt

{-
importOtherConfig :: ConfigClass a => Y.Value -> [String] -> Y.Value -> a -> a
importOtherConfig allconfigs imported config dflt =
  (include config
    >>= (\str -> bool (Just str) Nothing (L.elem str imported))
    >>= (\str ->
            getObject (Data.Text.pack str) allconfigs
            >>= return . (config <<<) . importOtherConfig allconfigs (imported ++ [str]) . objectToPreDBConfig)) ||| config
-}
instance Castable String Env where
  from "production" = Production
  from "test"       = Test
  from _            = Development

instance Castable Env String where
  from Production  = "production"
  from Test        = "test"
  from Development = "development"
  
readInclude :: Object -> Maybe Text
readInclude config =
  case M.lookup "<<" config of
    Just (Y.String s) -> Just s -- $ Data.Text.unpack s
    Nothing         -> Nothing
    _               -> fail "Invalid type for: <<"
    
lookupInt :: Text -> Object -> Maybe (Int)
lookupInt key config = lookupInteger key config >>= return . fromInteger

lookupInteger :: Text -> Object -> Maybe Integer
lookupInteger k config =
  case M.lookup k config of
    Just (Number t) -> Just (coefficient t)
    Nothing         -> Nothing
    _               -> fail $ "Invalid type (not integer) for: " ++ (Data.Text.unpack k)

lookupText :: Text -> Object -> Maybe Text
lookupText k config =
  case M.lookup k config of
    Just (Y.String t) -> Just t
    Nothing           -> Nothing
    _                 -> fail $ "Invalid type (not string) for: " ++ (Data.Text.unpack k)

lookupString :: Text -> Object -> Maybe String
lookupString k config = lookupText k config >>= return . Data.Text.unpack

lookupWord :: Text -> Object -> Maybe (Word)
lookupWord key config = lookupInteger key config >>= return . fromInteger
