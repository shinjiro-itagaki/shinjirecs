{-# LANGUAGE OverloadedStrings #-}
module Session.DB where
import Session.Class(IsSession(start,read,write,delete,destroy,renew,getExpire,setExpire,addExpireSecond), IsSessionValue)
import qualified DB
-- import DB(Session(sessionId,sessionData))
import Data.Time.Clock(UTCTime)
import Data.Int(Int64)
import Data.UUID(UUID,toString)
import Data.UUID.V4(nextRandom)
-- import Data.Aeson(ToJSON(..), Result(Error,Success),FromJSON, fromJSON, parseJSON,toJSON,ToJSON,Value(Object), encode)
import Data.Aeson(Value(Object), encode, fromJSON , Result(Error,Success), ToJSON(toJSON))
import Class.String(toJSON,StringClass(toText))
import Data.HashMap.Lazy(HashMap, lookup, fromList, insert)

-- import Model(ModelClass(..))
-- instance ModelClass DB.Session
--Session
--  sessionID String
--  data      Text

data SessionData = SessionData {
  connection :: DB.Connection,
  entity :: DB.Entity DB.Session
  }

lookup' :: (StringClass k, IsSessionValue v) => k -> Value -> Maybe v
lookup' k (Object obj) = case Data.HashMap.Lazy.lookup (toText k) obj of
  Nothing -> Nothing
  Just val -> case fromJSON val of
    Error str -> Nothing
    Success v -> Just v
lookup' _ _ = Nothing

insert' :: (StringClass k, IsSessionValue v) => k -> v -> Value -> Value
insert' k v (Object obj) = Object $ insert (toText k) (Data.Aeson.toJSON v) obj
insert' k v _ = Object $ fromList [(toText k, Data.Aeson.toJSON v)]

{-
instance IsSession SessionData where
--    start   :: String -> IO (Maybe s)
  start conn Nothing = nextRandom >>= start conn . Just . toString
  start conn (Just id) = do
    mkv <- getBy conn $ (DB.mkUnique dflt' :: [DB.Unique Session])
    case mkv of
      Just e -> return $ Just $ SessionData {connection = conn, entity = e }
      Nothing -> DB.insert dflt' >>= DB.get conn >>= return . fmap (Session conn)
    where
      dflt' = DB.Session id ""
    
--  read    :: (IsSessionKey k, IsSessionValue v) => s -> k -> IO (Maybe v)
  read (Session conn e) k = do
    return $ fmap (lookup' k) $ toJSON $ sessionData $ snd e
    
  write   :: (StringClass k, IsSessionValue v) => s -> k -> v -> IO SessionData
  write (Session conn e) k v = do
    case toJSON $ sessionData $ snd e of
      Just d -> SessionData conn $ e {sessionData = toText $ encode $ insert' k v d }
      Nothing -> SessionData conn $ e {sessionData = toText $ encode $ Object $ fromList [] }
    
  delete  :: (StringClass k, IsSessionValue v) => s -> k -> IO (Maybe v)
  destroy :: s -> IO ()
  renew   :: s -> IO s
  getExpire :: s -> UTCTime
  setExpire :: s -> UTCTime -> IO s
  addExpireSecond :: s -> Int64 -> IO UTCTime  

-}
