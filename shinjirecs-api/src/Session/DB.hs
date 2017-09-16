{-# LANGUAGE OverloadedStrings #-}
module Session.DB where
import Session.Class(IsSession(start,get,set,del,save,destroy,renew,getExpire,setExpire,addExpireSecond), IsSessionValue)
import qualified DB
import DB(Connection,Session(..),(.==))
import Data.Time.Clock(UTCTime,getCurrentTime)
import Data.Text(Text)
import Data.Int(Int64)
import Data.UUID(UUID,toString)
import Data.UUID.V4(nextRandom)
-- import Data.Aeson(ToJSON(..), Result(Error,Success),FromJSON, fromJSON, parseJSON,toJSON,ToJSON,Value(Object), encode)
import Data.Aeson(Value(Object), encode, fromJSON , Result(Error,Success), ToJSON(toJSON))
import Class.String(toJSON,StringClass(toText))
import Helper.DateTimeHelper((.++))
import Data.HashMap.Lazy(HashMap, lookup, fromList, insert, delete)

data SessionData = SessionData {
  connection :: DB.Connection,
  entity :: DB.Entity DB.Session
  }

sessionId :: SessionData -> String
sessionId = sessionSid . snd . entity

sessionHashMap :: SessionData -> HashMap Text Value
sessionHashMap s = case Class.String.toJSON $ sessionData $ snd $ entity s of
  Just (Object obj) -> obj
  _ -> fromList []

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

instance IsSession SessionData where
--  start :: Connection -> Maybe String -> IO (Maybe SessionData)
  start conn Nothing = nextRandom >>= start conn . Just . toString
  start conn (Just id) = do
    now <- getCurrentTime
    let dflt = Session id "" now
        t = (DB.readTable conn)
    kv <- DB.select t [DB.SessionSid .== id] [DB.mkLimit 1]
    case kv of
      [] -> do
        DB.insert t dflt >>= DB.get t >>= return . fmap (SessionData conn)
      (e:xs) -> do
        return $ Just $ SessionData {connection = conn, entity = e }

--  get :: (StringClass k, IsSessionValue v) => s -> k -> Maybe v
  get (SessionData conn e) k = case Class.String.toJSON $ sessionData $ snd e of
    Just val -> lookup' k val
    Nothing -> Nothing
    
--  set :: (StringClass k, IsSessionValue v) => s -> k -> v -> SessionData
  set (SessionData conn e@(ek,ev)) k v =
    SessionData conn $ (,) ek $ (\t -> ev { sessionData = t }) $ toText $ encode $ case Class.String.toJSON $ sessionData $ snd e of
      Just  d -> insert' k v d
      Nothing -> Object $ fromList []
    
--  del :: (StringClass k, IsSessionValue v) => s -> k -> SessionData
  del s@(SessionData _ (ekey, eval)) k = s { entity = newe'}
    where
      mvalue = Class.String.toJSON $ sessionData $ eval
      newdata' = case mvalue of
        Just (Object obj) -> toText $ encode $ delete (toText k) obj
        _ -> sessionData $ eval
      newe' = (ekey, eval { sessionData = newdata' })
      
--  save :: s -> IO ()
  save s = DB.repsert table' k' v'
    where
      table' = DB.readTable $ connection s
      (k',v') = entity s
--  destroy :: s -> IO ()
  destroy s = do
    DB.delete (DB.readTable $ connection s) $ fst $ entity s
--  renew :: s -> IO s
  renew s = do
    newid <- nextRandom
    let new' = v' {sessionSid = toString newid}
    DB.repsert (DB.readTable $ connection s) k' (v' {sessionSid = toString newid})
    return $ Just $ s {entity = (k', new')}
    where
      (k',v') = entity s
--  getExpire :: s -> UTCTime
  getExpire = sessionExpiringDate . snd . entity
--  setExpire :: s -> UTCTime -> s
  setExpire s t = s { entity = (k',v' {sessionExpiringDate = t}) }
    where
      (k',v') = entity s
--  addExpireSecond :: s -> Int64 -> s
  addExpireSecond s sec = s { entity = (k',v' {sessionExpiringDate = t' .++ sec}) }
    where
      (k',v') = entity s
      t' = sessionExpiringDate v'
