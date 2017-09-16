{-# LANGUAGE OverloadedStrings #-}
module Session.Class where
import Data.Int(Int64)
import Data.ByteString(ByteString)
import Data.Time.Clock(UTCTime ,getCurrentTime) -- ,utctDay)
import Helper.DateTimeHelper((.++))
import DB(Connection)
import Data.Aeson(ToJSON,FromJSON)
import Class.String(StringClass)

class (ToJSON v, FromJSON v) => IsSessionValue v where
  
class IsSession s where
  start   :: Connection -> Maybe String -> IO (Maybe s)
  get     :: (StringClass k, IsSessionValue v) => s -> k -> Maybe v
  set     :: (StringClass k, IsSessionValue v) => s -> k -> v -> s
  del     :: (StringClass k) => s -> k -> s
  save    :: s -> IO ()
  destroy :: s -> IO ()
  renew   :: s -> IO (Maybe s)
  getExpire :: s -> UTCTime
  setExpire :: s -> UTCTime -> s
  addExpireSecond :: s -> Int64 -> s

-- gc :: IO Int64
-- gc = 0

setNewExpire :: IsSession s => s -> Int64 -> IO s
setNewExpire s sec = getCurrentTime >>= (return . setExpire s . (.++ sec))
