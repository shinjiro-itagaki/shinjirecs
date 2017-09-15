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
  read    :: (StringClass k, IsSessionValue v) => s -> k -> IO (Maybe v)
  write   :: (StringClass k, IsSessionValue v) => s -> k -> v -> IO s
  delete  :: (StringClass k, IsSessionValue v) => s -> k -> IO (Maybe v)
  destroy :: s -> IO ()
  renew   :: s -> IO (Maybe s)
  getExpire :: s -> UTCTime
  setExpire :: s -> UTCTime -> IO s
  addExpireSecond :: s -> Int64 -> IO UTCTime

-- gc :: IO Int64
-- gc = 0

setNewExpire :: IsSession s => s -> Int64 -> IO s
setNewExpire s sec = getCurrentTime >>= (setExpire s . (.++ sec))
