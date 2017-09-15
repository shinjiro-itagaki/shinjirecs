{-# LANGUAGE OverloadedStrings #-}
module Session.Class where
import Data.Int(Int64)
import Data.ByteString(ByteString)
import Data.Time.Clock(UTCTime ,getCurrentTime) -- ,utctDay)
import Helper.DateTimeHelper((.++))

class Show k => IsSessionKey k where

class (Show v, Read v) => IsSessionValue v where
  
class IsSession s where
  start   :: ByteString -> IO s
  load    :: ByteString -> IO s
  read    :: (IsSessionKey k, IsSessionValue v) => s -> k -> IO (Maybe v)
  write   :: (IsSessionKey k, IsSessionValue v) => s -> k -> v -> IO ()
  delete  :: (IsSessionKey k, IsSessionValue v) => s -> k -> IO (Maybe v)
  destroy :: s -> IO ()
  renew   :: s -> IO s
  getExpire :: s -> UTCTime
  setExpire :: s -> UTCTime -> IO s
  addExpireSecond :: s -> Int64 -> IO UTCTime

-- gc :: IO Int64
-- gc = 0

setNewExpire :: IsSession s => s -> Int64 -> IO s
setNewExpire s sec = getCurrentTime >>= (setExpire s . (.++ sec))
