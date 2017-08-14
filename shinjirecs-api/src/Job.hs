{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
module Job where
import Control.Monad.IO.Class(liftIO)
import Control.Exception.Base(SomeException)
import Data.Eq(Eq(..))
import Data.Ord(Ord,Ordering(..))
import Data.Time.Clock(UTCTime(..),secondsToDiffTime)
import Data.Time.Calendar(fromGregorian)
import Helper.DateTimeHelper((.++))
import Control.Monad.IO.Class(MonadIO)
import Config(Config)
import Control.Concurrent(forkFinally,killThread,threadDelay,ThreadId)
-- import Control.Concurrent.MVar(MVar,newMVar,takeMVar,putMVar)

data EnqueueOption = QueueName String | WaitUntil UTCTime | Wait UTCTime Integer | Priority Integer | NoOption

type PerformFunction = Integer -> Bool 

data Job = Job {
  performFunction :: PerformFunction
  ,scheduledAt :: UTCTime
  ,jobId       :: Integer
  ,queueName   :: String
  ,priority    :: Integer
  ,classId     :: Integer
  }

instance Eq Job where
  (==) ja jb = (priority ja) == (priority jb) && (scheduledAt ja) == (scheduledAt jb)
  
instance Ord Job where
  compare ja jb =
    case (priority ja) `compare` (priority jb) of
      EQ -> (scheduledAt ja) `compare` (scheduledAt jb)
      cmp -> cmp

mkDefaultJob = Job {
  performFunction = (\i -> True)
  ,scheduledAt    = UTCTime { utctDayTime = secondsToDiffTime 0, utctDay = fromGregorian 2017 2 27 }
  ,jobId          = 0
  ,queueName      = ""
  ,priority       = 0
  ,classId        = 0
  }


data QueueInterface = QueueInterface {
  enqueueJob  :: Job   -> (Bool,Job)
  ,cancelJob  :: Integer -> Bool
  }

setOptionToJob :: Job -> EnqueueOption -> Job
setOptionToJob j (QueueName v) = j { queueName   = v }
setOptionToJob j (WaitUntil v) = j { scheduledAt = v }
setOptionToJob j (Wait    t v) = j { scheduledAt = t .++ v }
setOptionToJob j (Priority  v) = j { priority    = v }
setOptionToJob j _             = j

setOptionsToJob :: Job -> [EnqueueOption] -> Job
setOptionsToJob j     [] = j
setOptionsToJob j (x:xs) = setOptionsToJob (setOptionToJob j x) xs

mkJobFromOptions :: [EnqueueOption] -> Job
mkJobFromOptions options = setOptionsToJob mkDefaultJob options

data JobCancelCode = Canceled | CancelFailed | FinishedYet | CanceledYet | CancelNotImplemented | BeforeBlocked

class ActiveJob j where
  perform       :: (MonadIO m) => j -> m j  -- please implement
  new           :: Config -> QueueInterface -> j    -- please implement
  queue         :: j -> QueueInterface    -- please implement
  jobClassId    :: j -> Integer           -- please implement , return unique value

  beforeEnqueue,afterEnqueue,beforePerform,beforeEnqueueCancel,beforePerformCancel :: (MonadIO m) => j -> m (Bool,j)
  
  -- please override if you need
  beforeEnqueue = return . (,) True
  
  -- please override if you need
  afterEnqueue  = return . (,) True
  
  -- please override if you need
  beforePerform = return . (,) True

  afterPerformed, afterPerformFailed, afterEnqueueCanceled,afterEnqueueCancelFailed :: (MonadIO m) => j -> m j
  afterPerformCanceled, afterPerformCancelFailed :: (MonadIO m) => JobCancelCode -> j -> m j
  -- please override if you need
  afterPerformed  = return

  -- please override if you need
  afterPerformFailed = return  
 
  -- please override if you need
  beforeEnqueueCancel = return . (,) True
  
  -- please override if you need
  afterEnqueueCanceled = return

  -- please override if you need
  beforePerformCancel = return . (,) True

  -- please override if you need
  afterEnqueueCancelFailed = return

  -- please override if you need
  afterPerformCanceled code = return

  -- please override if you need
  afterPerformCancelFailed code = return

  performNow :: (MonadIO m) => j -> m ThreadId
  performNow self = liftIO $ forkFinally impl' onError'
    where
      -- impl' :: IO j
      impl' = do
        (b,self2) <- beforePerform self
        if b
          then perform self2 >>= afterPerformed
          else return self2
      onError' :: Either SomeException j -> IO ()
      onError' (Left ex) = return ()
      onError' (Right _) = return ()

  performLater :: (MonadIO m) => j -> [EnqueueOption] -> m j
  performLater self options = do
    (b,self2) <- beforeEnqueue self
    if b
      then
      do
        let (b', job') = enqueueJob' $ setOptionsToJob (toJob self2) options in
          if b'
          then
          do
            (b2, self3) <- afterEnqueue self2
            if b2
              then return self3
              else
                if cancelJob' $ jobId job'
                then return self3
                else return self3
          else return self2
      else return self2
    return self
    where
      queueI' = queue self
      cancelJob' = cancelJob queueI'
      enqueueJob' = enqueueJob queueI'
      
  toJob :: j -> Job
  toJob self = mkDefaultJob { performFunction = (\i -> True ), classId = jobClassId self}

  performCancel, performCancelWithoutHooks :: (MonadIO m) => j -> m (JobCancelCode,j)

  performCancel self = do
    -- check performing(enqueued)
    -- if enqueued then remove from queue
    (b,self2) <- beforePerformCancel self
    if b -- if performing now then 
      then performCancelWithoutHooks self2 >>= after'
      else return (BeforeBlocked,self2)
    where
      -- doAfterHook' :: (MonadIO m) => JobCancelCode -> j -> (JobCancelCode -> j -> m j) -> m j
      doAfterHook' code self func = func code self >>= return . (,) code
      -- after' :: (MonadIO m) => (JobCancelCode,j) -> m j
      after' (Canceled,   self) = doAfterHook' Canceled    self afterPerformCanceled     
      after' (CanceledYet,self) = doAfterHook' CanceledYet self afterPerformCanceled     
      after' (code       ,self) = doAfterHook' code        self afterPerformCancelFailed 

  -- please implement if you need
  performCancelWithoutHooks self = return (CancelNotImplemented, self)

