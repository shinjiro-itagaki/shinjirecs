{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
module Job where
-- import Data.List(List)

data EnqueueOption = QueueName String | WaitUntil Integer | Wait Integer | Priority Integer | NoOption

type ScheduledTime = Integer
type JobId         = Integer

type PerformFunction = Integer -> Bool 

data Job = Job {
  performFunction :: PerformFunction
  ,scheduledAt :: ScheduledTime
  ,jobId       :: JobId
  ,queueName   :: String
  ,priority    :: Integer
  }

mkDefaultJob = Job {
  performFunction = (\i -> True)
  ,scheduledAt    = 0
  ,jobId          = 0
  ,queueName      = ""
  ,priority       = 0
  }
-- instance Ord Job

data QueueInterface = QueueInterface {
  enqueueJob  :: Job   -> Bool
  ,cancelJob  :: JobId -> Bool
  }

setOptionToJob :: Job -> EnqueueOption -> Job
setOptionToJob j (QueueName v) = j { queueName   = v }
setOptionToJob j (WaitUntil v) = j { scheduledAt = v }
setOptionToJob j (Wait      v) = j { scheduledAt = v }
setOptionToJob j (Priority  v) = j { priority    = v }
setOptionToJob j _             = j

setOptionsToJob :: Job -> [EnqueueOption] -> Job
setOptionsToJob j     [] = j
setOptionsToJob j (x:xs) = setOptionsToJob (setOptionToJob j x) xs

mkJobFromOptions :: [EnqueueOption] -> Job
mkJobFromOptions options = setOptionsToJob mkDefaultJob options

class ActiveJob j where
  perform       :: (Monad m) => j -> m j  -- please implementate
  new           :: QueueInterface -> j    -- please implementate
  queue         :: j -> QueueInterface    -- please implementate

  beforeEnqueue,afterEnqueue,beforePerform :: (Monad m) => j -> m (Bool,j)
  beforeEnqueue = return . (,) True
  afterEnqueue  = return . (,) True
  beforePerform = return . (,) True
  afterPerform  :: (Monad m) => j -> m j
  afterPerform  = return

  performNow :: (Monad m) => j -> m j
  performNow self = do
    (b,self2) <- beforePerform self
    if b
      then perform self2 >>= afterPerform
      else return self2

  performLater :: (Monad m) => j -> [EnqueueOption] -> m j
  performLater self options = do
    (b,self2) <- beforeEnqueue self
    if b
      then
      do
        if enqueueJob queue' $ setOptionsToJob (toJob self2) options
          then afterEnqueue self2 >>= return . snd
          else return self2
      else return self2
    return self
    where
      queue' = queue self
      
  toJob :: j -> Job
  toJob self = mkDefaultJob { performFunction = (\i -> True )} -- mkJobFromOptions options
