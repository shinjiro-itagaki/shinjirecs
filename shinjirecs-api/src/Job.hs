{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
module Job where
import Data.Eq(Eq(..))
import Data.Ord(Ord,Ordering(..))
import Data.Time.Clock(UTCTime(..),secondsToDiffTime)
import Data.Time.Calendar(fromGregorian)
import Helper((.++))

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

class ActiveJob j where
  perform       :: (Monad m) => j -> m j  -- please implementate
  new           :: QueueInterface -> j    -- please implementate
  queue         :: j -> QueueInterface    -- please implementate
  jobClassId    :: j -> Integer           -- please implementate , return unique value

  beforeEnqueue,afterEnqueue,beforePerform :: (Monad m) => j -> m (Bool,j)
  beforeEnqueue = return . (,) True
  afterEnqueue  = return . (,) True
  beforePerform = return . (,) True
  afterPerform  :: (Monad m) => j -> m j
  afterPerform  = return

  beforeEnqueueCancel  :: (Monad m) => j -> m (Bool,j)
  afterEnqueueCanceled :: (Monad m) => j -> m j

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
