module Jobs.Recording where
import Job(Job(..),EnqueueOption(..),QueueInterface(..),ActiveJob(..))
import DB(Reservation)

import System.Process(proc,shell,CreateProcess,createProcess) -- process
import System.IO -- base
import Control.Monad.IO.Class(MonadIO, liftIO) -- transformers
import Config(Config(..))

data RecordingJob = RecordingJob{
  mRecord  :: Maybe DB.Reservation
  ,config :: Config
  ,queueI :: QueueInterface
  }

performImpl :: RecordingJob -> IO RecordingJob
performImpl job = do
  -- proc :: FilePath -> [String] -> CreateProcess
  (stdin, stdout, stderr, procHandle) <- createProcess $ proc fname' args'
  return job
  where
    fname' = ""
    args' = ["",""]
    mr' = mRecord job
    
instance ActiveJob RecordingJob where
  -- new :: QueueInterface -> j
  new conf qi = RecordingJob { config = conf, queueI = qi, mRecord = Nothing  }
  
  -- queue :: j -> QueueInterface
  queue = queueI 
  
  -- jobClassId :: j -> Integer
  jobClassId self = 474265
  
  -- perform    :: (Monad m) => j -> m j
  perform self = liftIO $ performImpl self
