{-# LANGUAGE OverloadedStrings #-}
module Models.Reservation where
import System.Process(CreateProcess,createProcess)
import Control.Monad.IO.Class(MonadIO,liftIO) -- base
import DB(Reservation(..),Channel(..))
import Database.Persist.Sql(ConnectionPool)
import DB.Status(ReservationState(..))
import Model(ActiveRecord(..),runDB,findByKey)
-- import qualified Database.Persist.Class as PS
import Database.Persist.Sql(toSqlKey)  --persistent
import Data.Maybe(isJust,catMaybes)
import Data.Foldable(all)
import Data.Char
import Data.Time.Clock(UTCTime,getCurrentTime,utctDay)
import Class.Castable(from)
import Helper(finishTime,inTime,inTimeNow,(.++),weekDayFlagsToWeekDays,nearestWeekDayInterval,DateTime(..), pNum0xd,replaceString)

import Data.Dates(WeekDay(..),dateWeekDay,dayToDateTime) -- dates
import Config(Config(..),PathsConfig(..),ReservationConfig(..),ReservationCommandArg(..),scriptArgs)
import System.FilePath.Posix((</>),pathSeparators) -- filepath
import Database.Persist.Types (Entity(..))

instance ActiveRecord Reservation

  -- delete
  -- deleteWhere

-- select_label(lmap)
-- excluded?(wday)
isEveryweek     :: Reservation -> Bool
setEveryweek    :: Reservation -> Reservation
setNotEveryweek :: Reservation -> Reservation
isWaiting       :: Reservation -> Bool
isSameState     :: ReservationState -> Reservation -> Bool

isEveryweek     r = reservationNext r == 7
setEveryweek    r = r { reservationNext = 7 }
setNotEveryweek r@Reservation { reservationNext = 7 } = r { reservationNext = 0 }
setNotEveryweek r = r
-- isVideoFileExist :: Reservation -> Bool ---File.exist? self.videofilepath end
-- isVideoFileExist = 
state       = reservationState
isSameState = impl' where impl' status r = (reservationState r) == status
isWaiting   = isSameState Waiting
isRecording = isSameState Recording
isSuccess   = isSameState Success
isFailed    = isSameState Failed

-- recorded?
-- destroy()
-- videofile_deletable?
-- destroyable?
-- destroy_archive
-- destroy_videofile

-- filepath()
-- filename

-- videoFileNameFormat FilePath

data FormatSymbolDateType = Year | YYYY | Month | Mon | Day | DD | Hour | Hours | HH | Minute | Minutes | Min | MM | Second | Seconds | Sec | SS deriving (Show,Enum,Bounded)
data FormatSymbol = Counter | StartTime FormatSymbolDateType | ProgramName

symbolKeyStr :: FormatSymbol -> String
symbolKeyStr sym = "%{" ++ (map toLower $ show sym) ++ "}"

symbolValue :: Reservation -> FormatSymbol -> String
symbolValue r Counter = reservationCounterStr r
symbolValue r (StartTime tipe) = impl' tipe
  where
    printf' :: (Integral a) => a -> String
    printf' = pNum0xd 2 . fromIntegral
    ymd' = toYMD $ reservationStartTime r
    hms' = toHMS $ reservationStartTime r
    year' = show $ fst ymd'
    mon'  = printf' $ fst $ snd ymd'
    day'  = printf' $ snd $ snd ymd'
    hour' = printf' $ fst hms'
    min'  = printf' $ fst $ snd hms'
    sec'  = printf' $ snd $ snd hms'
    impl' :: FormatSymbolDateType -> String
    impl' Year    = year'
    impl' YYYY    = year'
    impl' Month   = mon'
    impl' Mon     = mon' 
    impl' Day     = day'
    impl' DD      = day'
    impl' Hour    = hour'
    impl' Hours   = hour'    
    impl' HH      = hour'
    impl' Minute  = min'
    impl' Minutes = min'
    impl' Min     = min'
    impl' MM      = min'
    impl' Second  = sec'
    impl' Seconds = sec'
    impl' Sec     = sec'
    impl' SS      = sec'
symbolValue r ProgramName = from $ reservationName r

instance Show FormatSymbol where
  show Counter = "counter"
  show (StartTime tipe) = "st." ++ (show tipe)

allFormatSymbols :: [FormatSymbol]
allFormatSymbols = [Counter] ++ (map StartTime [minBound .. maxBound])

reservationCounterStr :: Reservation -> String
reservationCounterStr r = pNum0xd keta' counter'
  where
    keta' = reservationKeta r
    counter' = reservationCounter r

reservationFileName :: Reservation -> FilePath
reservationFileName r = sanitize' $ foldl replace' (reservationVideoFileNameFormat r) allFormatSymbols
  where
    replace' rtn sym = replaceString (symbolKeyStr sym) (symbolValue r sym) rtn
    sanitizeOne' str separator = replaceString [separator] "_" str
    sanitize' str = foldl sanitizeOne' str pathSeparators

reservationFilePath :: PathsConfig -> Reservation -> FilePath
reservationFilePath pconfig r = (videoFilesDir pconfig) </> reservationFileName r

reservationFinishTime :: Reservation -> UTCTime
reservationFinishTime r = finishTime (reservationStartTime r) (reservationDuration r)

reservationInTime :: Reservation -> UTCTime -> Bool
reservationInTime r t = inTime (reservationStartTime r) (reservationDuration r) t

reservationInTimeNow :: Reservation -> IO Bool
reservationInTimeNow r = inTimeNow (reservationStartTime r) (reservationDuration r)

inTimeReservations :: [Reservation] -> UTCTime -> [Reservation]
inTimeReservations rs t = filter (\r -> reservationInTime r t) rs

inTimeNowReservations :: [Reservation] -> IO [Reservation]
inTimeNowReservations rs = getCurrentTime >>= return . inTimeReservations rs

increaseCounter :: Reservation -> Reservation
increaseCounter r = r { reservationCounter = 1 + reservationCounter r}

createNextReservation :: Reservation -> Maybe Reservation
createNextReservation r@Reservation {reservationNext = 0} = case calcNext r of
  0 -> Nothing
  n -> Just $ increaseCounter
       $ r { reservationStartTime = (reservationStartTime r) .++ n }

createNextReservation r@Reservation {reservationNext = x} = Just $ increaseCounter
  $ r { reservationStartTime = (reservationStartTime r) .++ x,
        reservationNext = 0}

reservationWeekDay :: Reservation -> WeekDay
reservationWeekDay = dateWeekDay . dayToDateTime . utctDay . reservationStartTime

reservationWeekDays :: Reservation -> [WeekDay]
reservationWeekDays Reservation { reservationXwday = xwday } = weekDayFlagsToWeekDays xwday

calcNext :: Reservation
         -> Word -- second
calcNext r = (*) (24 * 3600) $ nearestWeekDayInterval rWeekDay' rWeekDays'
  where
    rWeekDay'  = reservationWeekDay  r
    rWeekDays' = reservationWeekDays r

reservationCommand :: MonadIO m => Reservation -> ConnectionPool -> PathsConfig -> ReservationConfig -> m (Maybe CreateProcess)
reservationCommand r conn pconf rconf = liftIO $ do
  argMStrs' <- from $ map (reservationToCommandArg r conn pconf) $ scriptArgs :: IO [Maybe String]
  return (if all isJust argMStrs'
          then Just $ from (script',catMaybes argMStrs')
          else Nothing)
  where
    script' = commandDir pconf </> scriptFilePath rconf
  
-- MonadIO m => 
reservationToCommandArg :: Reservation -> ConnectionPool -> PathsConfig -> ReservationCommandArg -> IO (Maybe String)
reservationToCommandArg r conn pconf ArgDevice       = return $ Just ""
reservationToCommandArg r conn pconf ArgChannel      = reservationChannel r conn >>= return . (>>= return . channelNumber . entityVal)
reservationToCommandArg r conn pconf ArgDurationSec  = return $ Just $ show $ reservationDuration r
reservationToCommandArg r conn pconf ArgDestFilePath = return $ Just $ reservationFilePath pconf r

-- ReaderT SqlBackend IO (Maybe (Entity entity))
reservationChannel :: MonadIO m => Reservation -> ConnectionPool -> m (Maybe (Entity DB.Channel))
reservationChannel r conn = runDB conn $ findByKey $ reservationChannelId r

{-
  startTime UTCTime
  duration Int default=0
  title String
  description Text
  next Word
  name Text
  counter Word
  keta Word
  videoFileNameFormat FilePath
  xwday Word
  state ReservationState
-}
