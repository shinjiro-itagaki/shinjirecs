{-# LANGUAGE OverloadedStrings #-}
module Models.Reservation where
import System.Process(CreateProcess,createProcess)
import Control.Monad.IO.Class(MonadIO,liftIO) -- base
import DB(Reservation(..))
import qualified DB
import DB.Status(ReservationState(..))
import Model(ModelClass(..))
import Data.Maybe(isJust,catMaybes)
import Data.Foldable(all)
import Data.Char(toLower)
import Data.Time.Clock(UTCTime,getCurrentTime,utctDay)
import Class.Castable(from)
import Class.String(replace)
import Class.DateTime(DateTimeClass(..))
import Helper.DateTimeHelper(finishTime,inTime,inTimeNow,weekDayFlagsToWeekDays,nearestWeekDayInterval,(.++))
import Helper.NumHelper(pNum0xd)

import Data.Dates(WeekDay(..),dateWeekDay,dayToDateTime) -- dates
import Config(Config(..),PathsConfig(..),ReservationConfig(..),ReservationCommandArg(..),scriptArgs)
import System.FilePath.Posix((</>),pathSeparators) -- filepath
import Class.String(StringClass(..))


instance ModelClass DB.Reservation

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
symbolValue r (StartTime typ) = impl' typ
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
symbolValue r ProgramName = toString $ reservationName r

instance Show FormatSymbol where
  show Counter = "counter"
  show (StartTime typ) = "st." ++ (show typ)
  show ProgramName = "name"

allFormatSymbols :: [FormatSymbol]
allFormatSymbols = [Counter] ++ (map StartTime [minBound .. maxBound])

reservationCounterStr :: Reservation -> String
reservationCounterStr r = pNum0xd keta' counter'
  where
    keta'    = reservationKeta    r
    counter' = reservationCounter r

reservationFileName :: Reservation -> FilePath
reservationFileName r = sanitize' $ foldl replace' (reservationVideoFileNameFormat r) allFormatSymbols
  where
    replace'     rtn       sym = replace (symbolKeyStr sym) (symbolValue r sym) rtn
    sanitizeOne' str separator = replace [separator] "_" str
    sanitize'    str           = foldl sanitizeOne' str pathSeparators

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

reservationCommand :: MonadIO m => DB.Reservation -> DB.Table DB.Reservation -> PathsConfig -> ReservationConfig -> m (Maybe CreateProcess)
reservationCommand r t pconf rconf = liftIO $ do
  argMStrs' <- from $ map (reservationToCommandArg r t pconf) $ scriptArgs :: IO [Maybe String]
  return (if all isJust argMStrs'
          then Just $ from (script',catMaybes argMStrs')
          else Nothing)
  where
    script' = commandDir pconf </> scriptFilePath rconf
  
-- MonadIO m => 
reservationToCommandArg :: Reservation -> DB.Table Reservation -> PathsConfig -> ReservationCommandArg -> IO (Maybe String)
reservationToCommandArg r t pconf ArgDevice       = return $ Just ""
reservationToCommandArg r t pconf ArgChannel      = reservationChannel r t >>= return . (>>= return . DB.channelNumber . snd)
reservationToCommandArg r t pconf ArgDurationSec  = return $ Just $ show $ reservationDuration r
reservationToCommandArg r t pconf ArgDestFilePath = return $ Just $ reservationFilePath pconf r

reservationChannel :: DB.Reservation -> DB.Table Reservation -> IO (Maybe (DB.Entity DB.Channel))
reservationChannel r t = getChannel' (reservationChannelId r) -- error "not implemented" -- return Nothing -- runDB conn $ findByKey $ reservationChannelId r
  where
    getChannel' :: DB.Key DB.Channel -> IO (Maybe (DB.Entity DB.Channel))
    getChannel' key = DB.get chtable' key
    chtable' = (DB.readTable $ DB.connection t) :: DB.Table DB.Channel

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
