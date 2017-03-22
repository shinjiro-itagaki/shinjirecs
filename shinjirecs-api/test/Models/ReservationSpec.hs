module Models.ReservationSpec where
import Data.Maybe(fromJust)
import Test.Hspec(Spec,describe,it) -- hspec
import Test.Hspec.Expectations(shouldBe) -- hspec-expectations
import Test.QuickCheck
import Control.Exception (evaluate)
import DB
import Models.Reservation
import Config(Config(..),PathsConfig(..),ReservationConfig(..))
import Helper(from,(.++),holidays,weekdays,allWeekDays,weekdayInterval,nearestWeekDay)
import DB.Status(ReservationState(..))
import Data.Text(pack)
import System.FilePath.Posix((</>))
import Data.Dates(WeekDay(..)) -- dates
--- import Text.Printf(printf)

-- import Data.Time.Clock(UTCTime(..))
--import Data.Time.Clock(getCurrentTime)
--import Control.Monad.IO.Class(liftIO) -- base

sampleRec = DB.Reservation { reservationVideoFileNameFormat = "%{st.year}-%{st.mon}-%{st.dd}-%{st.hh}-%{st.mm}-%{st.ss}",
                             reservationStartTime = from (2017,3,21,13,5,11), -- Tuesday
                             reservationDuration = 1800,
                             reservationKeta = 3,
                             reservationTitle = "WBC 2017 Semi-Final NED VS PUR",
                             reservationDescription = pack "World Baseball Classic 2017 semi final Netherlands vs Puerto-Rico",
                             reservationNext = 1 * 3600 * 24,
                             reservationName = pack "WBC 2017",
                             reservationCounter = 37,
                             reservationXwday   = from holidays,
                             reservationState   = Waiting}

spec :: Spec
spec = do
  let now = from (2017,3,21,13,5,11)
      count = 37
      keta  = 4
      vdir = "private/test/videofiles"
      pconf = PathsConfig {
        privateDir = "private/test",
        commandDir = "private/test/commands",
        videoFilesDir = vdir
        } -- reservationFilePath
      sample' = sampleRec { reservationVideoFileNameFormat = "%{st.year}-%{st.mon}-%{st.dd}-%{st.hh}-%{st.mm}-%{st.ss}-%{counter}",
                            reservationStartTime = now,
                            reservationXwday = from [Monday,reservationWeekDay sampleRec],
                            reservationKeta = keta}
      next' = fromJust $ createNextReservation sample'

  describe "Models.Reservation test" $ do
    it "reservationFilePath" $ do
      (reservationFilePath pconf sample') `shouldBe` ((videoFilesDir pconf) </> "2017-03-21-13-05-11-0037")
    it "createNextReservation starttime" $ do
      (reservationStartTime next') `shouldBe` ((reservationStartTime sample') .++ (reservationNext sample'))
    it "next after createNext" $ do
      (reservationNext next') `shouldBe` 0
    it "weekday of starttime of after next" $ do
      (reservationWeekDay next') `shouldBe` Wednesday
    it "calcNext" $ do
      (calcNext sampleRec { reservationXwday = from holidays,
                            reservationNext = 0
                          }) `shouldBe` (3600 * 24 * weekdayInterval Tuesday Saturday)
    it "createNextReservation weekday of starttime" $ do
      (reservationWeekDay $ fromJust $ createNextReservation next') `shouldBe` Monday
    it "createNextReservation same weekday" $ do
      (reservationWeekDay $ fromJust $ createNextReservation $ next' {reservationXwday = from Monday}) `shouldBe` Monday
