module Models.ReservationSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import DB
import Models.Reservation
import Config(Config(..),PathsConfig(..),ReservationConfig(..))
import Helper
import DB.Status(ReservationState(..))
import Data.Text(pack)
import System.FilePath.Posix((</>))
--- import Text.Printf(printf)

-- import Data.Time.Clock(UTCTime(..))
--import Data.Time.Clock(getCurrentTime)
--import Control.Monad.IO.Class(liftIO) -- base

sampleRec = DB.Reservation { reservationVideoFileNameFormat = "%{st.year}-%{st.mon}-%{st.dd}-%{st.hh}-%{st.mm}-%{st.ss}",
                             reservationStartTime = from (2017,3,21,13,5,11),
                             reservationDuration = 1800,
                             reservationKeta = 3,
                             reservationTitle = "WBC 2017 Semi-Final NED VS PUR",
                             reservationDescription = pack "World Baseball Classic 2017 semi final Netherlands vs Puerto-Rico",
                             reservationNext = 1,
                             reservationName = pack "WBC 2017",
                             reservationCounter = 37,
                             reservationXwday   = 0,
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
                            reservationKeta = keta}
        
  describe "Models.Reservation test" $ do
    it "Int Inverse Element" $ do
      (reservationFilePath pconf sample') `shouldBe` (videoFilesDir pconf) </> "2017-03-21-13-05-11-0037"
      -- putStrLn "not yet implemented"
      -- putStr $ reservationFilePath pconf sample'
      -- (+) 5 6 `shouldBe` 11
      
