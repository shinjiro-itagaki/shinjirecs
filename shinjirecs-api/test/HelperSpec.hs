module HelperSpec where
import Test.Hspec(Spec,describe,it) -- hspec
import Test.Hspec.Expectations(shouldBe) -- hspec-expectations
import Helper(from,(.++),holidays,weekdays,allWeekDays,weekdayInterval,nearestWeekDay)
import Data.Dates(WeekDay(..)) -- dates

spec :: Spec
spec = do
  describe "Helper test" $ do  
    it "nearestWeekDay" $ do
      (nearestWeekDay Monday [Tuesday]) `shouldBe` Tuesday
      (nearestWeekDay Friday [Tuesday,Sunday]) `shouldBe` Sunday
      (nearestWeekDay Friday [Tuesday,Sunday,Friday]) `shouldBe` Sunday


