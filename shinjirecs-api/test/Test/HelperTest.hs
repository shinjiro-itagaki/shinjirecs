module Test.HelperTest where
import Test.HUnit
import Helper((.++),holidays,weekdays,allWeekDays,weekdayInterval,nearestWeekDay)
import Class.Castable(from)
import Data.Dates(WeekDay(..)) -- dates

tests :: Test
tests = TestList [
  TestLabel "Test of Routing.getMaybeRawPathParamsFromPatternAndPath" $ TestList $
  [
    (nearestWeekDay Monday [Tuesday])               ~?= Tuesday
  , (nearestWeekDay Friday [Tuesday,Sunday])        ~?= Sunday
  , (nearestWeekDay Friday [Tuesday,Sunday,Friday]) ~?= Sunday
  ]
  ]
