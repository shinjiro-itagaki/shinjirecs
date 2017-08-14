module Test.HelperTest where
import Test.HUnit
import Helper.DateTimeHelper((.++),holidays,weekdays,allWeekDays,weekdayInterval,nearestWeekDay)
import Helper.ListHelper((\\))
import Class.Castable(from)
import Data.Dates(WeekDay(..)) -- dates
import Network.HTTP.Types.Method(StdMethod(GET,POST,HEAD,PUT,DELETE,TRACE,CONNECT,OPTIONS,PATCH), parseMethod)

tests :: Test
tests = TestList [
  TestLabel "Test of Helper.DateTimeHelper.nearestWeekDay" $ TestList $
  [
    (nearestWeekDay Monday [Tuesday])               ~?= Tuesday
  , (nearestWeekDay Friday [Tuesday,Sunday])        ~?= Sunday
  , (nearestWeekDay Friday [Tuesday,Sunday,Friday]) ~?= Sunday
  ],
  TestLabel "Test of Helper.ListHelper.(//)" $ TestList $
  [
    ([1,2,3,4,5,6,7] \\ [5,2,3]) ~?= [1,4,6,7]
  , ([minBound .. maxBound] \\ [GET,POST]) ~?= [HEAD,PUT,DELETE,TRACE,CONNECT,OPTIONS,PATCH]
  ]  
  ]
