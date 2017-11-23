module Utils.DateTime exposing (..)
import List exposing (filter)
import Utils.List exposing (mapWithIndex)
import Bitwise exposing (shiftLeftBy, and)
import Date exposing (Day(Sun,Mon,Tue,Wed,Thu,Fri,Sat),Month(Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec))

type alias WeekdayFlags  = { sun : Bool, mon : Bool, tue : Bool, wed : Bool, thu : Bool, fri : Bool, sat : Bool }

weekdayLabel : Day -> { ja : String, en : String }
weekdayLabel d =
    (\(ja,en) -> { ja = ja, en = en } )
    <| case d of
           Sun -> ("日", "Sun")
           Mon -> ("月", "Mon")
           Tue -> ("火", "Tue")
           Wed -> ("水", "Wed")
           Thu -> ("木", "Thu")
           Fri -> ("金", "Fri")
           Sat -> ("土", "Sat")
    
weekdayToInt : Day -> Int
weekdayToInt d =
    case d of
        Sun -> 0
        Mon -> 1
        Tue -> 2
        Wed -> 3
        Thu -> 4
        Fri -> 5
        Sat -> 6

weekdays = [Sun,Mon,Tue,Wed,Thu,Fri,Sat]
               
monthToInt : Month -> Int
monthToInt m =
    case m of
        Jan -> 1
        Feb -> 2
        Mar -> 3
        Apr -> 4
        May -> 5
        Jun -> 6
        Jul -> 7
        Aug -> 8
        Sep -> 9
        Oct -> 10
        Nov -> 11
        Dec -> 12

months = [Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec]
               
weekdayFlagsToInt : WeekdayFlags -> Int
weekdayFlagsToInt x =
    [x.sun,x.mon,x.tue,x.wed,x.thu,x.fri,x.sat]
        |> mapWithIndex (\b idx -> if b then shiftLeftBy idx 1 else 0)
        |> List.foldr (+) 0

(<<.) : Int -> Int -> Int
(<<.) org shift = shiftLeftBy shift org

isOn : Int -> Day -> Bool
isOn i d = (/=) 0 <| and i <|  (1 <<. (weekdayToInt d))
                  
intToWeekdayFlags : Int -> WeekdayFlags
intToWeekdayFlags i =
    let f = isOn i
    in  { sun = f Sun
        , mon = f Mon
        , tue = f Tue
        , wed = f Wed
        , thu = f Thu
        , fri = f Fri
        , sat = f Sat
        }
