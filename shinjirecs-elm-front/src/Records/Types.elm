module Records.Types exposing (..)
import Json.Decode as D
import Json.Encode exposing (Value)
import Date as D
import List exposing (map)
import Dict exposing (Dict,fromList)

type alias Entity a =
    { id  : Int
    , val : a
    }

type ChannelType = GR | BS    

entitiesToDict : List (Entity a) -> Dict Int (Entity a)
entitiesToDict = fromList << List.map (\e -> (e.id,e))

type alias Errors = Dict String String

toBool : String -> Result String Bool
toBool str =
    case String.toLower str of
        "on"    -> Ok True
        "t"     -> Ok True
        "true"  -> Ok True
        "yes"   -> Ok True
        "off"   -> Ok False
        "f"     -> Ok False
        "false" -> Ok False
        "no"    -> Ok False
        x       -> Result.map (\x -> x > 0) <| String.toInt x
