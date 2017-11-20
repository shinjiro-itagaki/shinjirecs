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
