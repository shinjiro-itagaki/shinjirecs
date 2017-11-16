module Records.Types exposing (..)
import Json.Decode as D
import Json.Encode exposing (Value)
import Date as D

type alias Entity a =
    { id  : Int
    , val : a
    }
