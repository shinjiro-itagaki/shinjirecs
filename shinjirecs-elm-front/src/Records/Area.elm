module Records.Area exposing (Area,AreaId)
import Time exposing (Time)
import Json.Decode as D

type AreaId = AreaId Int
type alias Area = { label : String
                  , channels_checked : Bool
                  , created_at : Time
                  , updated_at : Time
                  }

areaDecoder : D.Decoder Area
areaDecoder =
    D.map4 Area
        (D.field "label"            D.string)
        (D.field "channels_checked" D.bool)
        (D.field "created_at"       D.float)
        (D.field "updated_at"       D.float)
