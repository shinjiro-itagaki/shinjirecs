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
        (D.at ["label"] D.string)
        (D.at ["channels_checked"] D.bool)
        (D.at ["created_at"] D.float)
        (D.at ["updated_at"] D.float)
