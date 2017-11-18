module Records.Area exposing (Area,AreaId)
import Time exposing (Time)
import Json.Decode as D
import Json.Decode.Pipeline exposing (decode,required,optional)

type AreaId = AreaId Int
type alias Area = { label : String
                  , channels_checked : Bool
                  , created_at : Time
                  , updated_at : Time
                  }

areaDecoder : D.Decoder Area
areaDecoder =
    decode Area
        |> required "label"            D.string
        |> required "channels_checked" D.bool
        |> required "created_at"       D.float
        |> required "updated_at"       D.float
