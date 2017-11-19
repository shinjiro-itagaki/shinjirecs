module Records.Area exposing (Area,AreaId,areaDecoder,areaEncoder)
import Time exposing (Time)
import Json.Decode as D
import Json.Encode as E
import Json.Decode.Pipeline exposing (decode,required,optional)
import Utils.Json exposing (Encoder)

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

areaEncoder : Encoder Area
areaEncoder x = E.object
                  [ ("label", E.string  x.label)
                  , ("channels_checked" , E.bool x.channels_checked )
                  , ("created_at"  , E.float x.created_at  )
                  , ("updated_at", E.float x.updated_at)
                  ]
