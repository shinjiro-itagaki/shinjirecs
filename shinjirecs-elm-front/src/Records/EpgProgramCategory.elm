module Records.EpgProgramCategory exposing (EpgProgramCategory,EpgProgramCategoryId,epgProgramCategoryDecoder,epgProgramCategoryEncoder)
import Time exposing (Time)
import Json.Decode as D
import Json.Decode.Pipeline exposing (decode,required,optional)
import Json.Encode as E
import Utils.Maybe exposing (ifJust)
import Utils.Json exposing (Encoder)

type EpgProgramCategoryId = EpgProgramCategoryId Int
type alias EpgProgramCategory =
    { label_ja : String
    , label_en : String
    , parent_id : Maybe Int
    , created_at : Time
    , updated_at : Time
    }

epgProgramCategoryDecoder : D.Decoder EpgProgramCategory
epgProgramCategoryDecoder =
    decode EpgProgramCategory
        |> required "label_ja"    D.string
        |> required "label_en"    D.string
        |> optional "parent_id"   (D.maybe <| D.int) Nothing
        |> required "created_at"  D.float
        |> required "updated_at"  D.float
    

epgProgramCategoryEncoder : Encoder EpgProgramCategory
epgProgramCategoryEncoder x = E.object
                  [ ("label_ja", E.string  x.label_ja)
                  , ("label_en", E.string  x.label_en)
                  , ("parent_id" , ifJust E.int x.parent_id E.null)
                  , ("created_at"  , E.float x.created_at)
                  , ("updated_at", E.float x.updated_at)
                      
                  ]
