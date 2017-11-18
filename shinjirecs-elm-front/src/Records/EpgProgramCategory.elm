module Records.EpgProgramCategory exposing (EpgProgramCategory,EpgProgramCategoryId)
import Time exposing (Time)
import Json.Decode as D
import Json.Decode.Pipeline exposing (decode,required,optional)

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
        |> required "parent_id"   (D.maybe <| D.int)
        |> required "created_at"  D.float
        |> required "updated_at"  D.float
    
