module Records.EpgProgramCategory exposing (EpgProgramCategory,EpgProgramCategoryId)
import Time exposing (Time)
import Json.Decode as D

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
    D.map5 EpgProgramCategory
        (D.at ["label_ja"] D.string)
        (D.at ["label_en"] D.string)
        (D.at ["parent_id"] (D.maybe D.int))
        (D.at ["created_at"] D.float)
        (D.at ["updated_at"] D.float)
    
