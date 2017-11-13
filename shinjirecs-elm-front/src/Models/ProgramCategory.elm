module Models.ProgramCategory exposing (ProgramCategory,ProgramCategoryId)
import Time exposing (Time)
import Json.Decode as D

type ProgramCategoryId = ProgramCategoryId Int
type alias ProgramCategory =
    { label : String
    , created_at : Time
    , updated_at : Time
    }

programCategoryDecoder : D.Decoder ProgramCategory
programCategoryDecoder =
    D.map3 ProgramCategory
        (D.at ["label"] D.string)
        (D.at ["created_at"] D.float)
        (D.at ["updated_at"] D.float)
    
