module Models.ProgramTitle exposing (ProgramTitle,ProgramTitleId)
import Models.Types exposing (map12)
import Time exposing (Time)
import Json.Decode as D

type ProgramTitleId = ProgramTitleId Int
type alias ProgramTitle =
    { start_time : Time
    , end_time : Time
    , channel_id : Int
    , title : String
    , desc : String
    , program_category_id : Int
    , next_counter : Int
    , weekdays : Int
    , auto_next : Bool
    , label_format : String
    , created_at : Time
    , updated_at : Time
    }

    
programTitleDecoder : D.Decoder ProgramTitle
programTitleDecoder =
    map12 ProgramTitle
        (D.at ["start_time"] D.float)
        (D.at ["end_time"] D.float)
        (D.at ["channel_id"] D.int)
        (D.at ["title"] D.string)
        (D.at ["desc"] D.string)
        (D.at ["program_category_id"] D.int)
        (D.at ["next_counter"] D.int)
        (D.at ["weekdays"] D.int)
        (D.at ["auto_next"] D.bool)
        (D.at ["label_format"] D.string)
        (D.at ["created_at"] D.float)
        (D.at ["updated_at"] D.float)
    
