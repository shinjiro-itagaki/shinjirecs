module Models.ProgramTitle exposing (ProgramTitle)
import Time exposing (Time)

type ProgramTitle =
    { id : Int
    , start_time : Time
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
