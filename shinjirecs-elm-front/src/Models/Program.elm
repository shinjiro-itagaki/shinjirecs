module Models.Program exposing (Program)
import Time exposing (Time)

type Program =
    { id : Int
    , start_time : Time
    , stop_time : Time
    , channel_id : Int 
    , title : String
    , desc : String
    , event_id : Int
    , program_category_id : Int
    , created_at : Time
    , updated_at : Time
    }
