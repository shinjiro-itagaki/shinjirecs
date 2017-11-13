module Models.Program exposing (Program,ProgramId)
import Time exposing (Time)
import Models.Types exposing (map9)
import Json.Decode as D

type ProgramId = ProgramId Int
type alias Program =
    { start_time : Time
    , stop_time : Time
    , channel_id : Int 
    , title : String
    , desc : String
    , event_id : Int
    , program_category_id : Int
    , created_at : Time
    , updated_at : Time
    }

programDecoder : D.Decoder Program
programDecoder =
    map9
        Program
        (D.at ["start_time"] D.float)
        (D.at ["stop_time"] D.float)
        (D.at ["channel_id"] D.int)
        (D.at ["title"] D.string)
        (D.at ["desc"] D.string)
        (D.at ["event_id"] D.int)
        (D.at ["program_category_id"] D.int)
        (D.at ["created_at"] D.float)
        (D.at ["updated_at"] D.float)
