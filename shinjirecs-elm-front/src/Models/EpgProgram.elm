module Models.EpgProgram exposing (EpgProgram,EpgProgramId)
import Time exposing (Time)
import Json.Decode as D
import Utils.Json exposing (map9,Encoder)

type EpgProgramId = EpgProgramId Int
type alias EpgProgram =
    { start_time : Time
    , stop_time : Time
    , channel_id : Int 
    , title : String
    , desc : String
    , event_id : Int
    , epg_program_category_id : Int
    , created_at : Time
    , updated_at : Time
    }

programDecoder : D.Decoder EpgProgram
programDecoder =
    map9
        EpgProgram
        (D.at ["start_time"] D.float)
        (D.at ["stop_time"] D.float)
        (D.at ["channel_id"] D.int)
        (D.at ["title"] D.string)
        (D.at ["desc"] D.string)
        (D.at ["event_id"] D.int)
        (D.at ["epg_program_category_id"] D.int)
        (D.at ["created_at"] D.float)
        (D.at ["updated_at"] D.float)
