module Records.EpgProgram exposing (EpgProgram,EpgProgramId)
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
        (D.field "start_time"               D.float)
        (D.field "stop_time"                D.float)
        (D.field "channel_id"               D.int)
        (D.field "title"                    D.string)
        (D.field "desc"                     D.string)
        (D.field "event_id"                 D.int)
        (D.field "epg_program_category_id"  D.int)
        (D.field "created_at"               D.float)
        (D.field "updated_at"               D.float)
