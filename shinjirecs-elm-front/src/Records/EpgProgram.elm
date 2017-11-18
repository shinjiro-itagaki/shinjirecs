module Records.EpgProgram exposing (EpgProgram,EpgProgramId)
import Time exposing (Time)
import Json.Decode as D
import Utils.Json exposing (map9,Encoder)
import Json.Decode.Pipeline exposing (decode,required,optional)

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
    decode EpgProgram
        |> required "start_time"               D.float
        |> required "stop_time"                D.float
        |> required "channel_id"               D.int
        |> required "title"                    D.string
        |> required "desc"                     D.string
        |> required "event_id"                 D.int
        |> required "epg_program_category_id"  D.int
        |> required "created_at"               D.float
        |> required "updated_at"               D.float
