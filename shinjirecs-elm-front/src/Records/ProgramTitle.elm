module Records.ProgramTitle exposing (ProgramTitle,ProgramTitleId)
import Utils.Json exposing (map15,fromTimeToDateDecoder)
import Time exposing (Time)
import Json.Decode as D
import Date exposing (Date)
import Json.Decode.Pipeline exposing (decode,required,optional)

type ProgramTitleId = ProgramTitleId Int
type alias ProgramTitle =
    { start_at : Int
    , duration : Int
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
    , begin_on  : Maybe Date
    , finish_on : Maybe Date
    , dayoffs : List Date
    }

programTitleDecoder : D.Decoder ProgramTitle
programTitleDecoder =
    decode ProgramTitle
        |> required "start_at"             D.int -- 0 ~ (24 * 3600 - 1), from second from 00:00:00
        |> required "duration"             D.int
        |> required "channel_id"           D.int
        |> required "title"                D.string
        |> required "desc"                 D.string
        |> required "program_category_id"  D.int
        |> required "next_counter"         D.int
        |> required "weekdays"             D.int
        |> required "auto_next"            D.bool
        |> required "label_format"         D.string
        |> required "created_at"           D.float
        |> required "updated_at"           D.float
        |> optional "begin_on"             (D.maybe fromTimeToDateDecoder) Nothing
        |> optional "finish_on"            (D.maybe fromTimeToDateDecoder) Nothing
        |> optional "dayoffs"              (D.list fromTimeToDateDecoder) []
    
