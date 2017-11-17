module Records.ProgramTitle exposing (ProgramTitle,ProgramTitleId)
import Utils.Json exposing (map15,fromTimeToDateDecoder)
import Time exposing (Time)
import Json.Decode as D
import Date exposing (Date)

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
    map15 ProgramTitle
        (D.field "start_at"             D.int) -- 0 ~ (24 * 3600 - 1), from second from 00:00:00
        (D.field "duration"             D.int)
        (D.field "channel_id"           D.int)
        (D.field "title"                D.string)
        (D.field "desc"                 D.string)
        (D.field "program_category_id"  D.int)
        (D.field "next_counter"         D.int)
        (D.field "weekdays"             D.int)
        (D.field "auto_next"            D.bool)
        (D.field "label_format"         D.string)
        (D.field "created_at"           D.float)
        (D.field "updated_at"           D.float)
        (D.field "begin_on"            (D.maybe fromTimeToDateDecoder))
        (D.field "finish_on"           (D.maybe fromTimeToDateDecoder))
        (D.field "dayoffs"             (D.list  fromTimeToDateDecoder))
    
