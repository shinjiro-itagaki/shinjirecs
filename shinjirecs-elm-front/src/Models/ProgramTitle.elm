module Models.ProgramTitle exposing (ProgramTitle,ProgramTitleId)
import Models.Types exposing (map15,fromTimeToDateDecoder)
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
        (D.at ["start_at"] D.int) -- 0 ~ (24 * 3600 - 1), from second from 00:00:00
        (D.at ["duration"] D.int)
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
        (D.at ["begin_on"]  (D.maybe fromTimeToDateDecoder))
        (D.at ["finish_on"] (D.maybe fromTimeToDateDecoder))
        (D.at ["dayoffs"]   (D.list  fromTimeToDateDecoder))
    
