module Records.ProgramSeries exposing (ProgramSeries,ProgramSeriesId,programSeriesDecoder,programSeriesEncoder)
import Utils.Json exposing (fromTimeToDateDecoder,dateEncoder)
import Time exposing (Time)
import Json.Decode as D
import Date exposing (Date)
import Json.Decode.Pipeline exposing (decode,required,optional)
import Json.Encode as E
import Utils.Maybe exposing (ifJust)
import Utils.Json exposing (Encoder)

type ProgramSeriesId = ProgramSeriesId Int
type alias ProgramSeries =
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

programSeriesDecoder : D.Decoder ProgramSeries
programSeriesDecoder =
    decode ProgramSeries
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
    
programSeriesEncoder : Encoder ProgramSeries
programSeriesEncoder x = E.object
                  [ ("start_at",              E.int x.start_at)
                  , ("duration",              E.int x.duration)
                  , ("channel_id",            E.int x.channel_id)
                  , ("title",                 E.string x.title)
                  , ("desc",                  E.string x.desc)
                  , ("program_category_id",   E.int x.program_category_id)
                  , ("next_counter",          E.int x.next_counter)
                  , ("weekdays",              E.int x.weekdays)
                  , ("auto_next",             E.bool x.auto_next)
                  , ("label_format",          E.string x.label_format)
                  , ("created_at",            E.float x.created_at)
                  , ("updated_at",            E.float x.updated_at)
                  , ("begin_on",              ifJust dateEncoder x.begin_on  E.null)
                  , ("finish_on",             ifJust dateEncoder x.finish_on E.null)
                  , ("dayoffs",               E.list <| List.map dateEncoder x.dayoffs)
                  ]
