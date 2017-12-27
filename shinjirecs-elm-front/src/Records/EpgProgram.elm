module Records.EpgProgram exposing (EpgProgram,EpgProgramId,epgProgramDecoder,epgProgramEncoder,asc,desc,durationS,durationMS)
import Time exposing (Time)
import Json.Decode as D
import Json.Encode as E
import Utils.Json exposing (Encoder)
import Json.Decode.Pipeline exposing (decode,required,optional)
import Records.Types exposing (Entity)

type EpgProgramId = EpgProgramId Int
type alias EpgProgram =
    { start_time : Time
    , stop_time : Time
    , channel_id : Int 
    , title : String
    , desc : String
    , event_id : Int
    , epg_program_categories : List Int
    , epg_program_medium_categories : List Int
    , created_at : Time
    , updated_at : Time
    }

epgProgramDecoder : D.Decoder EpgProgram
epgProgramDecoder =
    decode EpgProgram
        |> required "start_time"               D.float
        |> required "stop_time"                D.float
        |> required "channel_id"               D.int
        |> required "title"                    D.string
        |> required "desc"                     D.string
        |> required "event_id"                 D.int
        |> required "epg_program_categories"         (D.list D.int)
        |> required "epg_program_medium_categories"  (D.list D.int)
        |> required "created_at"               D.float
        |> required "updated_at"               D.float

epgProgramEncoder : Encoder EpgProgram
epgProgramEncoder x = E.object []
                  -- [ ("start_time", E.float x.start_time)
                  -- , ("stop_time", E.float x.stop_time)
                  -- , ("channel_id", E.int  x.channel_id)
                  -- , ("title", E.string  x.title)
                  -- , ("desc", E.string  x.desc)
                  -- , ("event_id", E.int  x.channel_id)
                  -- , ("epg_program_category_id", E.int  x.epg_program_category_id)
                  -- , ("created_at"  , E.float x.created_at)
                  -- , ("updated_at", E.float x.updated_at)
                  -- ]
           
desc : List (Entity EpgProgram) -> List (Entity EpgProgram)
desc = List.sortBy (\e -> -e.val.start_time)
       
asc : List (Entity EpgProgram) -> List (Entity EpgProgram)       
asc = List.sortBy (\e -> e.val.start_time)

durationS : EpgProgram -> Int
durationS p = truncate <| p.stop_time - p.start_time

durationMS : EpgProgram -> (Int,Int)
durationMS p = let sec = durationS p in (sec // 60, sec % 60)
