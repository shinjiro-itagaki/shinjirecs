module Records.Reservation exposing (Reservation,ReservationId,reservationDecoder,reservationEncoder)
import Time exposing (Time)
import Json.Decode as D
import Json.Encode as E
import Utils.Json exposing (Encoder)
import Json.Decode.Pipeline exposing (decode,required,optional)

type State = Canceled -- -2
           | Failed -- -1
           | Waiting -- 0
           | Preparing -- 1
           | Recording -- 2
           | Success -- 3

toState : Int -> State
toState i = case i of
                (-2) -> Canceled
                (-1) -> Failed
                0 -> Waiting
                1 -> Preparing
                2 -> Recording
                3 -> Success
                _ -> Waiting

stateToInt : State -> Int
stateToInt s =
    case s of
        Canceled  -> (-2)
        Failed    -> (-1)
        Waiting   -> 0
        Preparing -> 1
        Recording -> 2
        Success   -> 3
                     
type ReservationId = ReservationId Int
type alias Reservation =
    { start_time : Time
    , duration : Int
    , channel_id : Int
    , program_title_id : Int
    , title : String
    , desc : String
    , event_id : Int
    , counter : Int
    , state : State
    , command_str : String
    , command_pid : Int
    , log : String
    , error_log : String 
    , filename : String
    , created_at : Time
    , updated_at : Time
    }
stateDecoder : D.Decoder State
stateDecoder = D.int |> D.andThen (D.succeed << toState)
            
reservationDecoder : D.Decoder Reservation
reservationDecoder =
    decode Reservation
        |> required "start_time"        D.float
        |> required "duration"          D.int
        |> required "channel_id"        D.int
        |> required "program_title_id"  D.int
        |> required "title"             D.string
        |> required "desc"              D.string
        |> required "event_id"          D.int
        |> required "counter"           D.int
        |> required "state"             stateDecoder
        |> required "command_str"       D.string
        |> required "command_pid"       D.int
        |> required "log"               D.string
        |> required "error_log"         D.string
        |> required "filename"          D.string
        |> required "created_at"        D.float
        |> required "updated_at"        D.float

reservationEncoder : Encoder Reservation
reservationEncoder x = E.object
                  [ ("start_time", E.float x.start_time)
                  , ("duration", E.int  x.duration) 
                  , ("channel_id", E.int  x.channel_id) 
                  , ("program_title_id", E.int  x.program_title_id)
                  , ("title", E.string  x.title)
                  , ("desc", E.string  x.desc) 
                  , ("event_id", E.int  x.event_id) 
                  , ("counter", E.int  x.counter) 
                  , ("state", E.int <| stateToInt <| x.state) 
                  , ("command_str", E.string  x.command_str)
                  , ("command_pid", E.int  x.command_pid)
                  , ("log", E.string x.log)
                  , ("error_log", E.string x.error_log)
                  , ("filename", E.string  x.filename) 
                  , ("created_at", E.float x.created_at)
                  , ("updated_at", E.float x.updated_at)
                  ]
           
