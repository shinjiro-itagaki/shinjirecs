module Records.Reservation exposing (Reservation,ReservationId)
import Time exposing (Time)
import Json.Decode as D
import Utils.Json exposing (map16)
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
    , errror_log : String 
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
        |> required "errror_log"        D.string
        |> required "filename"          D.string
        |> required "created_at"        D.float
        |> required "updated_at"        D.float
