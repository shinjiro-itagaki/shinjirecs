module Records.Reservation exposing (Reservation,ReservationId)
import Time exposing (Time)
import Json.Decode as D
import Utils.Json exposing (map16)

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
    map16
        Reservation
        (D.field "start_time"        D.float)
        (D.field "duration"          D.int)
        (D.field "channel_id"        D.int)
        (D.field "program_title_id"  D.int)
        (D.field "title"             D.string)
        (D.field "desc"              D.string)
        (D.field "event_id"          D.int)
        (D.field "counter"           D.int)
        (D.field "state"             stateDecoder)
        (D.field "command_str"       D.string)
        (D.field "command_pid"       D.int)
        (D.field "log"               D.string)
        (D.field "errror_log"        D.string)
        (D.field "filename"          D.string)
        (D.field "created_at"        D.float)
        (D.field "updated_at"        D.float)
