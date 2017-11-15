module Models.Reservation exposing (Reservation,ReservationId)
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
        (D.at ["start_time"] D.float)
        (D.at ["duration"] D.int)
        (D.at ["channel_id"] D.int)
        (D.at ["program_title_id"] D.int)
        (D.at ["title"] D.string)
        (D.at ["desc"] D.string)
        (D.at ["event_id"] D.int)
        (D.at ["counter"] D.int)
        (D.at ["state"] stateDecoder)
        (D.at ["command_str"] D.string)
        (D.at ["command_pid"] D.int)
        (D.at ["log"] D.string)
        (D.at ["errror_log"] D.string)
        (D.at ["filename"] D.string)
        (D.at ["created_at"] D.float)
        (D.at ["updated_at"] D.float)
