module Models.Reservation exposing (Reservation)
import Time exposing (Time)

type State = Canceled -- -2
           | Failed -- -1
           | Waiting -- 0
           | Preparing -- 1
           | Recording -- 2
           | Success -- 3

toState : Int -> State
toState -2 = Canceled
toState -1 = Failed
toState  0 = Waiting
toState  1 = Preparing
toState  2 = Recording
toState  3 = Success
    
type Reservation =
    { id : Int
    , start_time : Time
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

