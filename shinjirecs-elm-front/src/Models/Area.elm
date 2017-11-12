module Models.Area exposing (Area)
import Time exposing (Time)

type Area =
    { id : Int
    , label : String
    , channels_checked : Bool
    , created_at : Time
    , updated_at : Time
    }
