module Models.System exposing (System)
import Time exposing (Time)

type System =
    { id : Int
    , area_id : Int
    , active : Bool
    , setup  : Bool
    , gr_tuner_count : Int
    , bs_tuner_count : Int
    , rest_gr_tuner_count : Int
    , rest_bs_tuner_count : Int
    , created_at : Time
    , updated_at : Time
    , setup : Bool
    }
    
