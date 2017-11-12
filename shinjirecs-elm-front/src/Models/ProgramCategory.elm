module Models.ProgramCategory exposing (ProgramCategory)
import Time exposing (Time)

type ProgramCategory =
    { id : Int
    , label : String
    , created_at : Time
    , updated_at : Time
    }
