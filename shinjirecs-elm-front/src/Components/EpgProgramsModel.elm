module Components.EpgProgramsModel exposing (..)
import Records.Types exposing (Entity)
import Records.Channel exposing (..)
import Records.EpgProgram exposing (..)

type alias EpgProgramsModel = 
    { programs : Maybe (List (Entity EpgProgram))
    , channels : Maybe (List (Entity Channel))
    , nowLoading : Bool
    }
