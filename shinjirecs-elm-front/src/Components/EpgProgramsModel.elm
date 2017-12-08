module Components.EpgProgramsModel exposing (..)
import Records.Types exposing (Entity)
import Records.EpgProgram exposing (..)

type alias EpgProgramsModel =
    {
        programs : List (Entity EpgProgram)
    }
