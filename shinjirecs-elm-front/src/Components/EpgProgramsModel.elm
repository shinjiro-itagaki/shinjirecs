module Components.EpgProgramsModel exposing (..)
import Records.Types exposing (Entity)
import Records.Channel exposing (..)
import Records.EpgProgram exposing (..)
import Http.Progress exposing(Progress(Done,None,Some,Fail))

type alias EpgProgramsModel = 
    { programs : Maybe (List (Entity EpgProgram))
    , channels : Maybe (List (Entity Channel))
    , startProgramsLoading : Bool
    , startChannelsLoading : Bool
    , programsLoading : Progress (List (Entity EpgProgram))
    , channelsLoading : Progress (List (Entity Channel))
    }
