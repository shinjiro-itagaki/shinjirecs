module Components.EpgProgramsModel exposing (..)
import Records.Types exposing (Entity)
import Records.Channel exposing (..)
import Records.Channel as Channel
import Records.EpgProgram exposing (..)
import Http.Progress exposing(Progress(Done,None,Some,Fail))
import Dict exposing (Dict)
import Utils.List as List
import Utils.Maybe as Maybe

type alias EpgProgramsModel = 
    { programs : Maybe (List (Entity EpgProgram))
    , channels : Maybe (List (Entity Channel))
    , shownChannelsList : List Int
    , startProgramsLoading : Bool
    , startChannelsLoading : Bool
    , programsLoading : Progress (List (Entity EpgProgram))
    , channelsLoading : Progress (List (Entity Channel))
    }

enableChannels : EpgProgramsModel -> List (Entity Channel)
enableChannels model = Channel.enables <| List.fromMaybe model.channels
    
shownChannels : EpgProgramsModel -> List (Entity Channel)
shownChannels m =
    case m.channels of
        Just ls -> Maybe.catMaybes <| List.map (\id -> List.find (\e -> id == e.id) ls) m.shownChannelsList
        Nothing -> []

channelPrograms : EpgProgramsModel -> Entity Channel -> List (Entity EpgProgram)
channelPrograms m ech =
    List.filter (\ep -> ep.val.channel_id == ech.id) <| List.fromMaybe m.programs
