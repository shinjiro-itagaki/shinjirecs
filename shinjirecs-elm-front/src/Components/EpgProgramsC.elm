module Components.EpgProgramsC exposing (new)
import Components.EpgProgramsMsg exposing (ActionType(IndexAction,SearchAction,MakeReservationAction))
import Components.EpgProgramsModel exposing (EpgProgramsModel)
import Components.Types exposing (Component,Models,CommonModelReadOnly,CommonModelEditable,PublicRootMsg(DirectMsg,HasCmd,SendRequest,DoNothing,UpdateModel),Request(NoSelect,ToEpgProgramsReq),redirectTo)
import Html exposing (Html,div,input,text,li,Attribute,button)
import Html.Events exposing (onClick)
import Html as H

new : Component EpgProgramsModel ActionType
new = { init = init, accept = accept }

init : EpgProgramsModel
init = { programs = Nothing
       , channels = Nothing
       , nowLoading = False
       }
                   
accept : ActionType -> Models -> PublicRootMsg
accept tipe m =
    case tipe of
        IndexAction  -> execIndexAction m
        SearchAction -> execIndexAction m
        MakeReservationAction -> execIndexAction m

cmdMapIfOk : (Models -> PublicRootMsg) -> Cmd (Result Models Models) -> PublicRootMsg
cmdMapIfOk f cmd = HasCmd
                   <| Cmd.map
                       (\res ->
                            case res of
                                Ok  m -> f m
                                Err m -> let mm = m.epgPrograms in UpdateModel { m | epgPrograms = {mm | nowLoading = False }}
                       ) cmd
                                 
execIndexAction : Models -> PublicRootMsg
execIndexAction m =
    let model = m.epgPrograms
        {programs,channels,nowLoading} = model
    in case (programs,channels,nowLoading) of
           (_      , _      , False) -> execIndexAction {m| epgPrograms = { model | nowLoading = True }}
           (Nothing, _      , _    ) -> cmdMapIfOk execIndexAction <| loadEpgPrograms m
           (_      , Nothing, _    ) -> cmdMapIfOk execIndexAction <| loadChannels m
           (Just px, Just cx, _    ) -> DirectMsg {m| epgPrograms = { model | nowLoading = False }} listView

loadEpgPrograms : Models -> Cmd (Result Models Models)
loadEpgPrograms m =
    let model = m.epgPrograms
        r  = m.readonly
        rw = m.editable
    in Cmd.map
        (\res ->
             (case res of
                  Ok newrecs  -> Ok  {m|epgPrograms = {model| programs = Just newrecs}}
                  Err httperr -> Err {m|editable = {rw| errmsg = r.httpErrorToString httperr }}
             )
        ) (m.readonly.api.epgPrograms.index Nothing)

loadChannels : Models -> Cmd (Result Models Models)
loadChannels m =
    let model = m.epgPrograms
        r  = m.readonly
        rw = m.editable
    in Cmd.map
        (\res ->
             (case res of
                  Ok newrecs  -> Ok  {m|epgPrograms = {model| channels = Just newrecs}}
                  Err httperr -> Err {m|editable = {rw| errmsg = r.httpErrorToString httperr }}
             )
        ) (m.readonly.api.channels.index Nothing)
        
listView : Models -> Html PublicRootMsg
listView m =
    div [] [
         H.span [] [text <| "件数：" ++ (case m.epgPrograms.programs of
                                             Just recs -> (\s -> s ++ "件") <| toString <| List.length recs
                                             Nothing -> "不明（データが読み込まれていません）"
                                        )]
        ,H.div [] [text <| if m.epgPrograms.nowLoading then "読み込み中..." else "" ]
--        ,button [onClick <| SendRequest <| ToEpgProgramsReq IndexAction] [text "EPGプログラム一覧へ"]
        ]        
