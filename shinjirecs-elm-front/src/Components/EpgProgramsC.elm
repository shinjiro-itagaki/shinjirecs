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
init = { programs = [] }
                   
accept : ActionType -> Models -> PublicRootMsg
accept tipe m =
    case tipe of
        IndexAction  -> execIndexAction m
        SearchAction -> execIndexAction m
        MakeReservationAction -> execIndexAction m

execIndexAction : Models -> PublicRootMsg
execIndexAction m = 
    let model = m.epgPrograms
        r  = m.readonly
        rw = m.editable
    in HasCmd <| Cmd.map
        (\res ->
             (case res of
                  Ok newrecs  -> DirectMsg {m|epgPrograms = {model| programs = newrecs}}
                  Err httperr -> DirectMsg {m|editable = {rw| errmsg = r.httpErrorToString httperr }}
             ) listView
        ) (m.readonly.api.epgPrograms.index Nothing)

listView : Models -> Html PublicRootMsg
listView m =
    div [] [
         H.span [] [text <| "件数：" ++ (toString <| List.length m.epgPrograms.programs) ++ "件"]
--        ,button [onClick <| SendRequest <| ToEpgProgramsReq IndexAction] [text "EPGプログラム一覧へ"]
        ]        
