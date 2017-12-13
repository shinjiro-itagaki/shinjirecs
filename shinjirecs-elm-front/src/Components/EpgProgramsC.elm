module Components.EpgProgramsC exposing (new)
import Components.EpgProgramsMsg exposing (ActionType(IndexAction,SearchAction,MakeReservationAction))
import Components.EpgProgramsModel exposing (EpgProgramsModel)
import Components.Types exposing (Component,Models,CommonModelReadOnly,CommonModelEditable,PublicRootMsg(DirectMsg,HasCmd,SendRequest,DoNothing,UpdateModel),Request(NoSelect,ToEpgProgramsReq),redirectTo)
import Html exposing (Html,div,input,text,li,Attribute,button)
import Html.Events exposing (onClick)
import Html as H
import Http exposing(Error)
import Http.Progress exposing(Progress(Done,None,Some,Fail))
import Records.Types exposing (Entity)
import Records.EpgProgram exposing (EpgProgram)
import Records.Channel exposing (Channel)

new : Component EpgProgramsModel ActionType
new = { init = init, accept = accept, subscriptions = subscriptions }

init : EpgProgramsModel
init = { programs = Nothing
       , channels = Nothing
       , startProgramsLoading = False
       , startChannelsLoading = False
       , programsLoading = None
       , channelsLoading = None
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
                                Err m -> let mm = m.epgPrograms in UpdateModel { m | epgPrograms = {mm | programsLoading = None }}
                       ) cmd

execIndexAction : Models -> PublicRootMsg
execIndexAction m =
    let model = m.epgPrograms
    in case ((model.startProgramsLoading,model.programs),(model.startChannelsLoading,model.channels)) of
           ((False, Nothing),(False, Nothing)) -> UpdateModel { m | epgPrograms = {model | startProgramsLoading = True, startChannelsLoading = True }}           
           ((False, Nothing), _              ) -> UpdateModel { m | epgPrograms = {model | startProgramsLoading = True }}
           (_               ,(False, Nothing)) -> UpdateModel { m | epgPrograms = {model | startChannelsLoading = True }}
           _                                   -> DoNothing

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
        ,H.div [] [ listViewProgramsLoading m.epgPrograms.programsLoading
                  , listViewChannelsLoading m.epgPrograms.channelsLoading
                  ]
--        ,button [onClick <| SendRequest <| ToEpgProgramsReq IndexAction] [text "EPGプログラム一覧へ"]
        ]        

listViewProgramsLoading : Progress (List (Entity EpgProgram)) -> Html PublicRootMsg
listViewProgramsLoading p =
    case p of
        None -> listViewWhenNone
        Some info -> listViewWhenSome info
        Fail err -> listViewWhenFail err
        Done list -> listViewWhenDone

listViewChannelsLoading : Progress (List (Entity Channel)) -> Html PublicRootMsg
listViewChannelsLoading p =
    case p of
        None -> listViewWhenNone
        Some info -> listViewWhenSome info
        Fail err -> listViewWhenFail err
        Done list -> listViewWhenDone
                     
listViewWhenNone : Html PublicRootMsg
listViewWhenNone =
    H.span [] [text ""]
        
listViewWhenSome : { bytes : Int, bytesExpected : Int } -> Html PublicRootMsg
listViewWhenSome {bytes, bytesExpected} =
    H.span [] [text <| (toString bytes) ++ " / " ++ (toString bytesExpected) ++ " ( " ++ (toString <| 100.0 * (toFloat bytes) / (toFloat bytesExpected)) ++ " % )"]
        
listViewWhenFail : Error -> Html PublicRootMsg
listViewWhenFail err =
    H.span [] [text "読み込みエラー"]
        
listViewWhenDone : Html PublicRootMsg
listViewWhenDone =
    H.span [] [text "読み込み完了"]
    
--    None | Some { bytes : Int, bytesExpected : Int } | Fail Error | Done data
            
subscriptions : Models -> Sub PublicRootMsg
subscriptions m =
    let model = m.epgPrograms
        {programs,channels,startProgramsLoading,startChannelsLoading,programsLoading,channelsLoading} = model
    in case ((startProgramsLoading,programs,programsLoading),(startChannelsLoading,channels,channelsLoading)) of
           ((True,_,None),_            ) -> Sub.map (\p -> UpdateModel {m| epgPrograms = { model | programsLoading = p}} ) <| m.readonly.api.epgPrograms.indexAsync Nothing
           (_            ,(True,_,None)) -> Sub.map (\p -> UpdateModel {m| epgPrograms = { model | channelsLoading = p}} ) <| m.readonly.api.channels.indexAsync Nothing
           _                             -> Sub.none
