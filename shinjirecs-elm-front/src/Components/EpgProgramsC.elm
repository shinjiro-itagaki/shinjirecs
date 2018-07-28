module Components.EpgProgramsC exposing (new)
import Components.EpgProgramsMsg exposing (ActionType(IndexAction,SearchAction,MakeReservationAction))
import Components.EpgProgramsModel exposing (EpgProgramsModel,enableChannels,shownChannels,channelPrograms)
import Components.Types exposing (Component,Models,CommonModelReadOnly,CommonModelEditable,PublicRootMsg(DirectMsg,HasCmd,SendRequest,DoNothing,UpdateModel),Request(NoSelect,ToEpgProgramsReq),redirectTo,countUp,countDown,countPlus)
import Html exposing (Html,div,input,text,li,Attribute,button,ol)
import Html as H
import Html.Events exposing (onClick,onCheck)
import Html.Attributes exposing (type_,name,value)
import Http exposing(Error)
import Http.Progress exposing(Progress(Done,None,Some,Fail))
import Records.Types exposing (Entity)
import Records.EpgProgram exposing (EpgProgram,asc,desc,durationS,durationMS)
import Records.Channel exposing (Channel,enables)
import Time exposing (every,second)
import Utils.List as List
import Utils.DateTime exposing (timeToStringJa)
import String exposing (join,concat)
import MainCssInterface exposing (CssClasses(EpgHovered),CssIds(..))
import Html.CssHelpers exposing (withNamespace)
import Css exposing (height,px,em)
import Css.Foreign exposing (class,id,selector,everything,media) -- http://package.elm-lang.org/packages/rtfeldman/elm-css/13.0.1/Css-Foreign
import Html.Styled.Attributes exposing (css)
import Html.Styled exposing (toUnstyled,styled)
import Html.Styled as HS

{ id, class, classList } = withNamespace ""

new : Component EpgProgramsModel ActionType
new = { init = init, accept = accept, subscriptions = subscriptions }

init : EpgProgramsModel
init = { programs = Nothing
       , channels = Nothing
       , shownChannelsList = []
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
        rw    = m.editable
    in DirectMsg {m| epgPrograms = {model | startProgramsLoading = True, startChannelsLoading = True }} listView

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
--         button [onClick <| HasCmd (Cmd PublicRootMsg) SendRequest <| ToSystemReq EditAction] [text "Epgdomp"]
         H.div [] [text <| "プログラム件数：" ++ (case m.epgPrograms.programs of
                                                      Just recs -> (\s -> s ++ "件") <| toString <| List.length recs
                                                      Nothing -> "不明（データが読み込まれていません）"
                                                 )]
        ,H.div [] [text <| "チャンネル件数：" ++ (case m.epgPrograms.channels of
                                                      Just recs -> (\s -> s ++ "件") <| toString <| List.length recs
                                                      Nothing -> "不明（データが読み込まれていません）"
                                                 )]
        ,H.div [] [listViewMain m]
        ,H.div [] [ listViewProgramsLoading m.epgPrograms.programsLoading
                  , listViewChannelsLoading m.epgPrograms.channelsLoading
                  ]
--        ,button [onClick <| SendRequest <| ToEpgProgramsReq IndexAction] [text "EPGプログラム一覧へ"]
        ]        

msgOnCheckChannel : Entity Channel -> Models -> Bool -> PublicRootMsg
msgOnCheckChannel c m b =
    let model   = m.epgPrograms
        shown  = model.shownChannelsList
        shown2 = if b then (c.id :: shown) else List.filter ((/=) c.id) shown
    in UpdateModel <| (if b then countUp else countDown) <| {m|epgPrograms = {model|shownChannelsList = shown2 }}
            
listViewMain : Models -> Html PublicRootMsg
listViewMain m =
    let model = m.epgPrograms
        channels = enableChannels m.epgPrograms
        shown    = shownChannels m.epgPrograms
        programs = model.programs
        r  = m.readonly
        rw = m.editable
    in div [] [
         div [] <| List.map (\c -> H.label
                                 [ onCheck (msgOnCheckChannel c m)]
                                 [ input [ type_ "checkbox", name "shownChannels"] []
                                 , text c.val.display_name
                                 ]
                            ) channels
        ,div [] <| List.map (\c ->
                                 let programs = desc <| channelPrograms model c
                                 in ol [ name "channel_id", value (toString c.id) ]
                                     <| (div [] [text <| (\s -> s++"件") <| toString <| List.length programs]) :: (List.map (\p -> li [] [viewProgram p]) <| programs)
                            ) shown
        ]

viewProgram : Entity EpgProgram -> Html PublicRootMsg
viewProgram ep =
    let p = ep.val
        (m,s) = durationMS p
    in  HS.toUnstyled
        <| styled
            HS.dl
            [height (em <| toFloat m)]
            [Html.Styled.Attributes.fromUnstyled (class [EpgHovered])]
        <| List.map HS.fromUnstyled
            [ (H.dt [] [text <| "タイトル"])
            , (H.dd [] [text <| p.title])
            , (H.dt [] [text <| "日時"])
            , (H.dd [] [H.div [] [text <| concat [timeToStringJa p.start_time, "〜", timeToStringJa p.stop_time]]
                       ,H.div [] [text <| concat ["(",toString m,"分",toString s,"秒)"]]
                       ])                
            , (H.dt [] [text <| "内容"])
            , (H.dd [] [text <| p.desc])
            , (H.dt [] [text <| "event_id"])
            , (H.dd [] [text <| toString <| p.event_id])
            , (H.dt [] [text <| "id"])
            , (H.dd [] [text <| toString <| ep.id])
            ]
    -- , event_id : Int
    -- , epg_program_categories : List Int
    -- , epg_program_medium_categories : List Int
                    
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
    H.span [] [text "動作なし"]
        
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

updateProgramsByProgress : Models -> Progress (List (Entity EpgProgram)) -> Models
updateProgramsByProgress m p =
    let model_ = m.epgPrograms
        model  = { model_ | programsLoading = p }
    in case p of
           Done list -> {m| epgPrograms = { model | programs = Just list, startProgramsLoading = False }}
           _         -> {m| epgPrograms = model }
                        
updateChannelsByProgress : Models -> Progress (List (Entity Channel)) -> Models
updateChannelsByProgress m p =
    let model_ = m.epgPrograms
        model  = { model_ | channelsLoading = p }
    in case p of
           Done list -> {m| epgPrograms = { model | channels = Just list, startChannelsLoading = False }}
           _         -> {m| epgPrograms = model }

subscriptions : Models -> Sub PublicRootMsg
subscriptions m =
    let model = m.epgPrograms
    in case (model.startProgramsLoading, model.startChannelsLoading,model.programsLoading,model.channelsLoading) of
           (True,_   ,_,_) -> Sub.map (\p -> UpdateModel <| updateProgramsByProgress m p) <| m.readonly.api.epgPrograms.indexAsync Nothing
           (_   ,True,_,_) -> Sub.map (\p -> UpdateModel <| updateChannelsByProgress m p) <| m.readonly.api.channels.indexAsync Nothing
           _ -> Sub.none
