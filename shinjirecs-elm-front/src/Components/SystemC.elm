module Components.SystemC exposing (accept,init)
import Components.SystemModel exposing (SystemModel)
import Components.Types exposing (Models,CommonModelReadOnly,CommonModelEditable,RootMsg2(DirectMsg,HasCmd,SendRequest,DoNothing,UpdateModel2),Request(NoSelect,ToSystemReq),redirectTo2)
import Components.SystemMsg exposing (SystemMsg(CountUp,SystemInput,DoAction),ActionType(IndexAction,ShowAction,EditAction,ModifyAction))
import Records.Types exposing (Entity)
import Records.ColumnInfo exposing (ColumnInfo)
import Records.System exposing (System,ColumnTarget(AreaId,Active,Setup,TunerCount{- ,RestTunerCount -}),setValue,stringToTarget,toStringMap)
import Records.System as System exposing (new)
import Html exposing (Html,div,input,text,li,Attribute,button)
import Html.Events exposing (onClick)
import Html as H
import Components.Partials exposing (formByColumns,sample,SampleMsg,SampleModel)
import Dict exposing (Dict)
import Utils.Maybe exposing (catMaybes)
import Utils.Either exposing (Either(Left,Right))

linkToButton : ActionType -> String -> Html SystemMsg
linkToButton tgt label = button [ onClick <| DoAction tgt ] [text label]

linkToIndexButton = linkToButton IndexAction "システムメニュー"
linkToShowButton  = linkToButton ShowAction  "システム情報"
linkToEditButton  = linkToButton EditAction  "システム情報編集"
submitEditButton = linkToButton ModifyAction "システム情報更新"

init : SystemModel
init = { show_record = Nothing
       , edit_record = Nothing
       , system_schema  = Nothing
       , previousAction = IndexAction
       , actionType     = IndexAction
       }
                   
accept : ActionType -> Models -> RootMsg2
accept tipe m =
    let html =
            case tipe of
                IndexAction   -> DirectMsg m indexView2
                ShowAction    -> execShowAction2 m
                EditAction    -> execEditAction2 m
                ModifyAction  -> redirectTo2 (ToSystemReq ShowAction)
    in html

execEditAction2 : Models -> RootMsg2
execEditAction2 m = HasCmd <| Cmd.map execEditAction2Impl <| loadSchemaAction2 m
        
execEditAction2Impl : Models -> RootMsg2 -- HasCmd (Cmd (DirectMsg Models (Models -> Html RootMsg2))) 
execEditAction2Impl m =
    let smodel = m.system
        r  = m.readonly
        rw = m.editable
    in Cmd.map (\res -> (case res of
                            Ok newrec   -> DirectMsg {m|system   = {smodel| edit_record = Just newrec}}
                            Err httperr -> DirectMsg {m|editable = {rw| errmsg = r.httpErrorToString httperr }}
                        ) editView2
               ) m.readonly.api.system.get |> HasCmd

loadSchemaAction2 : Models -> Cmd Models
loadSchemaAction2 m =
    let smodel = m.system
        r  = m.readonly
        rw = m.editable
    in Cmd.map (\res -> (case res of
                            Ok newrec   -> {m|system   = {smodel| system_schema = Just newrec}}
                            Err httperr -> {m|editable = {rw| errmsg = r.httpErrorToString httperr }}
                        )
               ) m.readonly.api.system.info
        
execShowAction2 : Models -> RootMsg2
execShowAction2 m =
    let smodel = m.system
        r  = m.readonly
        rw = m.editable
    in Cmd.map (\res -> (case res of
                            Ok newrec   -> DirectMsg {m|system   = {smodel| show_record = Just newrec}}
                            Err httperr -> DirectMsg {m|editable = {rw| errmsg = r.httpErrorToString httperr }}
                        ) showView2
               ) m.readonly.api.system.get |> HasCmd
        
indexView2 : Models -> Html RootMsg2
indexView2 m =
    div [] [
         H.span [] [text <| "index"]
        ,button [onClick <| SendRequest <| ToSystemReq ShowAction] [text "システム情報表示"]
        ,button [onClick <| SendRequest <| ToSystemReq EditAction] [text "システム情報編集"]
        ]

showView2 : Models -> Html RootMsg2
showView2 m =
    div [] [
         H.span [] [text <| if m.system.show_record == Nothing then "ロード失敗した" else "ロード済み"]
        ,button [onClick <| SendRequest <| ToSystemReq IndexAction] [text "システム情報インデックスへ"]
        ]

editView2 : Models -> Html RootMsg2
editView2 m =
    div [] [
         H.div [] [text <| "システム編集"]
        ,(case m.system.system_schema of
              Just rec -> editViewIfSystemSchemaLoaded rec
              Nothing  -> editViewIfSystemSchemaNotFound) m.system
        ,button [onClick <| SendRequest <| ToSystemReq IndexAction] [text "システム情報インデックスへ"] 
        ]

editViewIfSystemSchemaNotFound : SystemModel -> Html RootMsg2
editViewIfSystemSchemaNotFound m =
    div [] [
         H.div [] [text <| "スキーマがロードされていません"]
        ]
editViewIfSystemSchemaLoaded : Dict String ColumnInfo -> SystemModel -> Html RootMsg2
editViewIfSystemSchemaLoaded scm m =
    (case  m.edit_record of
         Just rec -> editViewImpl rec
         Nothing  -> editViewIfRecordNotFound) m

editViewIfRecordNotFound : SystemModel -> Html RootMsg2
editViewIfRecordNotFound m =
    div [] [
         H.div [] [text <| "データがロードされていません"]
        ]
editViewImpl : Entity System -> SystemModel -> Html RootMsg2
editViewImpl rec m =
    div [] [text <| "データはロード済み"]
