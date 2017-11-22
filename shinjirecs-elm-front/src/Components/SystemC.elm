module Components.SystemC exposing (new)
import Components.SystemModel exposing (SystemModel)
import Components.Types exposing (Models,CommonModelReadOnly,CommonModelEditable,PublicRootMsg(DirectMsg,HasCmd,SendRequest,DoNothing,UpdateModel),Request(NoSelect,ToSystemReq),redirectTo)
import Components.SystemMsg exposing (SystemMsg(CountUp,SystemInput,DoAction),ActionType(IndexAction,ShowAction,EditAction,ModifyAction))
import Records.Types exposing (Entity)
import Records.ColumnInfo exposing (ColumnInfo)
import Records.System exposing (System,ColumnTarget(AreaId,Setup,TunerCount{- ,RestTunerCount -}),setValue,stringToTarget,toStringMap)
import Records.System as System exposing (new)
import Html exposing (Html,div,input,text,li,Attribute,button)
import Html.Events exposing (onClick)
import Html as H
import Components.Partials exposing (formByColumns,sample,SampleMsg,SampleModel)
import Dict exposing (Dict)
import Utils.Maybe exposing (catMaybes)
import Utils.Either exposing (Either(Left,Right))
import Components.Types exposing (Component)

new : Component SystemModel ActionType
new = { init = init, accept = accept }

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
                   
accept : ActionType -> Models -> PublicRootMsg
accept tipe m =
    let html =
            case tipe of
                IndexAction   -> DirectMsg m indexView
                ShowAction    -> execShowAction m
                EditAction    -> execEditAction m
                ModifyAction  -> execModifyAction m
    in html

execModifyAction : Models -> PublicRootMsg
execModifyAction m =
    case m.system.edit_record of
        Just erec ->
            HasCmd
            <| Cmd.map
                (\res ->
                     case res of
                         Ok _  -> redirectTo (ToSystemReq ShowAction)
                         Err _ -> redirectTo (ToSystemReq EditAction)
                )
            <| m.readonly.api.system.modify erec
        Nothing -> redirectTo (ToSystemReq EditAction)
        
execEditAction : Models -> PublicRootMsg
execEditAction m = HasCmd <| Cmd.map execEditActionImpl <| loadSchemaAction m
        
execEditActionImpl : Models -> PublicRootMsg -- HasCmd (Cmd (DirectMsg Models (Models -> Html PublicRootMsg))) 
execEditActionImpl m =
    let smodel = m.system
        r  = m.readonly
        rw = m.editable
    in Cmd.map (\res -> (case res of
                            Ok newrec   -> DirectMsg {m|system   = {smodel| edit_record = Just newrec}}
                            Err httperr -> DirectMsg {m|editable = {rw| errmsg = r.httpErrorToString httperr }}
                        ) editView
               ) m.readonly.api.system.get |> HasCmd

loadSchemaAction : Models -> Cmd Models
loadSchemaAction m =
    let smodel = m.system
        r  = m.readonly
        rw = m.editable
    in Cmd.map (\res -> (case res of
                            Ok newrec   -> {m|system   = {smodel| system_schema = Just newrec}}
                            Err httperr -> {m|editable = {rw| errmsg = r.httpErrorToString httperr }}
                        )
               ) m.readonly.api.system.info
        
execShowAction : Models -> PublicRootMsg
execShowAction m =
    let smodel = m.system
        r  = m.readonly
        rw = m.editable
    in Cmd.map (\res -> (case res of
                            Ok newrec   -> DirectMsg {m|system   = {smodel| show_record = Just newrec}}
                            Err httperr -> DirectMsg {m|editable = {rw| errmsg = r.httpErrorToString httperr }}
                        ) showView
               ) m.readonly.api.system.get |> HasCmd
        
indexView : Models -> Html PublicRootMsg
indexView m =
    div [] [
         H.span [] [text <| "index"]
        ,button [onClick <| SendRequest <| ToSystemReq ShowAction] [text "システム情報表示"]
        ,button [onClick <| SendRequest <| ToSystemReq EditAction] [text "システム情報編集"]
        ]

showView : Models -> Html PublicRootMsg
showView m =
    div [] [
         H.span [] [text <| if m.system.show_record == Nothing then "ロード失敗した" else "ロード済み"]
        ,button [onClick <| SendRequest <| ToSystemReq IndexAction] [text "システム情報インデックスへ"]
        ]

editView : Models -> Html PublicRootMsg
editView m =
    div [] [
         H.div [] [text <| "システム編集"]
        ,(case m.system.system_schema of
              Just rec -> editViewIfSystemSchemaLoaded rec
              Nothing  -> editViewIfSystemSchemaNotFound) m
        ,button [onClick <| SendRequest <| ToSystemReq IndexAction] [text "システム情報インデックスへ"] 
        ]

editViewIfSystemSchemaNotFound : Models -> Html PublicRootMsg
editViewIfSystemSchemaNotFound m =
    div [] [
         H.div [] [text <| "スキーマがロードされていません"]
        ]
editViewIfSystemSchemaLoaded : Dict String ColumnInfo -> Models -> Html PublicRootMsg
editViewIfSystemSchemaLoaded scm models =
    (case  models.system.edit_record of
         Just rec -> editViewImpl rec scm
         Nothing  -> editViewIfRecordNotFound) models

editViewIfRecordNotFound : Models -> Html PublicRootMsg
editViewIfRecordNotFound m =
    div [] [
         H.div [] [
              text <| "データがロードされていません"
             ,button [ onClick <| SendRequest <| ToSystemReq EditAction ] [text "リトライ"]
             ]
        ]

setValueResultToEditInputMsg : Models -> Result (String,ColumnTarget) System -> PublicRootMsg
setValueResultToEditInputMsg models res =
    let system_model = models.system
    in case res of
        Ok system -> case system_model.edit_record of
                         Just erec -> UpdateModel {models | system = {system_model | edit_record = Just <| Entity erec.id system}}
                         Nothing   -> DoNothing
        Err (inputv,tgt) -> DoNothing
        
editViewImpl : Entity System -> Dict String ColumnInfo -> Models -> Html PublicRootMsg
editViewImpl rec scm models =
    div [] [
         text <| "データはロード済み"
         ,formByColumns (Dict.map (\name info ->
                                       case stringToTarget name of
                                           Just tgt -> (True,info, (\inputv -> setValueResultToEditInputMsg models <| setValue rec.val tgt inputv))
                                           Nothing  -> (False,info, (\_      -> DoNothing))
                                  ) scm
                        |> Dict.filter (\_ (b,_,_) -> b)
                        |> Dict.map (\_ (b,x,y) -> (x,y))
                        ) (toStringMap rec.val) Nothing
        ,button [ onClick <| SendRequest <| ToSystemReq ModifyAction ] [text "保存"]
             -- UpdateModel Models
        ]
