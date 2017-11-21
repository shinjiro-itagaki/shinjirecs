module Components.SystemC exposing (SystemC,new,accept)
import Components.SystemModel exposing (SystemModel)
import Components.Types exposing (Component,Models,CommonModelReadOnly,CommonModelEditable,RootMsg(SwitchTo,NoComponentSelected,ShowHttpError),NextMsg(ToRoot,NextCmd,Direct,NoNext),RootMsg2(DirectMsg,HasCmd))
import Components.SystemMsg exposing (SystemMsg(CountUp,SystemInput,DoAction),ActionType(IndexAction,ShowAction,EditAction,ModifyAction))
import Records.Types exposing (Entity)
import Records.ColumnInfo exposing (ColumnInfo)
import Records.System exposing (System,ColumnTarget(AreaId,Active,Setup,TunerCount{- ,RestTunerCount -}),setValue,stringToTarget,toStringMap)
import Records.System as System exposing (new)
import Html exposing (Html,div,input,text,li,Attribute,button)
import Html.Events exposing (onClick)
import Components.Partials exposing (formByColumns,sample,SampleMsg,SampleModel)
import Dict exposing (Dict)
import Utils.Maybe exposing (catMaybes)
import Utils.Either exposing (Either(Left,Right))

type alias SystemC = Component SystemModel SystemMsg

new : SystemC
new = { init          = init
      , update        = update
      , subscriptions = subscriptions
      , view          = view
      }
init : SystemModel
init = { show_record = Nothing
       , edit_record = Nothing
       , system_schema = Nothing
       , previousAction = IndexAction
       , actionType = IndexAction
       }

redirectTo : ActionType -> SystemModel -> SystemModel
redirectTo act m = { m | previousAction = m.actionType, actionType = act }
    
update : SystemMsg -> (SystemModel,CommonModelReadOnly,CommonModelEditable) -> Either (Cmd (SystemModel,CommonModelEditable)) (SystemModel,CommonModelEditable)
update msg (model,r,wr) =
    let always = Right (model,wr)
        sendErrMsg errmsg = Right (model, { wr | errmsg = errmsg })
    in case msg of
           CountUp -> Right (model,{ wr | counter = wr.counter + 1 })
           DoAction act ->
               let  model_ = { model | previousAction = model.actionType, actionType = act }
                    action = case act of
                                 IndexAction -> indexAction
                                 ShowAction  -> showAction
                                 EditAction  -> editAction
                                 ModifyAction -> modifyAction
               in action (model_,r,wr)
           SystemInput target val ->
               let edit_rec = model.edit_record
               in case edit_rec of
                      Nothing -> always
                      Just erec -> case setValue erec.val target val of
                                       Ok newsystem -> Right ({ model | edit_record = Just { erec | val = newsystem }}, {wr | errmsg = val })
                                       Err (colname,target2) -> sendErrMsg <| colname ++ " input error"
                                                                
indexAction : (SystemModel,CommonModelReadOnly,CommonModelEditable) -> Either (Cmd (SystemModel,CommonModelEditable))  (SystemModel,CommonModelEditable)
indexAction (m, r, rw) = Right ({m | system_schema = Nothing, show_record = Nothing, edit_record = Nothing},rw)

showAction : (SystemModel,CommonModelReadOnly,CommonModelEditable) -> Either (Cmd (SystemModel,CommonModelEditable))  (SystemModel,CommonModelEditable)
showAction (m, r, rw) =
    case m.show_record of
        Just x -> Right (m,rw)
        Nothing -> Left <| reloadShowRecord m r rw


rewriteShowRecord : SystemModel -> Entity System -> SystemModel
rewriteShowRecord m rec = {m|show_record = Just rec}

rewriteEditRecord : SystemModel -> Entity System -> SystemModel
rewriteEditRecord m rec = {m|edit_record = Just rec}

reloadRecord : SystemModel -> CommonModelReadOnly -> CommonModelEditable -> (SystemModel -> Entity System -> SystemModel) -> Cmd (SystemModel,CommonModelEditable)
reloadRecord m r rw f =
    let caster res = case res of
                         (Ok e)        -> (f m e ,rw)
                         (Err httperr) -> (m,{rw|errmsg = r.httpErrorToString httperr})
    in Cmd.map caster r.api.system.get
                          
reloadShowRecord : SystemModel -> CommonModelReadOnly -> CommonModelEditable -> Cmd (SystemModel,CommonModelEditable)
reloadShowRecord m r rw = reloadRecord m r rw (\m2 e -> {m2|show_record = Just e} )

reloadEditRecord : SystemModel -> CommonModelReadOnly -> CommonModelEditable -> Cmd (SystemModel,CommonModelEditable)
reloadEditRecord m r rw = reloadRecord m r rw (\m2 e -> {m2|edit_record = Just e} )
                      
editAction : (SystemModel,CommonModelReadOnly,CommonModelEditable) -> Either (Cmd (SystemModel,CommonModelEditable)) (SystemModel,CommonModelEditable)
editAction (m, r, rw) =
    case (m.system_schema, m.edit_record) of
        (Just scm, Just rec) -> Right (m,rw)
        (Just scm, Nothing)  -> Left <| reloadEditRecord m r rw
        (Nothing, Just rec)  -> Left <| reloadSchema m r {rw|errmsg = "tried to load schema"} 
        (Nothing, Nothing)   -> Left <| reloadSchema m r {rw|errmsg = "jfoefe"}

modifyAction : (SystemModel,CommonModelReadOnly,CommonModelEditable) -> Either (Cmd (SystemModel,CommonModelEditable)) (SystemModel,CommonModelEditable)
modifyAction (m, r, rw) =
    case m.edit_record of
        Just erec -> Left <| modifyActionImpl erec (m,r,rw)
        Nothing   -> Right (m, {rw|errmsg="edit record is not found"})

                                
modifyActionImpl : Entity System -> (SystemModel,CommonModelReadOnly,CommonModelEditable) -> Cmd (SystemModel,CommonModelEditable)
modifyActionImpl erec (m,r,rw) =
    Cmd.map (\res -> case res of
                         Ok newsys   -> let e = Just <| Entity erec.id newsys in ({m| show_record = e, edit_record = e} |> redirectTo ShowAction, rw)
                         Err httperr -> (redirectTo EditAction m, {rw| errmsg=r.httpErrorToString httperr })
            ) <| r.api.system.modify erec
        
reloadSchema : SystemModel -> CommonModelReadOnly -> CommonModelEditable -> Cmd (SystemModel,CommonModelEditable)
reloadSchema m r rw =
    let f res = case res of
                    (Ok scm)      -> ({m|system_schema = Just scm},rw)
                    (Err httperr) -> (m,{rw|errmsg = r.httpErrorToString httperr})
    in Cmd.map f r.api.system.info
                      
subscriptions : (SystemModel,CommonModelReadOnly,CommonModelEditable) -> Sub SystemMsg
subscriptions (m,r,wr) = Sub.none

viewMain : (SystemModel,CommonModelReadOnly,CommonModelEditable) -> Html SystemMsg
viewMain (m,r,rw) =
    (case m.actionType of
         IndexAction -> indexView
         ShowAction  -> showView
         EditAction  -> editView m.system_schema
         ModifyAction -> editView m.system_schema
    ) m r rw

view : (SystemModel,CommonModelReadOnly,CommonModelEditable) -> Html SystemMsg
view (m,r,rw) =
    div [] [
         viewMain (m,r,rw)
        ]
    
    
linkToButton : ActionType -> String -> Html SystemMsg
linkToButton tgt label = button [ onClick <| DoAction tgt ] [text label]

linkToIndexButton = linkToButton IndexAction "システムメニュー"
linkToShowButton  = linkToButton ShowAction  "システム情報"
linkToEditButton  = linkToButton EditAction  "システム情報編集"

submitEditButton = linkToButton ModifyAction "システム情報更新"
                    
viewIfSchemaNotLoaded : SystemModel -> CommonModelReadOnly -> CommonModelEditable -> Html SystemMsg
viewIfSchemaNotLoaded model r rw = div [] [text <| "スキーマがロードされていない"]
    
indexView : SystemModel -> CommonModelReadOnly -> CommonModelEditable -> Html SystemMsg
indexView model r rw =
    div [] <| [
         text <| "システム設定"
        ,button [ onClick CountUp ] [text <| "カウントアップ"]
        ,linkToShowButton
        ,linkToEditButton
        ]

showView : SystemModel -> CommonModelReadOnly -> CommonModelEditable -> Html SystemMsg
showView model r rw =
    div [] <| [
         case model.show_record of
             Nothing -> div [] [text <| "システム表示 ロードされていない"]
             Just e  -> div [] [text <| "システム表示 ロードすみ"]
        ,linkToIndexButton
        ]

editView : Maybe (Dict String ColumnInfo) -> SystemModel -> CommonModelReadOnly -> CommonModelEditable -> Html SystemMsg
editView mscm = case mscm of
                   Just scm -> editViewImpl scm
                   Nothing  -> viewIfSchemaNotLoaded          

editViewImpl : Dict String ColumnInfo -> SystemModel -> CommonModelReadOnly -> CommonModelEditable -> Html SystemMsg
editViewImpl scm model r rw =
    div [] <| [
         case model.edit_record of
             Nothing   -> div [] [text <| "システム編集 ロードされていない"]
             Just erec -> formByColumns (cast scm) (toStringMap erec.val) Nothing
        , submitEditButton
        , linkToIndexButton
        ]

modifyView : SystemModel -> CommonModelReadOnly -> CommonModelEditable -> Html SystemMsg
modifyView model r rw =
    div [] <| [
         div [] [text <| "システム更新中"]
        ]
             
cast : Dict String ColumnInfo -> Dict String (ColumnInfo,(String -> SystemMsg))
cast = Dict.fromList
       << catMaybes
       << List.map (\(colname,info) ->
                        Maybe.map (\target -> (colname, (info,SystemInput target)))
                        <| stringToTarget colname
                   )
       << Dict.toList

castToRootMsg : Models -> SystemModel -> Models
castToRootMsg m sysm = {m|system = sysm}
           
accept : ActionType -> Models -> RootMsg2
accept tipe m =
    let html =
            case tipe of
                IndexAction   -> DirectMsg m (\x -> div [] [text <| ""])
                ShowAction    -> DirectMsg m (\x -> div [] [text <| ""])
                EditAction    -> DirectMsg m (\x ->  div [] [text <| ""])
                ModifyAction  -> DirectMsg m (\x ->  div [] [text <| ""])
    in html
