module Components.SystemC exposing (SystemModel,SystemC,new)
import Components.Types exposing (Component,CommonModelReadOnly,CommonModelEditable,RootMsg(SwitchTo,NoComponentSelected,ShowHttpError),NextMsg(ToRoot,NextCmd,Direct,NoNext))
import Components.SystemMsg exposing (SystemMsg(CountUp,SystemInput,DoAction),ActionType(IndexAction,ShowAction,EditAction))
import Records.Types exposing (Entity)
import Records.ColumnInfo exposing (ColumnInfo)
import Records.System exposing (System,ColumnTarget(AreaId,Active,Setup,TunerCount,RestTunerCount),updateSystem,stringToTarget,toStringMap)
import Records.System as System exposing (new)
import Html exposing (Html,div,input,text,li,Attribute,button)
import Html.Events exposing (onClick)
import Components.Partials exposing (formByColumns)
import Dict exposing (Dict)
import Utils.Maybe exposing (catMaybes)
import Utils.Either exposing (Either(Left,Right))

type alias SystemModel = { show_record : Maybe (Entity System)
                         , edit_record : Maybe (Entity System)
                         , system_schema : Maybe (Dict String ColumnInfo)
                         , previousAction : ActionType
                         , actionType : ActionType
                         }
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


update : SystemMsg -> (SystemModel,CommonModelReadOnly,CommonModelEditable) -> Either (Cmd (SystemModel,CommonModelEditable)) (SystemModel,CommonModelEditable)
update msg (model,r,wr) =
    let always = Right (model,wr)
        sendErrMsg errmsg = Right (model, { wr | errmsg = Just errmsg })
    in case msg of
           CountUp -> Right (model,{ wr | counter = wr.counter + 1 })
           DoAction act ->
               let  model_ = { model | previousAction = model.actionType, actionType = act }
                    action = case act of
                                 IndexAction -> indexAction
                                 ShowAction  -> showAction
                                 EditAction  -> editAction
               in action (model_,r,wr)
           SystemInput target val ->
               let edit_rec = model.edit_record
               in case edit_rec of
                      Nothing -> always
                      Just erec -> case updateSystem erec.val target val of
                                       Ok newsystem -> Right ({ model | edit_record = Just { erec | val = newsystem }}, {wr | errmsg = Nothing})
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
                         (Err httperr) -> (m,{rw|errmsg = Just <| r.httpErrorToString httperr})
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
        (Nothing, Just rec)  -> Left <| reloadSchema m r {rw|errmsg = Just "tried to load schema"} 
        (Nothing, Nothing)   -> Left <| reloadSchema m r {rw|errmsg = Just "jfoefe"}
                                              
reloadSchema : SystemModel -> CommonModelReadOnly -> CommonModelEditable -> Cmd (SystemModel,CommonModelEditable)
reloadSchema m r rw =
    let f res = case res of
                    (Ok scm)      -> ({m|system_schema = Just scm},rw)
                    (Err httperr) -> (m,{rw|errmsg = Just <| r.httpErrorToString httperr})
    in Cmd.map f r.api.system.info
                      
subscriptions : (SystemModel,CommonModelReadOnly,CommonModelEditable) -> Sub SystemMsg
subscriptions (m,r,wr) = Sub.none

view : (SystemModel,CommonModelReadOnly,CommonModelEditable) -> Html SystemMsg
view (m,r,rw) =
    (case m.actionType of
         IndexAction -> indexView
         ShowAction  -> showView
         EditAction  -> case m.system_schema of
                            Just scm -> editView scm
                            Nothing  -> viewIfSchemaNotLoaded
    ) m r rw

linkToButton : ActionType -> String -> Html SystemMsg
linkToButton tgt label = button [ onClick <| DoAction tgt ] [text label]

linkToIndexButton = linkToButton IndexAction "システムメニュー"
linkToShowButton  = linkToButton ShowAction  "システム情報"
linkToEditButton  = linkToButton EditAction  "システム情報編集"

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

editView : Dict String ColumnInfo -> SystemModel -> CommonModelReadOnly -> CommonModelEditable -> Html SystemMsg
editView scm model r rw =
    div [] <| [
         case model.edit_record of
             Nothing   -> div [] [text <| "システム編集 ロードされていない"]
             Just erec -> formByColumns (cast scm) (toStringMap erec.val)
        , linkToIndexButton
        ]

cast : Dict String ColumnInfo -> Dict String (ColumnInfo,(String -> SystemMsg))
cast = Dict.fromList
       << catMaybes
       << List.map (\(colname,info) ->
                        Maybe.map (\target -> (colname, (info,SystemInput target)))
                        <| stringToTarget colname
                   )
       << Dict.toList
