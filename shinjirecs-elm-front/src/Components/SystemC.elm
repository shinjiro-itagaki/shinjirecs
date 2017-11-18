module Components.SystemC exposing (SystemModel,SystemC,new) -- ,newSystemC)
import Components.Types exposing (Component,CommonModelReadOnly,CommonModelEditable,RootMsg(SwitchTo,NoComponentSelected,ShowHttpError),NextMsg(ToRoot,NextCmd,Direct,NoNext))
import Components.SystemMsg exposing (SystemMsg(CountUp,SystemInput,DoAction),ActionType(IndexAction,ShowAction,EditAction))
import Records.Types exposing (Entity)
import Records.ColumnInfo exposing (ColumnInfo)
import Records.System exposing (System,ColumnTarget(AreaId,Active,Setup,TunerCount,RestTunerCount),updateSystem,stringToTarget)
import Records.System as System exposing (new)
import Html exposing (Html,div,input,text,li,Attribute,button)
import Html.Events exposing (onClick)
import Components.Partials exposing (formByColumns)
import Dict exposing (Dict)
import Utils.Maybe exposing (catMaybes)
import Utils.Either exposing (Either(Left,Right))

type alias SystemModel = { system_record : System
                         , show_record : Maybe (Entity System)
                         , edit_record : Either (Maybe Int) (Entity System)
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
init = { system_record = System.new
       , show_record = Nothing
       , edit_record = Left Nothing
       , system_schema = Nothing
       , previousAction = IndexAction
       , actionType = IndexAction
       }


-- { index   : Maybe Int -> Cmd (Result Http.Error (List (Entity a))) -- Int => limit
-- , get     : Int       -> Cmd (Result Http.Error a) 
-- , create  : a         -> Cmd (Result Http.Error (Entity a)) 
-- , modify  : Entity a  -> Cmd (Result Http.Error a)
-- , destroy : Entity a  -> Cmd (Result Http.Error Bool)
-- , info    : Cmd (Result Http.Error (Dict String ColumnInfo))
update : SystemMsg -> (SystemModel,CommonModelReadOnly,CommonModelEditable) -> Either (Cmd (SystemModel,CommonModelEditable)) (SystemModel,CommonModelEditable)  -- ((SystemModel,CommonModelEditable), NextMsg SystemMsg)
update msg (model,r,wr) =
    let -- always = ((model,wr),NoNext)
        always = Right (model,wr)
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
               case updateSystem model.system_record target val of
                   Ok newsystem -> Right ({ model | system_record = newsystem }, {wr | errmsg = Nothing})
                   Err (colname,target2) -> sendErrMsg <| colname ++ " input error"
--           _ -> sendErrMsg "another action !!"

    -- { index   : Maybe Int -> Cmd (Result Http.Error (List (Entity a))) -- Int => limit
    -- , get     : Int       -> Cmd (Result Http.Error a) 
    -- , create  : a         -> Cmd (Result Http.Error (Entity a)) 
    -- , modify  : Entity a  -> Cmd (Result Http.Error a)
    -- , destroy : Entity a  -> Cmd (Result Http.Error Bool)
    -- , info    : Cmd (Result Http.Error (Dict String ColumnInfo))
    -- }
indexAction : (SystemModel,CommonModelReadOnly,CommonModelEditable) -> Either (Cmd (SystemModel,CommonModelEditable))  (SystemModel,CommonModelEditable)
indexAction (m, r, rw) = Right (m,rw)

showAction : (SystemModel,CommonModelReadOnly,CommonModelEditable) -> Either (Cmd (SystemModel,CommonModelEditable))  (SystemModel,CommonModelEditable)
showAction (m, r, rw) =
    case m.show_record of
        Just x -> Right (m,rw)
        Nothing -> Left <| reloadSystem m r rw

reloadSystem : SystemModel -> CommonModelReadOnly -> CommonModelEditable -> Cmd (SystemModel,CommonModelEditable)
reloadSystem m r rw =
    let f res = case res of
                    (Ok e)      -> ({m|show_record = Just e },rw)
                    (Err httperr) -> (m,{rw|errmsg = Just <| r.httpErrorToString httperr})
    in Cmd.map f r.api.system.get
    
editAction : (SystemModel,CommonModelReadOnly,CommonModelEditable) -> Either (Cmd (SystemModel,CommonModelEditable))  (SystemModel,CommonModelEditable)
editAction (m, r, rw) =
    case m.system_schema of
        Just x  -> Right (m,rw)
        Nothing -> Left <| reloadSchema m r rw

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
         EditAction  -> editView
    ) m r rw

linkToButton : ActionType -> String -> Html SystemMsg
linkToButton tgt label = button [ onClick <| DoAction tgt ] [text label]

linkToIndexButton = linkToButton IndexAction "システムメニュー"
linkToShowButton  = linkToButton ShowAction  "システム情報"
linkToEditButton  = linkToButton EditAction  "システム情報編集"

                   
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

editView : SystemModel -> CommonModelReadOnly -> CommonModelEditable -> Html SystemMsg
editView model r rw =
    div [] <| [
         case model.edit_record of
             Left Nothing          -> div [] [text <| "システム編集 id指定されていない"]
             Left (Just id)        -> div [] [text <| "システム編集 id指定されているがロードされていない"]
             Right e -> div [] [text <| "システム編集 ロードすみ"]
        ,linkToIndexButton
        ]

-- Dict String ColumnInfo =>  =>  =>  => List (String, (ColumnInfo,func)) => Dict String (ColumnInfo,func)
cast : Dict String ColumnInfo -> Dict String (ColumnInfo,(String -> SystemMsg))
cast = Dict.fromList
       << catMaybes
       << List.map (\(colname,info) ->
                        Maybe.map (\target -> (colname, (info,SystemInput target)))
                        <| stringToTarget colname
                   )
       << Dict.toList
