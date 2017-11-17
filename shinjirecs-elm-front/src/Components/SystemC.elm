module Components.SystemC exposing (SystemModel,SystemC,new) -- ,newSystemC)
import Components.Types exposing (Component,CommonModelReadOnly,CommonModelEditable,RootMsg(SwitchTo,NoComponentSelected,ShowHttpError),NextMsg(ToRoot,NextCmd,Direct,NoNext))
import Components.SystemMsg exposing (SystemMsg(None,CountUp,LoadSchema,LoadSchemaResult,ShowAll,Show,ShowNew,PostNew,Edit,Put,Delete,SystemInput))
import Records.ColumnInfo exposing (ColumnInfo)
import Records.System exposing (System,ColumnTarget(AreaId,Active,Setup,TunerCount,RestTunerCount),updateSystem,stringToTarget)
import Records.System as System exposing (new)
import Html exposing (Html,div,input,text,li,Attribute,button)
import Html.Events exposing (onClick)
import Components.Partials exposing (formByColumns)
import Dict exposing (Dict)
import Utils.Maybe exposing (catMaybes)

type alias SystemModel = { system_record : System, system_schema : Maybe (Dict String ColumnInfo) }
type alias SystemC = Component SystemModel SystemMsg

new : SystemC
new = { init          = init
      , update        = update
      , subscriptions = subscriptions
      , view          = view
      }
init : SystemModel
init = {system_record = System.new, system_schema = Nothing}


-- { index   : Maybe Int -> Cmd (Result Http.Error (List (Entity a))) -- Int => limit
-- , get     : Int       -> Cmd (Result Http.Error a) 
-- , create  : a         -> Cmd (Result Http.Error (Entity a)) 
-- , modify  : Entity a  -> Cmd (Result Http.Error a)
-- , destroy : Entity a  -> Cmd (Result Http.Error Bool)
-- , info    : Cmd (Result Http.Error (Dict String ColumnInfo))
update : SystemMsg -> (SystemModel,CommonModelReadOnly,CommonModelEditable) -> ((SystemModel,CommonModelEditable), NextMsg SystemMsg)
update msg (model,r,wr) =
    let always = ((model,wr),NoNext)
        notimpl = ((model,wr),NoNext)
        sendErrMsg errmsg = ((model, { wr | errmsg = Just errmsg }), NoNext)
    in case msg of
           CountUp -> ((model,{ wr | counter = wr.counter + 1 }),NoNext)
           None -> always
           LoadSchema -> ((model,wr), NextCmd <| Cmd.map LoadSchemaResult r.api.system.info)
           LoadSchemaResult (Ok res) -> (({model | system_schema = Just res}, wr), NoNext)
           LoadSchemaResult (Err httperr) -> ((model,wr),ToRoot <| ShowHttpError httperr)
           ShowAll -> notimpl
           Show id -> notimpl
           ShowNew -> notimpl
           PostNew params -> notimpl
           Edit entity -> notimpl
           Put entity -> notimpl
           Delete entity -> notimpl
           SystemInput target val ->
               case updateSystem model.system_record target val of
                   Ok newsystem -> (({ model | system_record = newsystem },wr),NoNext)
                   Err (colname,target2) -> sendErrMsg <| colname ++ " input error"

subscriptions : (SystemModel,CommonModelReadOnly,CommonModelEditable) -> Sub SystemMsg
subscriptions (m,r,wr) = Sub.none

view : (SystemModel,CommonModelReadOnly,CommonModelEditable) -> Html SystemMsg
view (model,r,wr) = div [] <| [
                     text <| "システム設定"
                    ,button [ onClick CountUp ] [text <| "カウントアップ"]
                    ,button [ onClick LoadSchema ] [text <| "スキーマのロード"]
                    ,div [] (case model.system_schema of
                                 Nothing -> []
                                 Just scm -> [formByColumns <| cast scm])
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
