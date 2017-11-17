module Components.SystemC exposing (SystemModel,SystemC,new,Msg) -- ,newSystemC)
import Components.Types exposing (Component,CommonModelReadOnly,CommonModelEditable)
import Components.SystemMsg exposing (SystemMsg(None,CountUp,LoadSchema,LoadSchemaResult,ShowAll,Show,ShowNew,PostNew,Edit,Put,Delete,SystemInput))
import Records.ColumnInfo exposing (ColumnInfo)
import Records.System exposing (System,ColumnTarget(AreaId,Active,Setup,TunerCount,RestTunerCount),updateSystem,stringToTarget)
import Records.System as System exposing (new)
import Html exposing (Html,div,input,text,li,Attribute,button)
import Html.Events exposing (onClick)
import Components.Partials exposing (formByColumns)
import Dict exposing (Dict)
import Utils.Maybe exposing (catMaybes)

type alias Msg = SystemMsg
type alias SystemModel = { system_record : System, system_schema : Maybe (Dict String ColumnInfo) }
type alias SystemC = Component SystemModel Msg

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
update : Msg -> (SystemModel,CommonModelReadOnly,CommonModelEditable) -> ((SystemModel,CommonModelEditable), Cmd Msg)
update msg (model,r,wr) =
    let always = ((model,wr),Cmd.none)
    in case msg of
           CountUp -> ((model,{ wr | counter = wr.counter + 1 }),Cmd.none)
           LoadSchema -> ((model,wr),Cmd.map LoadSchemaResult r.api.system.info)
           LoadSchemaResult (Ok res) -> always -- not impl (({model | system_schema = res}, wr), Cmd.none)
           LoadSchemaResult (Err httperr) -> always
           SystemInput target val ->
               case updateSystem model.system_record target val of -- : System -> ColumnTarget -> String -> Result (String,ColumnTarget) System
                   Ok newsystem -> (({ model | system_record = newsystem },wr),Cmd.none)
                   Err (colname,target2) -> always -- not impl ((model, { wr | errmsg = colname ++ " input error" }),Cmd.none)
           _ -> always

subscriptions : (SystemModel,CommonModelReadOnly,CommonModelEditable) -> Sub Msg
subscriptions (m,r,wr) = Sub.none

view : (SystemModel,CommonModelReadOnly,CommonModelEditable) -> Html Msg
view (model,r,wr) = div [] <| [
                     text <| "システム設定"
                    ,button [ onClick CountUp ] [text <| "カウントアップ"]
                    ,button [ onClick LoadSchema ] [text <| "スキーマのロード"]
                    ,div [] (case model.system_schema of
                                 Nothing -> []
                                 Just scm -> [formByColumns <| cast scm]
                            )
                    ]

-- Dict String ColumnInfo =>  =>  =>  => List (String, (ColumnInfo,func)) => Dict String (ColumnInfo,func)
cast : Dict String ColumnInfo -> Dict String (ColumnInfo,(String -> Msg))
cast = Dict.fromList
       << catMaybes
       << List.map (\(colname,info) ->
                        Maybe.map (\target -> (colname, (info,SystemInput target)))
                        <| stringToTarget colname
                   )
       << Dict.toList
