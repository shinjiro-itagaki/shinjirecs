module Components.SystemModel exposing (SystemModel)
import Dict exposing (Dict)
import Components.SystemMsg exposing (ActionType(IndexAction,ShowAction,EditAction,ModifyAction))
import Records.Types exposing (Entity)
import Records.System exposing (System)
import Records.ColumnInfo exposing (ColumnInfo)
type alias SystemModel = { show_record : Maybe (Entity System)
                         , edit_record : Maybe (Entity System)
                         , system_schema : Maybe (Dict String ColumnInfo)
                         , previousAction : ActionType
                         , actionType : ActionType
                         }
