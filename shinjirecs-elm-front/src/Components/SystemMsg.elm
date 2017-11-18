module Components.SystemMsg exposing (..)
import Records.System exposing (System,ColumnTarget(AreaId,Active,Setup,TunerCount,RestTunerCount))
import Records.Types exposing (Entity)
import Records.ColumnInfo exposing (ColumnInfo)
import Dict exposing (Dict)
import Http exposing (Error)

type ActionType = IndexAction | ShowAction | EditAction
type SystemMsg = CountUp | SystemInput ColumnTarget String | DoAction ActionType
