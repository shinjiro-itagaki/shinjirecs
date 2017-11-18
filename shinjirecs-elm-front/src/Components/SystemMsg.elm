module Components.SystemMsg exposing (..)
import Records.System exposing (System,ColumnTarget(AreaId,Active,Setup,TunerCount,RestTunerCount))
import Records.Types exposing (Entity)
import Records.ColumnInfo exposing (ColumnInfo)
import Dict exposing (Dict)
import Http exposing (Error)

type ViewType = IndexView | ShowView | EditView
type SystemMsg = CountUp | None | LoadSchema | LoadSchemaResult (Result Http.Error (Dict String ColumnInfo)) | Load | AfterLoad (Result Http.Error System) | Edit (Entity System) | Put (Entity System) | SystemInput ColumnTarget String | ChangeView ViewType
