module Components.SystemMsg exposing (..)
import Records.System exposing (System,ColumnTarget(AreaId,Active,Setup,TunerCount,RestTunerCount))
import Records.Types exposing (Entity)
import Records.ColumnInfo exposing (ColumnInfo)
import Dict exposing (Dict)
import Http exposing (Error)

type SystemMsg = CountUp | None | LoadSchema | LoadSchemaResult (Result Http.Error (Dict String ColumnInfo)) | ShowAll | Show Int | ShowNew | PostNew System | Edit (Entity System) | Put (Entity System) | Delete (Entity System) | SystemInput ColumnTarget String
