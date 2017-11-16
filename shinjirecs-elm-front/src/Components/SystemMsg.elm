module Components.SystemMsg exposing (..)
import Records.System exposing (System,ColumnTarget(AreaId,Active,Setup,TunerCount,RestTunerCount))
import Records.Types exposing (Entity)

type Input = SystemInput 
type SystemMsg = CountUp | None | ShowAll | Show Int | ShowNew | PostNew System | Edit (Entity System) | Put (Entity System) | Delete (Entity System)
