module Components.Types exposing (..)
import Html exposing (Html,div,input,text,li,Attribute)
import API exposing (API,getAPI)
import API.Types exposing (Cache)
import Components.SystemMsg exposing (SystemMsg)
import Http exposing (Error)
import Utils.Either exposing (Either(Left,Right))
import Components.SystemModel exposing (SystemModel)
import Components.SystemMsg as SystemMsg exposing (ActionType(IndexAction,ShowAction,EditAction,ModifyAction))
import Components.EpgProgramsMsg as EpgProgramsMsg exposing (ActionType(IndexAction,MakeReservationAction))
import Components.EpgProgramsModel as EpgProgramsModel exposing (EpgProgramsModel)

type alias CommonModelReadOnly = { config : Int, api : API, httpErrorToString : (Http.Error -> String), cache : Cache }
type alias CommonModelEditable = { counter : Int, errmsg : String }

type alias Models = { readonly : CommonModelReadOnly
                    , editable : CommonModelEditable
                    , system : SystemModel
                    , epgPrograms : EpgProgramsModel
                    }

type Request = NoSelect | ToSystemReq SystemMsg.ActionType | ToEpgProgramsReq EpgProgramsMsg.ActionType
type PublicRootMsg = DirectMsg Models (Models -> Html PublicRootMsg) | HasCmd (Cmd PublicRootMsg) | SendRequest Request | DoNothing | UpdateModel Models | UpdateAPICache

redirectTo : Request -> PublicRootMsg
redirectTo = SendRequest

type alias Component m act = { init : m
                             , accept : (act -> Models -> PublicRootMsg)
                             , subscriptions : (Models -> Sub PublicRootMsg)}

countUp : Models -> Models
countUp = countPlus 1
          
countDown : Models -> Models
countDown = countPlus -1

countPlus : Int -> Models -> Models
countPlus i m = let e = m.editable in {m|editable = {e|counter = e.counter + i}}
