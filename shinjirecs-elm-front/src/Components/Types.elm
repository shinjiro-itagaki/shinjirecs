module Components.Types exposing (..)
import Html exposing (Html,div,input,text,li,Attribute)
import API exposing (API,getAPI)
import API.Types exposing (Cache)
import Components.SystemMsg exposing (SystemMsg)
import Http exposing (Error)
import Utils.Either exposing (Either(Left,Right))
import Components.SystemModel exposing (SystemModel)
import Components.SystemMsg as SystemMsg exposing (ActionType(IndexAction,ShowAction,EditAction,ModifyAction))

type alias CommonModelReadOnly = { config : Int, api : API, httpErrorToString : (Http.Error -> String), cache : Cache }
type alias CommonModelEditable = { counter : Int, errmsg : String }

type alias Models = { readonly : CommonModelReadOnly
                    , editable : CommonModelEditable
                    , system : SystemModel
                    }
    
type Request = NoSelect | ToSystemReq SystemMsg.ActionType
type RootMsg2 = DirectMsg Models (Models -> Html RootMsg2) | HasCmd (Cmd RootMsg2) | SendRequest Request | DoNothing | UpdateModel2 Models

redirectTo2 : Request -> RootMsg2
redirectTo2 = SendRequest
