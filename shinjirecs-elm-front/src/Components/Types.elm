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

type alias Models = { currentC : Maybe ComponentSym
                    , readonly : CommonModelReadOnly
                    , editable : CommonModelEditable
                    , system : SystemModel
                    }
    
type ComponentSym = SystemCSym
type RootMsg = SwitchTo ComponentSym
             | NoComponentSelected
             | ShowHttpError Http.Error
             | RefreshAPICache

type NextMsg msg = ToRoot RootMsg
                 | NextCmd (Cmd (msg,CommonModelEditable))
                 | Direct msg
                 | NoNext
                   
type alias Component model msg = { init          : model
                                 , update        : msg -> (model,CommonModelReadOnly,CommonModelEditable) -> Either (Cmd (model,CommonModelEditable)) (model,CommonModelEditable)
                                 , subscriptions : (model,CommonModelReadOnly,CommonModelEditable) -> Sub msg
                                 , view          : (model,CommonModelReadOnly,CommonModelEditable) -> Html msg }

    -- (Cmd (Cmd (Cmd ... RootMsg2)))
--type Ls a = Car a | Cdr (Ls a)
--type CmdWrapper a = CmdW (Cmd a) | HasNext (CmdWrapper a)
type Request = ToSystemReq SystemMsg.ActionType
type RootMsg2 = DirectMsg Models (Models -> Html RootMsg2) | HasCmd (Cmd RootMsg2) | SendRequest Request
