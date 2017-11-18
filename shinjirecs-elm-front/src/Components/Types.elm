module Components.Types exposing (..)
import Html exposing (Html,div,input,text,li,Attribute)
import API exposing (API,getAPI)
import Components.SystemMsg exposing (SystemMsg)
import Http exposing (Error)
import Utils.Either exposing (Either(Left,Right))

type alias CommonModelReadOnly = { config : Int, api : API, httpErrorToString : (Http.Error -> String) }
type alias CommonModelEditable = { counter : Int, errmsg : Maybe String }

type ComponentSym = SystemCSym
type RootMsg = SwitchTo ComponentSym
             | NoComponentSelected
             | ShowHttpError Http.Error

type NextMsg msg = ToRoot RootMsg
                 | NextCmd (Cmd (msg,CommonModelEditable))
                 | Direct msg
                 | NoNext
                   
type alias Component model msg = { init          : model
                                 , update        : msg -> (model,CommonModelReadOnly,CommonModelEditable) -> Either (Cmd (model,CommonModelEditable)) (model,CommonModelEditable)
                                 , subscriptions : (model,CommonModelReadOnly,CommonModelEditable) -> Sub msg
                                 , view          : (model,CommonModelReadOnly,CommonModelEditable) -> Html msg }
    


