module Components.Types exposing (..)
import Html exposing (Html,div,input,text,li,Attribute)
import API exposing (API,getAPI)
import Components.SystemMsg exposing (SystemMsg)

type alias CommonModelReadOnly = { config : Int, api : API }
type alias CommonModelEditable = { counter : Int }

type alias Component model msg = { init          : model -- (model, Cmd msg)
                                 , update        : msg -> (model,CommonModelReadOnly,CommonModelEditable) -> ((model,CommonModelEditable), Cmd msg)
                                 , subscriptions : (model,CommonModelReadOnly,CommonModelEditable) -> Sub msg
                                 , view          : (model,CommonModelReadOnly,CommonModelEditable) -> Html msg }
type ComponentSym = SystemCSym
type MsgToRoot = SwitchTo ComponentSym
               | NoComponentSelected
               | FromSystem SystemMsg
