module Components.Types exposing (Component,ComponentSym(..),CommonCmd(..),CommonModelReadOnly,CommonModelEditable)
import Html exposing (Html,div,input,text,li,Attribute)

type alias CommonModelReadOnly = { config : Int }
type alias CommonModelEditable = { counter : Int }

type alias Component model msg = { init          : model -- (model, Cmd msg)
                                 , update        : msg -> (model,CommonModelReadOnly,CommonModelEditable) -> ((model,CommonModelEditable), Cmd msg)
                                 , subscriptions : (model,CommonModelReadOnly,CommonModelEditable) -> Sub msg
                                 , view          : (model,CommonModelReadOnly,CommonModelEditable) -> Html msg }
type ComponentSym = SystemCSym
type CommonCmd = SwitchTo ComponentSym | NoComponentSelected
