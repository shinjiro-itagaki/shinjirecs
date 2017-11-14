module Components.Types exposing (Component,ComponentSym(..))
import Html exposing (Html,div,input,text,li,Attribute)

type alias Component model msg = { init          : model -- (model, Cmd msg)
                                 , update        : msg -> model -> (model, Cmd msg)
                                 , subscriptions : model -> Sub msg
                                 , view          : model -> Html msg }
type ComponentSym = SystemCSym
