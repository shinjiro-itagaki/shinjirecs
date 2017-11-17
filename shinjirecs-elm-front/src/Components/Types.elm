module Components.Types exposing (..)
import Html exposing (Html,div,input,text,li,Attribute)
import API exposing (API,getAPI)
import Components.SystemMsg exposing (SystemMsg)
import Http exposing (Error)
type alias CommonModelReadOnly = { config : Int, api : API }
type alias CommonModelEditable = { counter : Int, errmsg : Maybe String }

type ComponentSym = SystemCSym
type RootMsg = SwitchTo ComponentSym
             | NoComponentSelected
             | ShowHttpError Http.Error

type NextMsg msg = ToRoot RootMsg
                 | NextCmd (Cmd msg)
                 | Direct msg
                 | NoNext
                   
type alias Component model msg = { init          : model -- (model, Cmd msg)
                                 , update        : msg -> (model,CommonModelReadOnly,CommonModelEditable) -> ((model,CommonModelEditable), NextMsg msg)
                                 , subscriptions : (model,CommonModelReadOnly,CommonModelEditable) -> Sub msg
                                 , view          : (model,CommonModelReadOnly,CommonModelEditable) -> Html msg }
    


