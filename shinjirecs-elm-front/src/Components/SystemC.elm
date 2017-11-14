module Components.SystemC exposing (SystemModel,SystemC,new,Msg) -- ,newSystemC)
import Components.Types exposing (Component)
import Models.System exposing (System)
import Models.System as System exposing (new)
import Html exposing (Html,div,input,text,li,Attribute)

type Msg = None
type alias SystemModel = { record : System }
type alias SystemC = Component SystemModel Msg

new : SystemC
new = { init          = init
      , update        = update
      , subscriptions = subscriptions
      , view          = view
      }

init : SystemModel
init = {record = System.new }

update : Msg -> SystemModel -> (SystemModel, Cmd Msg)
update msg model = (model,Cmd.none)

subscriptions : SystemModel -> Sub Msg
subscriptions m = Sub.none

view : SystemModel -> Html Msg
view model = div [] [text <| "こんにちは"]
    
