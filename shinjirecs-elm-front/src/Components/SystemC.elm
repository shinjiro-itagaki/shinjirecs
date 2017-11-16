module Components.SystemC exposing (SystemModel,SystemC,new,Msg) -- ,newSystemC)
import Components.Types exposing (Component,CommonModelReadOnly,CommonModelEditable)
import Components.SystemMsg exposing (SystemMsg(None,CountUp))
import Models.System exposing (System)
import Models.System as System exposing (new)
import Html exposing (Html,div,input,text,li,Attribute,button)
import Html.Events exposing (onClick)
import Components.Partials exposing (formByColumns)

type alias Msg = SystemMsg
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

update : Msg -> (SystemModel,CommonModelReadOnly,CommonModelEditable) -> ((SystemModel,CommonModelEditable), Cmd Msg)
update msg (model,r,wr) =
    case msg of
        CountUp -> ((model,{ wr | counter = wr.counter + 1 }),Cmd.none)
        None -> ((model,wr),Cmd.none)

subscriptions : (SystemModel,CommonModelReadOnly,CommonModelEditable) -> Sub Msg
subscriptions (m,r,wr) = Sub.none

view : (SystemModel,CommonModelReadOnly,CommonModelEditable) -> Html Msg
view (model,r,wr) = div [] [
                     text <| "システム設定"
                    ,button [ onClick CountUp ] [text <| "カウントアップ"]
                    ]
    
