module Components exposing (Components,Models)
import Html exposing (Html,div,input,text,li,Attribute,program)
import Components.SystemC exposing (SystemC,SystemModel)
import Components.SystemC as SystemC exposing (new,Msg)
import Components.Types exposing (ComponentSym(SystemCSym))

type MsgToRoot = FromSystem SystemC.Msg

type alias Components = { system : SystemC }
type alias Models = { currentC : Maybe ComponentSym
                    , models : { system : SystemModel }
                    }

components : Components
components = { system = SystemC.new }

root = program { init = init
               , view = view
               , update = update
               , subscriptions = subscriptions
               }

init : (Models, Cmd MsgToRoot)
init = let x = components
           m = { currentC = Nothing
               , models = { system = x.system.init
                          }
               }
       in (m, Cmd.none)

update : MsgToRoot -> Models -> (Models, Cmd MsgToRoot)
update msg models = (models,Cmd.none)

subscriptions : Models -> Sub MsgToRoot
subscriptions m = Sub.none

-- div [] []
view : Models -> Html MsgToRoot
view model =
    case model.currentC of
        Just SystemCSym -> div [] [invoke model]
        Nothing  -> div [] [text <| "初期状態です"]

invoke : Models -> Html MsgToRoot
invoke m =
    case m.currentC of
        Just SystemCSym -> Html.map FromSystem <| components.system.view m.models.system
        Nothing -> div [] []

-- program : { init : (model, Cmd msg), update : msg -> model -> (model, Cmd msg), subscriptions : model -> Sub msg, view : model -> Html msg } -> Program Never model msg
