module Components exposing (root)
import Html exposing (Html,div,input,text,li,Attribute,program)
import Components.SystemC exposing (SystemC,SystemModel)
import Components.SystemC as SystemC exposing (new,Msg)
import Components.Types exposing (ComponentSym(SystemCSym))
import MainCssInterface as Css exposing (CssClasses(NavBar),CssIds(Page),mainCssLink)
import Html.CssHelpers exposing (withNamespace)

{ id, class, classList } = withNamespace "root"

type MsgToRoot = FromSystem SystemC.Msg

type alias Components = { system : SystemC }
type alias Models = { currentC : Maybe ComponentSym
                    , system : SystemModel
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
               , system = x.system.init
               }
       in (m, Cmd.none)

update : MsgToRoot -> Models -> (Models, Cmd MsgToRoot)
update msg models =
    case msg of
        FromSystem msg -> components.system.update msg models.system |> \(m,cmd) -> ( { models | system = m } , Cmd.map FromSystem cmd )

subscriptions : Models -> Sub MsgToRoot
subscriptions m = Sub.none

view : Models -> Html MsgToRoot
view models = div [ class [NavBar] ] <|
              case models.currentC of
                  Just sym -> [invoke sym models]
                  Nothing  -> [text <| "初期状態です"]

invoke : ComponentSym -> Models -> Html MsgToRoot
invoke sym m =
    case sym of
        SystemCSym -> Html.map FromSystem <| components.system.view m.system

-- program : { init : (model, Cmd msg), update : msg -> model -> (model, Cmd msg), subscriptions : model -> Sub msg, view : model -> Html msg } -> Program Never model msg
