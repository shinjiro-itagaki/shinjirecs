module Components exposing (root)
import Html exposing (Html,div,span,input,text,li,Attribute,program)
import Components.SystemC exposing (SystemC,SystemModel)
import Components.SystemC as SystemC exposing (new,Msg)
import Components.Types exposing (ComponentSym(SystemCSym),CommonCmd(SwitchTo),CommonModelReadOnly,CommonModelEditable)
import MainCssInterface as Css exposing (CssClasses(NavBar),CssIds(Page),mainCssLink)
import Html.CssHelpers exposing (withNamespace)
import Html.Events exposing (on,keyCode,onInput,onClick)
import List exposing (singleton)

{ id, class, classList } = withNamespace "root"

type MsgToRoot = FromSystem SystemC.Msg | Common CommonCmd

type alias Components = { system : SystemC }
type alias Models = { currentC : Maybe ComponentSym
                    , readonly : CommonModelReadOnly
                    , editable : CommonModelEditable
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
               , readonly = { config = 1 }
               , editable = { counter = 0 }
               }
       in (m, Cmd.none)

update : MsgToRoot -> Models -> (Models, Cmd MsgToRoot)
update msg models =
    case msg of
        FromSystem msg -> components.system.update msg (models.system, models.readonly, models.editable) |> \((m,wr),cmd) -> ( { models | system = m, editable = wr } , Cmd.map FromSystem cmd )
        Common (SwitchTo sym) -> ({ models | currentC = Just sym }, Cmd.none)

subscriptions : Models -> Sub MsgToRoot
subscriptions m = Sub.none

switchTo : ComponentSym -> MsgToRoot
switchTo sym = Common (SwitchTo sym)
                  
view : Models -> Html MsgToRoot
view models = div [ class [NavBar] ] <| singleton <|
              case models.currentC of
                  Just sym -> invoke sym models
                  Nothing  -> div [] [
                               (span [onClick (switchTo SystemCSym)] [text "システム設定へ"])
                              ]

invoke : ComponentSym -> Models -> Html MsgToRoot
invoke sym m =
    case sym of
        SystemCSym -> Html.map FromSystem <| components.system.view (m.system,m.readonly,m.editable)

-- program : { init : (model, Cmd msg), update : msg -> model -> (model, Cmd msg), subscriptions : model -> Sub msg, view : model -> Html msg } -> Program Never model msg
