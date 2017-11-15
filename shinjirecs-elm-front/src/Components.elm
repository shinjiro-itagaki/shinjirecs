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

type MsgFromComponent = FromSystem SystemC.Msg
-- type MsgToRoot = FromSystem SystemC.Msg | Common CommonCmd
type MsgToRoot = FromComponent MsgFromComponent | Common CommonCmd

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

--mergeModel : ComponentSym -> Models
--mergeModel (x,r,wr) ()

updateComponent : Models
                -> msg
                -> (msg -> (model,CommonModelReadOnly,CommonModelEditable) -> ((model,CommonModelEditable), Cmd msg))
                -> (msg -> MsgFromComponent)
                -> model
                -> (Models -> model -> Models)
                -> (Models, Cmd MsgToRoot)
updateComponent models msg_ f_update f_castToMsgFromComponent model f_model_updater =
    f_update msg_ (model, models.readonly, models.editable) |> \((m,wr),cmd) -> ( f_model_updater {models | editable = wr} m, Cmd.map (FromComponent << f_castToMsgFromComponent) cmd )
    
update : MsgToRoot -> Models -> (Models, Cmd MsgToRoot)
update msg models =
    case msg of
        FromComponent msgfrom ->
            let updateComponent__ = updateComponent models
            in case msgfrom of
                   FromSystem system_msg -> updateComponent__ system_msg components.system.update FromSystem models.system (\ms_ m_ -> {ms_ | system = m_})
                                             
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
        SystemCSym -> Html.map (FromComponent << FromSystem) <| components.system.view (m.system,m.readonly,m.editable)

-- program : { init : (model, Cmd msg), update : msg -> model -> (model, Cmd msg), subscriptions : model -> Sub msg, view : model -> Html msg } -> Program Never model msg
