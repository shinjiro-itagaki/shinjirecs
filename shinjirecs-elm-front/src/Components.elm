module Components exposing (root)
import Html exposing (Html,program)
import Html as H
import Components.SystemC exposing (SystemC,SystemModel)
import Components.Types exposing (MsgToRoot(SwitchTo,NoComponentSelected,FromSystem,ShowHttpError),ComponentSym(SystemCSym),CommonModelReadOnly,CommonModelEditable)
import MainCssInterface as Css exposing (CssClasses(NavBar),CssIds(Page),mainCssLink)
import Html.CssHelpers exposing (withNamespace)
import Html.Events as E
import List exposing (singleton)
import API exposing (getAPI)
import Components.SystemC as SystemC

{ id, class, classList } = withNamespace "root"

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
               , readonly = { config = 1, api = getAPI "http://127.0.0.1:3000" }
               , editable = { counter = 0, errmsg = Nothing }
               }
       in (m, Cmd.none)

--mergeModel : ComponentSym -> Models
--mergeModel (x,r,wr) ()

updateComponent : Models
                -> msg
                -> (msg -> (model,CommonModelReadOnly,CommonModelEditable) -> ((model,CommonModelEditable), Cmd msg))
                -> (msg -> MsgToRoot)
                -> model
                -> (Models -> model -> Models)
                -> (Models, Cmd MsgToRoot)
updateComponent models msg_ f_update f_castToMsgToRoot model f_model_updater =
    f_update msg_ (model, models.readonly, models.editable) |> \((m,wr),cmd) -> ( f_model_updater {models | editable = wr} m, Cmd.map f_castToMsgToRoot cmd )
    
update : MsgToRoot -> Models -> (Models, Cmd MsgToRoot)
update msg models =
    let updateComponent__ = updateComponent models
        always = (models,Cmd.none)
    in case msg of
           FromSystem system_msg ->
               updateComponent__ system_msg components.system.update FromSystem models.system (\ms_ m_ -> {ms_ | system = m_})
           SwitchTo sym -> ({ models | currentC = Just sym }, Cmd.none)
           NoComponentSelected -> ({ models | currentC = Nothing }, Cmd.none)
           ShowHttpError httperr ->  always

subscriptions : Models -> Sub MsgToRoot
subscriptions m = Sub.none

switchTo : ComponentSym -> MsgToRoot
switchTo sym = SwitchTo sym

componentView : Models -> Html MsgToRoot
componentView models =
    case models.currentC of
        Just sym -> invoke sym models
        Nothing  -> H.div [] [
                     (H.button [E.onClick (switchTo SystemCSym)] [H.text "システム設定へ"])
                    ]

view : Models -> Html MsgToRoot
view models = H.div [class [NavBar]] [
               H.header [] [
                    H.div [][H.text <| (++) "カウンター : " <| toString models.editable.counter]
                   ,if models.currentC == Nothing then H.span [] [] else H.button [E.onClick NoComponentSelected] [H.text "選択解除へ"]
                   ]
              ,componentView models
              ,H.footer [] []
              ]
invoke : ComponentSym -> Models -> Html MsgToRoot
invoke sym m =
    case sym of
        SystemCSym -> Html.map FromSystem <| components.system.view (m.system,m.readonly,m.editable)
