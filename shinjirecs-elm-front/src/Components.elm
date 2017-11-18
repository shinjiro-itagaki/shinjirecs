module Components exposing (root)
import Html exposing (Html,program)
import Html as H
import Components.SystemC exposing (SystemC,SystemModel)
import Components.Types exposing (NextMsg(ToRoot,NextCmd,Direct,NoNext),RootMsg(SwitchTo,NoComponentSelected,ShowHttpError),ComponentSym(SystemCSym),CommonModelReadOnly,CommonModelEditable)
import MainCssInterface as Css exposing (CssClasses(NavBar),CssIds(Page),mainCssLink)
import Html.CssHelpers exposing (withNamespace)
import Html.Events as E
import List exposing (singleton)
import API exposing (getAPI)
import Components.SystemC as SystemC
import Components.SystemMsg exposing (SystemMsg)
import Http exposing (Error(BadUrl,Timeout,NetworkError,BadStatus,BadPayload))
import Result exposing (Result(Ok,Err))
import Json.Decode as D
import Utils.Either exposing (Either(Left,Right))
{ id, class, classList } = withNamespace "root"
                           
type alias Components = { system : SystemC }
type alias Models = { currentC : Maybe ComponentSym
                    , readonly : CommonModelReadOnly
                    , editable : CommonModelEditable
                    , system : SystemModel
                    }

type PrivateRootMsg = ToRootPrivate RootMsg | ToSystem SystemMsg | UpdateModel Models
    
components : Components
components = { system = SystemC.new }

root = program { init = init
               , view = view
               , update = update
               , subscriptions = subscriptions
               }

init : (Models, Cmd PrivateRootMsg)
init = let x = components
           m = { currentC = Nothing
               , system = x.system.init
               , readonly = { config = 1, api = getAPI "http://127.0.0.1:3000", httpErrorToString = httpErrorToMsg }
               , editable = { counter = 0, errmsg = Nothing }
               }
       in (m, Cmd.none)

showErrMsg : Models -> String -> Models
showErrMsg models msg =
    let editable = models.editable
    in { models | editable = { editable | errmsg = Just msg}}

httpErrorToMsg : Http.Error -> String
httpErrorToMsg err =
    case err of
        BadUrl url -> "BadUrl: " ++ url
        Timeout -> "Timeout"
        NetworkError -> "NetworkError"
        BadStatus response -> response.body
        BadPayload str response -> "bad payload error message = " ++ str ++ " , body = " ++ response.body
            
update : PrivateRootMsg -> Models -> (Models, Cmd PrivateRootMsg)
update msg models =
    let i = 0
    in case msg of
           ToRootPrivate rootmsg ->
               case rootmsg of
                   SwitchTo sym -> ({ models | currentC = Just sym }, Cmd.none)
                   NoComponentSelected -> ({ models | currentC = Nothing }, Cmd.none)
                   ShowHttpError httperr -> (showErrMsg models <| httpErrorToMsg httperr, Cmd.none)
           ToSystem system_msg ->
               let res = components.system.update system_msg (models.system,models.readonly, models.editable)
               in case res of
                      Right (m,rw) -> ({models | editable = rw, system = m}, Cmd.none)
                      Left cmd     -> (,) models <| Cmd.map (\(m,rw) -> UpdateModel {models | editable = rw, system = m}) cmd
           UpdateModel m -> (m,Cmd.none)

subscriptions : Models -> Sub PrivateRootMsg
subscriptions m = Sub.none

switchTo : ComponentSym -> PrivateRootMsg
switchTo = ToRootPrivate << SwitchTo

noComponentSelected : PrivateRootMsg
noComponentSelected = ToRootPrivate NoComponentSelected
           
componentView : Models -> Html PrivateRootMsg
componentView models =
    case models.currentC of
        Just sym -> invokeView sym models
        Nothing  -> H.div [] [
                     (H.button [E.onClick (switchTo SystemCSym)] [H.text "システム設定へ"])
                    ]

view : Models -> Html PrivateRootMsg
view models = H.div [class [NavBar]] [
               H.header [] [
                    H.div [][H.text <| (++) "カウンター : " <| toString models.editable.counter]
                   ,if models.currentC == Nothing then H.span [] [] else H.button [E.onClick noComponentSelected] [H.text "選択解除へ"]
                   ]
              ,componentView models
              ,H.div [] <| case models.editable.errmsg of
                             Just errmsg -> [H.text <| errmsg]
                             Nothing     -> []
              ,H.footer [] []
              ]
invokeView : ComponentSym -> Models -> Html PrivateRootMsg
invokeView sym m =
    case sym of
        SystemCSym -> Html.map ToSystem <| components.system.view (m.system,m.readonly,m.editable)
