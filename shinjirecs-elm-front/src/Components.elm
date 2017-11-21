module Components exposing (root)
import Html exposing (Html,program)
import Html as H
import Components.SystemC exposing (SystemC)
import Components.SystemModel exposing (SystemModel)
-- import Components.ReservationC exposing (ReservationC,ReservationModel)
import Components.Types exposing (Component,NextMsg(ToRoot,NextCmd,Direct,NoNext),RootMsg(SwitchTo,NoComponentSelected,ShowHttpError,RefreshAPICache),ComponentSym(SystemCSym),CommonModelReadOnly,CommonModelEditable,RootMsg2(DirectMsg,HasCmd,SendRequest,DoNothing,UpdateModel2),Request(ToSystemReq))
import MainCssInterface as Css exposing (CssClasses(NavBar),CssIds(Page),mainCssLink)
import Html.CssHelpers exposing (withNamespace)
import Html.Events as E
import List exposing (singleton)
import API exposing (getAPI)
import API.Types exposing (Cache, emptyCache)
import Components.Types exposing (Models)
import Components.SystemC as SystemC
import Components.SystemMsg exposing (SystemMsg)
import Components.SystemMsg as SystemMsg exposing (ActionType(IndexAction,ShowAction,EditAction,ModifyAction))
import Http exposing (Error(BadUrl,Timeout,NetworkError,BadStatus,BadPayload))
import Result exposing (Result(Ok,Err))
import Json.Decode as D
import Utils.Either exposing (Either(Left,Right))
{ id, class, classList } = withNamespace "root"
                           
type alias Components = { system : SystemC }

-- type ComponentWrapper = SystemW      SystemC      SystemModel
--                      | ReservationW ReservationC ReservationModel
    
type PrivateRootMsg = ToRootPrivate RootMsg | ToSystem SystemMsg | UpdateModelAndNextMsg Models PrivateRootMsg | UpdateModel Models
    
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
               , readonly = { config = 1, api = getAPI "http://127.0.0.1:3000", httpErrorToString = httpErrorToMsg , cache=emptyCache}
               , editable = { counter = 0, errmsg = "" }
               }
       in (m, Cmd.none)

showErrMsg : Models -> String -> Models
showErrMsg models msg =
    let editable = models.editable
    in { models | editable = { editable | errmsg = msg}}

httpErrorToMsg : Http.Error -> String
httpErrorToMsg err =
    case err of
        BadUrl url -> "BadUrl: " ++ url
        Timeout -> "Timeout"
        NetworkError -> "NetworkError"
        BadStatus response -> response.body
        BadPayload str response -> "bad payload error message = " ++ str ++ " , body = " ++ response.body

updateCache : Models -> Cache -> Models
updateCache models newcache =
    let r = models.readonly
    in {models | readonly = {r|cache = newcache}}


{-        
updateImpl : Models -> Component model msg -> model -> msg -> (model -> models -> models) -> Either (Cmd (model,CommonModelEditable)) (model,CommonModelEditable)
updateImpl models comp model msg updater =
    let res = comp.update msg (model,models.readonly, models.editable)
    in case res of
           Right (m,rw) -> ({models | editable = rw, system = m}, Cmd.none)
           Left cmd     -> (models, Cmd.map (\(m,rw) -> UpdateModelAndNextMsg <| updater m <| {models | editable = rw} msg ) cmd)    
-}

update : PrivateRootMsg -> Models -> (Models, Cmd PrivateRootMsg)
update msg models =
    case msg of
        ToRootPrivate (SwitchTo sym) -> ({ models | currentC = Just sym }, Cmd.none)
        ToRootPrivate NoComponentSelected -> ({ models | currentC = Nothing }, Cmd.none)
        ToRootPrivate (ShowHttpError httperr) -> (showErrMsg models <| httpErrorToMsg httperr, Cmd.none)
        ToRootPrivate RefreshAPICache ->
            let updateCache_ = updateCache models
                showErrMsg_  = showErrMsg models
                caster res = UpdateModel <| case res of
                                                Ok newcache -> updateCache_ newcache
                                                Err httperr -> showErrMsg_ <| httpErrorToMsg httperr
            in (models, Cmd.map caster models.readonly.api.system.all)
        ToSystem system_msg ->
            let res = components.system.update system_msg (models.system,models.readonly, models.editable)
            in case res of
                   Right (m,rw) -> ({models | editable = rw, system = m}, Cmd.none)
                   Left cmd     -> (models, Cmd.map (\(m,rw) -> UpdateModelAndNextMsg {models| system=m, editable = rw} <| ToSystem system_msg ) cmd)
        UpdateModelAndNextMsg m nextmsg -> update nextmsg m
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
                    ,(H.button [E.onClick (ToRootPrivate RefreshAPICache)] [H.text "APIキャッシュ更新"])
                    ]

view : Models -> Html PrivateRootMsg
view models = H.div [class [NavBar]] [
               H.header [] [
                    H.div [][H.text <| (++) "カウンター : " <| toString models.editable.counter]
                   ,if models.currentC == Nothing then H.span [] [] else H.button [E.onClick noComponentSelected] [H.text "選択解除へ"]
                   ]
              ,componentView models
              ,H.div [] <| case models.editable.errmsg of
                             ""     -> []
                             errmsg -> [H.text <| errmsg]
              ,H.div [] [H.text <| case String.toInt "" of
                                     Ok i -> "ok = " ++ toString i
                                     Err s -> "err => " ++ s
                        ]
              ,H.footer [] []
              ]
invokeView : ComponentSym -> Models -> Html PrivateRootMsg
invokeView sym m =
    case sym of
        SystemCSym -> Html.map ToSystem <| components.system.view (m.system,m.readonly,m.editable)



type alias PrivateModel =
    { m : Models
    , f : (Models -> Html RootMsg2)
    , req : Request
    }
    
dispatch : Request -> Models -> RootMsg2
dispatch req models =
    case req of
        ToSystemReq tipe -> SystemC.accept tipe models

replaceAnyModel : Request -> Models -> Models -> Models
replaceAnyModel req old new =
    case req of
        ToSystemReq _ -> {old| system = new.system }

updatePrivateModel : PrivateModel -> Models -> PrivateModel
updatePrivateModel oldpm rtnm =
    let oldm = oldpm.m
        newm = replaceAnyModel oldpm.req oldm rtnm -- replace only a model of target component
        newpm = {oldpm| m = { newm | editable = rtnm.editable }}
    in newpm
                         
update2 : RootMsg2 -> PrivateModel -> (PrivateModel, Cmd RootMsg2)
update2 msg oldpm =
    let updatePrivateModel_ = updatePrivateModel oldpm
    in case msg of
        UpdateModel2 rtnm   -> (updatePrivateModel_ rtnm, Cmd.none)
        DirectMsg    rtnm f -> ((\x -> {x| f = f}) <| updatePrivateModel_ rtnm, Cmd.none)
        HasCmd cmd -> (oldpm,cmd)
        SendRequest req -> update2 (dispatch req oldpm.m) {oldpm|req=req}
        DoNothing -> (oldpm,Cmd.none)

view2 : PrivateModel -> Html RootMsg2
view2 pm = pm.f pm.m
