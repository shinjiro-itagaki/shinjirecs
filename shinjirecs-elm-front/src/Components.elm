module Components exposing (root)
import Html exposing (Html,program)
import Html as H
import Components.SystemModel exposing (SystemModel)
import Components.Types exposing (Component,CommonModelReadOnly,CommonModelEditable,PublicRootMsg(DirectMsg,HasCmd,SendRequest,DoNothing,UpdateModel,UpdateAPICache),Request(ToSystemReq,NoSelect))
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

type PrivateRootMsg = ReplaceAPICache (Result Http.Error Cache)
type RootMsg = Private PrivateRootMsg | Public PublicRootMsg

{ id, class, classList } = withNamespace "root"
                          
root str =
    program { init = init str --init
            , view = view -- view
            , update = update -- update
            , subscriptions = subscriptions --subscriptions
            }

httpErrorToMsg : Http.Error -> String
httpErrorToMsg err =
    case err of
        BadUrl url -> "BadUrl: " ++ url
        Timeout -> "Timeout"
        NetworkError -> "NetworkError"
        BadStatus response -> response.body
        BadPayload str response -> "bad payload error message = " ++ str ++ " , body = " ++ response.body

updateCache : PrivateModel -> Cache -> PrivateModel
updateCache pm newcache =
    let models = pm.m
        r = models.readonly
    in { pm | m = {models| readonly = {r|cache = newcache}}}

setErrMsg : PrivateModel -> String -> PrivateModel
setErrMsg pm errmsg =
    let models = pm.m
        rw = models.editable
    in { pm | m = {models| editable = {rw| errmsg = errmsg}}}

setHttpErrMsg : PrivateModel -> Http.Error -> PrivateModel
setHttpErrMsg pm httperr = setErrMsg pm <| httpErrorToMsg httperr
        
type alias PrivateModel =
    { m : Models
    , f : (Models -> Html PublicRootMsg)
    , components : { system : Component SystemModel SystemMsg.ActionType }
    , req : Request
    }
    
dispatch : Request -> PrivateModel -> PublicRootMsg
dispatch req pm =
    case req of
        NoSelect -> DoNothing
        ToSystemReq tipe -> pm.components.system.accept tipe pm.m

replaceAnyModel : Request -> Models -> Models -> Models
replaceAnyModel req old new =
    case req of
        NoSelect -> old
        ToSystemReq _ -> {old| system = new.system }

updatePrivateModel : PrivateModel -> Models -> PrivateModel
updatePrivateModel oldpm rtnm =
    let oldm = oldpm.m
        newm = replaceAnyModel oldpm.req oldm rtnm -- replace only a model of target component
        newpm = {oldpm| m = { newm | editable = rtnm.editable }}
    in newpm

update : RootMsg -> PrivateModel -> (PrivateModel, Cmd RootMsg)
update msg oldpm =
    let updatePrivateModel_ = updatePrivateModel oldpm
    in case msg of
           Public public ->
               case public of
                   UpdateModel rtnm -> (updatePrivateModel_ rtnm, Cmd.none)
                   DirectMsg rtnm f  -> ((\x -> {x| f = f}) <| updatePrivateModel_ rtnm, Cmd.none)
                   HasCmd cmd        -> (oldpm,Cmd.map Public cmd)
                   SendRequest req   -> case req of
                                            NoSelect -> ({oldpm| req=req, f = emptyView },Cmd.none)
                                            _        -> update (Public <| dispatch req oldpm) {oldpm|req=req}
                   DoNothing -> (oldpm,Cmd.none)
                   UpdateAPICache -> (oldpm, Cmd.map (Private << ReplaceAPICache) oldpm.m.readonly.api.system.all)
           Private private ->
               case private of
                   ReplaceAPICache res ->
                       case res of
                           Ok cache    -> (updateCache   oldpm cache   ,Cmd.none)
                           Err httperr -> (setHttpErrMsg oldpm httperr ,Cmd.none)

view : PrivateModel -> Html RootMsg
view pm = H.div [class [NavBar]] [
            H.header [] [
                 H.div [][H.text <| (++) "カウンター : " <| toString pm.m.editable.counter]
                ,if pm.req == NoSelect then H.span [] [H.text <| "何も選択されていない"] else H.button [E.onClick <| Public <| SendRequest NoSelect] [H.text "選択解除へ"]
                ,H.button [E.onClick <| Public <| SendRequest <| ToSystemReq IndexAction] [H.text "システム設定へ"]
                ]
           ,Html.map Public <| pm.f pm.m
           ,H.div [] <| case pm.m.editable.errmsg of
                            ""     -> []
                            errmsg -> [H.text <| errmsg]
           ,H.footer [] []
           ]



emptyView _ = H.div [] []
          
init : String -> (PrivateModel, Cmd RootMsg)
init address =
    let systemC = SystemC.new
        x = { m = { system = systemC.init
                      , readonly = { config = 1, api = getAPI address, httpErrorToString = httpErrorToMsg , cache=emptyCache}
                      , editable = { counter = 0, errmsg = "" }
                      }
                , f = emptyView
                , components = {system = systemC}
                , req = NoSelect
                }
    in (x, Cmd.none)

subscriptions : PrivateModel -> Sub RootMsg
subscriptions m = Sub.none            
