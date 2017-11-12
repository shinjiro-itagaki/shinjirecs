import Html exposing (Html,div,input,text,li,Attribute,{- beginnerProgram -} program)
import Html.Events exposing (on,keyCode,onInput)
import Html.Attributes exposing (type_,value)
import Json.Decode as Json
import Html.CssHelpers exposing (withNamespace)
import MainCssInterface as Css exposing (CssClasses(NavBar),CssIds(Page),mainCssLink)

import Ports exposing (sendToJs,receiveFromJs)

-- program : { init : (model, Cmd msg),
 --            update : msg -> model -> (model, Cmd msg),
 --            subscriptions : model -> Sub msg,
 --            view : model -> Html msg }
 -- -> Program Never model msg

-- main : Program Never model msg
main = program { init = init, view = view, update = update, subscriptions = subscriptions }
       {- beginnerProgram { model = model, view = view, update = update } -} 
       
type Msg = Input String | Fail | Enter
type alias Model = {list : List String, value : String}
    
model : Model
model = {list=[],value=""}

init : (Model, Cmd Msg)
init =
    (model, sendToJs "init" )

subscriptions : Model -> Sub Msg
subscriptions m = Sub.none

-----Update
-- msg -> model -> (model, Cmd msg),
update : Msg -> Model -> (Model, Cmd Msg)
update msg ({list,value} as m) =
    case msg of
        Input str -> ({m | value = str}, Cmd.none)
        Enter     -> ({m | list = list ++ [value] , value = ""}, sendToJs (value ++ " was given") )
        Fail      -> (m, Cmd.none)

{ id, class, classList } = withNamespace "root"

---view
view : Model -> Html Msg
view {list,value} =
    div [ class [ NavBar ]]
        [ listStr list
        , yourName value
        , textField value]
        
listStr : List String -> Html Msg
listStr m =
    let toList a = li [] [text a ]
    in div [] <| List.map toList m

yourName : String -> Html Msg
yourName value =
    div [] [text <| "こんにちは　" ++ value ++ "　さん！"]

textField : String -> Html Msg
textField v =
    input [ type_ "text"
          , onInput Input
          , value v
          , onEnter Fail Enter ] []
                

onEnter : msg -> msg -> Attribute msg
onEnter v_at_fail v_at_success =
    let
        tagger code =
            if code == 13 then v_at_success
            else v_at_fail
    in
        on "keyup" (Json.map tagger keyCode)
