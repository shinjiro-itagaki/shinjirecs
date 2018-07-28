import Html exposing (Html,div,input,text,li,Attribute,{- beginnerProgram -} program)
import Html.Events exposing (on,keyCode,onInput)
import Html.Attributes exposing (type_,value)
import Json.Decode as Json
import Html.CssHelpers exposing (withNamespace)
import MainCssInterface as Css exposing (CssClasses(NavBar),CssIds(Page),mainCssLink)

import Ports exposing (sendToJs,receiveFromJs,newState)
import API exposing (getAPI)
import Components exposing (root)

type Msg = Input String | Fail | Enter | Subscribed String
    
main = root "." -- "http://127.0.0.1:3000"

{-        
type alias Model = {list : List String, value : String }
    
model : Model
model = {list=[],value=""}

init : (Model, Cmd Msg)
init =
    (model, sendToJs "init" )

subscriptions : Model -> Sub Msg
subscriptions m = newState Subscribed

-----Update
-- msg -> model -> (model, Cmd msg),
update : Msg -> Model -> (Model, Cmd Msg)
update msg ({list,value} as m) =
    case msg of
        Input str -> ({m | value = str}, Cmd.none)
        Enter     -> ({m | list = list ++ [value] , value = ""}, sendToJs (value ++ " was given") )
        Fail      -> (m, Cmd.none)
        Subscribed str -> (m, Cmd.none)

{ id, class, classList } = withNamespace "root"

view2 : Model -> Html Msg
view2 {list,value} =
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
-}
