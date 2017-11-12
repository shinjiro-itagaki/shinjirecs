import Html exposing (Html,div,input,text,li,Attribute,beginnerProgram)
import Html.Events exposing (on,keyCode,onInput)
import Html.Attributes exposing (type_,value)
import Json.Decode as Json
import Html.CssHelpers exposing (withNamespace)
import MainCssInterface as Css exposing (CssClasses(NavBar),CssIds(Page),mainCssLink)

-- main : Program Never model msg
main = beginnerProgram { model = model, view = view, update = update }
       
type Msg = Input String | Fail | Enter
type alias Model = {list : List String, value : String}
    
model : Model
model = {list=[],value=""}

-----Update
update : Msg -> Model -> Model
update msg ({list,value} as model) =
    case msg of
        Input str -> {model | value = str}
        Enter -> {model | list = list ++ [value] , value = ""}
        Fail -> model

{ id, class, classList } = withNamespace "root"

---view
view {list,value} =
    div [ class [ NavBar ]]
        [ listStr list
        , yourName value
        , textField value]
        

listStr model =
    let toList a = li [] [text a ]
    in div [] <| List.map toList model
        
yourName value =
    div [] [text <| "こんにちは　" ++ value ++ "　さん！"]
        
textField v =
    input [ type_ "text"
          , onInput Input
          , value v
          , onEnter Fail Enter ] []
                

onEnter : msg -> msg -> Attribute msg
onEnter fail success =
    let
        tagger code =
            if code == 13 then success
            else fail
    in
        on "keyup" (Json.map tagger keyCode)
