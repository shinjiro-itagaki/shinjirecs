module API.Remote exposing (getImpl)
import API.Types as T exposing (..)
import Http exposing (get,post)
import Json.Decode exposing (list, string, Decoder)
import Time exposing (Time)

notImplemented : String -> String -> a -> a
notImplemented domain path x =
    let uri = String.join "/" [domain, path]
    in x

type Method = Head | Options | Get | Post | Patch | Delete

{-    
mkRequest : { method : Method
            , headers : List Http.Header
            , url : String
            , body : Http.Body
            , expect : Http.Expect a
            , timeout : Maybe Time
            , withCredentials : Bool } -> Http.Request a
mkRequest mkRequest x = Http.request { x | method = toString x.method }
-}

-- send : (Result Error a -> msg) -> Request a -> Cmd msg

common : Http.Request a -> Cmd (Result Http.Error a)
common req =
    Http.send (\res -> res) req

head : String -> Decoder a -> Cmd (Result Http.Error a)
head s d = common <| Http.get s d
          
options : String -> Decoder a -> Cmd (Result Http.Error a)
options s d = common <| Http.get s d
          
get : String -> Decoder a -> Cmd (Result Http.Error a)
get s d = common <| Http.get s d
          
post : String -> Http.Body -> Decoder a -> Cmd (Result Http.Error a)
post s b d = common <| Http.post s b d
          
patch : String -> Http.Body -> Decoder a -> Cmd (Result Http.Error a)
patch s b d = common <| Http.post s b d

delete : String -> Decoder a -> Cmd (Result Http.Error a)
delete s d = common <| Http.get s d

--mkIdPath : String -> Int -> String
--mkIdPath act id = join "/" act (toString id)
             
system : String -> T.System
system domain =
    let path = "systems"
    in
        { index   = notImplemented domain (path ++ "/index")
        , get     = notImplemented domain (path ++ "/")
        , create  = notImplemented domain path
        , modify  = notImplemented domain (path ++ "/")
        , destroy = notImplemented domain (path ++ "/") }

channels : String -> T.Channels
channels domain =
    let path = "channels"
    in
        { index   = notImplemented domain path
        , get     = notImplemented domain path
        , create  = notImplemented domain path
        , modify  = notImplemented domain path
        , destroy = notImplemented domain path }
        
getImpl : String -> T.API
getImpl domain = { system   = system   domain
                 , channels = channels domain }


{- 
type Msg2 = Click | NewBook (Result Http.Error String)

type Model = Dummy
model = Dummy
    
update : Msg2 -> Model -> Model
update msg model =
    case msg of
        Click ->
            ( model, getWarAndPeace )
        NewBook (Ok book) ->
            ( model, getWarAndPeace )
        NewBook (Err _) ->
            ( model, getWarAndPeace )
getWarAndPeace : Cmd Msg2
getWarAndPeace =
    Http.send NewBook <| Http.getString "https://example.com/books/war-and-peace.md"
                                                                                                                               
-}

    
