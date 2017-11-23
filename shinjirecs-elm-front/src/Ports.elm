port module Ports exposing(..)
port sendToJs : String -> Cmd msg
port receiveFromJs : (String -> msg) -> Sub msg
port newState : (String -> msg) -> Sub msg
