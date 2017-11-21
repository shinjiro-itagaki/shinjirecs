module Utils.Http exposing (patch,delete,head,options)
import Json.Decode as D
import Http exposing (Request,Body,request,emptyBody,expectJson)

raw url decoder =
    { method = "GET"
    , headers = []
    , url = url
    , body = emptyBody
    , expect = expectJson decoder
    , timeout = Nothing
    , withCredentials = False
    }

patch : String -> Body -> D.Decoder a -> Request a
patch   url body decoder = request <| (\r -> {r | method = "PATCH", body = body } ) <| raw url decoder
delete  url      decoder = request <| (\r -> {r | method = "DELETE"             } ) <| raw url decoder
head    url      decoder = request <| (\r -> {r | method = "HEAD"               } ) <| raw url decoder
options url      decoder = request <| (\r -> {r | method = "OPTIONS"            } ) <| raw url decoder
