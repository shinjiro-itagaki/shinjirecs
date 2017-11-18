module API.Remote exposing (getImpl)
import String exposing (join)
import API.Types as T exposing (..)
import Http exposing (get,post,jsonBody)
import Json.Decode as D
import Time exposing (Time)
import Records.ColumnInfo exposing (ColumnInfo,columnInfoDecoder)
import Records.Types exposing (Entity)
import Utils.Json exposing (Encoder)
import Records.System  exposing (System  ,systemDecoder  ,systemEncoder)
import Records.Channel exposing (Channel ,channelDecoder ,channelEncoder)
import Dict exposing (Dict)

httpCommon : D.Decoder a -> (D.Decoder a -> Http.Request a) -> Cmd (Result Http.Error a)
httpCommon d mkReq =
    let req = mkReq <| D.field "body" d
    in Http.send (\res -> res) req

httpHead : String -> D.Decoder a -> Cmd (Result Http.Error a)
httpHead s d = httpCommon d <| Http.get s
          
httpOptions : String -> D.Decoder a -> Cmd (Result Http.Error a)
httpOptions s d = httpCommon d <| Http.get s
          
httpGet : String -> D.Decoder a -> Cmd (Result Http.Error a)
httpGet s d = httpCommon d <| Http.get s
          
httpPost : String -> Http.Body -> D.Decoder a -> Cmd (Result Http.Error a)
httpPost s b d = httpCommon d <| Http.post s b
          
httpPatch : String -> Http.Body -> D.Decoder a -> Cmd (Result Http.Error a)
httpPatch s b d = httpCommon d <| Http.post s b

httpDelete : String -> D.Decoder a -> Cmd (Result Http.Error a)
httpDelete s d = httpCommon d <| Http.get s

mkEntityDecoder : Maybe Int -> D.Decoder a -> D.Decoder (Entity a)
mkEntityDecoder maybe_id decoder =
    case maybe_id of
        Just id -> D.map2 Entity (D.succeed id)      decoder
        Nothing -> D.map2 Entity (D.at ["id"] D.int) decoder
    
    
rIndex : String -> String -> D.Decoder a -> Maybe Int -> Cmd (Result Http.Error (List (Entity a)))
rIndex domain path decoder mlimit = httpGet (join "/" [domain,path,"index"]) (D.list <| mkEntityDecoder Nothing decoder)
                                           
rGet : String -> String -> D.Decoder a -> Int -> Cmd (Result Http.Error a)
rGet domain path decoder id  = httpGet (join "/" [domain,path,(toString id)]) decoder
                                           
rCreate : String -> String -> D.Decoder a -> Encoder a -> a -> Cmd (Result Http.Error (Entity a))
rCreate domain path decoder encoder val = httpPost (join "/" [domain,path]) (jsonBody <| encoder val) (mkEntityDecoder Nothing decoder)
                                           
rModify : String -> String -> D.Decoder a -> Encoder a -> Entity a -> Cmd (Result Http.Error a)
rModify domain path decoder encoder e = httpPatch (join "/" [domain,path,(toString e.id)]) (jsonBody <| encoder e.val) decoder
                                           
rDestroy : String -> String -> D.Decoder a -> Entity a -> Cmd (Result Http.Error Bool)
rDestroy domain path decoder e = httpDelete (join "/" [domain,path,(toString e.id)]) D.bool

rInfo : String -> String -> D.Decoder ColumnInfo -> Cmd (Result Http.Error (Dict String ColumnInfo))
rInfo domain path decoder = httpGet (join "/" [domain,path,"params_info"]) (D.dict columnInfoDecoder)
           
mkResourcesI : String -> String -> D.Decoder a -> Encoder a -> T.ResourcesI a
mkResourcesI domain path decoder encoder =
    { index   = rIndex    domain path decoder
    , get     = rGet      domain path decoder
    , create  = rCreate   domain path decoder encoder
    , modify  = rModify   domain path decoder encoder
    , destroy = rDestroy  domain path decoder
    , info    = rInfo     domain path columnInfoDecoder
    }

mkSystemI : String -> T.SystemI
mkSystemI domain =
    let path = "systems"
    in { get    = httpGet (join "/" [domain,path]) <| mkEntityDecoder Nothing systemDecoder
       , modify = rModify domain path systemDecoder systemEncoder
       , info   = rInfo   domain path columnInfoDecoder
       }
                   
mkChannelsI : String -> T.ChannelsI                   
mkChannelsI domain = mkResourcesI domain "channels" channelDecoder channelEncoder
        
getImpl : String -> T.API
getImpl domain = { system   = mkSystemI   domain
                 , channels = mkChannelsI domain }

