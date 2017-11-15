module API.Remote exposing (getImpl)
import String exposing (join)
import API.Types as T exposing (..)
import Http exposing (get,post,jsonBody)
import Json.Decode as D
import Time exposing (Time)
import Models.ColumnInfo exposing (ColumnInfo,columnInfoDecoder)
import Models.Types exposing (Entity,Encoder)
import Models.System  exposing (System  ,systemDecoder  ,systemEncoder)
import Models.Channel exposing (Channel ,channelDecoder ,channelEncoder)
import Dict exposing (Dict)

common : Http.Request a -> Cmd (Result Http.Error a)
common req =
    Http.send (\res -> res) req

head : String -> D.Decoder a -> Cmd (Result Http.Error a)
head s d = common <| Http.get s d
          
options : String -> D.Decoder a -> Cmd (Result Http.Error a)
options s d = common <| Http.get s d
          
get : String -> D.Decoder a -> Cmd (Result Http.Error a)
get s d = common <| Http.get s d
          
post : String -> Http.Body -> D.Decoder a -> Cmd (Result Http.Error a)
post s b d = common <| Http.post s b d
          
patch : String -> Http.Body -> D.Decoder a -> Cmd (Result Http.Error a)
patch s b d = common <| Http.post s b d

delete : String -> D.Decoder a -> Cmd (Result Http.Error a)
delete s d = common <| Http.get s d

mkEntityDecoder : Maybe Int -> D.Decoder a -> D.Decoder (Entity a)
mkEntityDecoder maybe_id decoder =
    case maybe_id of
        Just id -> D.map2 Entity (D.succeed id)      decoder
        Nothing -> D.map2 Entity (D.at ["id"] D.int) decoder
    
    
rIndex : String -> String -> D.Decoder a -> Maybe Int -> Cmd (Result Http.Error (List (Entity a)))
rIndex domain path decoder mlimit = get (join "/" [domain,path,"index"]) (D.list <| mkEntityDecoder Nothing decoder)
                                           
rGet : String -> String -> D.Decoder a -> Int -> Cmd (Result Http.Error a)
rGet domain path decoder id  = get (join "/" [domain,path,(toString id)]) decoder
                                           
rCreate : String -> String -> D.Decoder a -> Encoder a -> a -> Cmd (Result Http.Error (Entity a))
rCreate domain path decoder encoder val = post (join "/" [domain,path]) (jsonBody <| encoder val) (mkEntityDecoder Nothing decoder)
                                           
rModify : String -> String -> D.Decoder a -> Encoder a -> Entity a -> Cmd (Result Http.Error a)
rModify domain path decoder encoder e = patch (join "/" [domain,path,(toString e.id)]) (jsonBody <| encoder e.val) decoder
                                           
rDestroy : String -> String -> D.Decoder a -> Entity a -> Cmd (Result Http.Error Bool)
rDestroy domain path decoder e = delete (join "/" [domain,path,(toString e.id)]) D.bool

rInfo : String -> String -> D.Decoder ColumnInfo -> Cmd (Result Http.Error (Dict String ColumnInfo))
rInfo domain path decoder = get (join "/" [domain,path,"params_info"]) (D.dict columnInfoDecoder)
           
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
mkSystemI domain = mkResourcesI domain "systems"  systemDecoder  systemEncoder
                   
mkChannelsI : String -> T.ChannelsI                   
mkChannelsI domain = mkResourcesI domain "channels" channelDecoder channelEncoder
        
getImpl : String -> T.API
getImpl domain = { system   = mkSystemI   domain
                 , channels = mkChannelsI domain }

