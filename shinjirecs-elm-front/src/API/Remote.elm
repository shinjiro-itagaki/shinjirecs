module API.Remote exposing (getImpl)
import String exposing (join)
import API.Types as T exposing (..)
import Http exposing (get,post,jsonBody)
import Http.Progress exposing (Progress,track)
import Utils.Http as Http exposing (patch,delete,head,options)
import Json.Decode as D
import Json.Encode as E
import Time exposing (Time)
import Records.ColumnInfo exposing (ColumnInfo,columnInfoDecoder)
import Records.Types exposing (Entity,entitiesToDict)
import Utils.Json exposing (Encoder)
import Records.System  exposing (System  ,systemDecoder  ,systemEncoder)
import Records.Area exposing (Area, areaDecoder, areaEncoder)
import Records.Channel exposing (Channel ,channelDecoder ,channelEncoder)
import Records.EpgProgram exposing (EpgProgram, epgProgramDecoder, epgProgramEncoder)
import Records.EpgProgramCategory exposing (EpgProgramCategory, epgProgramCategoryDecoder, epgProgramCategoryEncoder)
import Records.ProgramSeries exposing (ProgramSeries, programSeriesDecoder, programSeriesEncoder)
import Records.Reservation exposing (Reservation, reservationDecoder, reservationEncoder)
import Dict exposing (Dict)
import Json.Decode.Pipeline exposing (decode,optional,required)

httpCommon : D.Decoder a -> (D.Decoder a -> Http.Request a) -> Cmd (Result Http.Error a)
httpCommon d mkReq =
    let req = mkReq <| D.field "body" d
    in Http.send (\res -> res) req

httpCommonAsync : D.Decoder a -> (D.Decoder a -> Http.Request a) -> Sub (Progress a)
httpCommonAsync d mkReq =
    let req = mkReq <| D.field "body" d
    in track "" (\x -> x) req

httpHead : String -> D.Decoder a -> Cmd (Result Http.Error a)
httpHead s d = httpCommon d <| Http.head s

httpHeadAsync : String -> D.Decoder a -> Sub (Progress a)
httpHeadAsync s d = httpCommonAsync d <| Http.head s
          
httpOptions : String -> D.Decoder a -> Cmd (Result Http.Error a)
httpOptions s d = httpCommon d <| Http.options s

httpOptionsAsync : String -> D.Decoder a -> Sub (Progress a)
httpOptionsAsync s d = httpCommonAsync d <| Http.options s
          
httpGet : String -> D.Decoder a -> Cmd (Result Http.Error a)
httpGet s d = httpCommon d <| Http.get s

httpGetAsync : String -> D.Decoder a -> Sub (Progress a)
httpGetAsync s d = httpCommonAsync d <| Http.get s
          
httpPost : String -> Http.Body -> D.Decoder a -> Cmd (Result Http.Error a)
httpPost s b d = httpCommon d <| Http.post s b

httpPostAsync : String -> Http.Body -> D.Decoder a -> Sub (Progress a)
httpPostAsync s b d = httpCommonAsync d <| Http.post s b
          
httpPatch : String -> Http.Body -> D.Decoder a -> Cmd (Result Http.Error a)
httpPatch s b d = httpCommon d <| Http.patch s b

httpPatchAsync : String -> Http.Body -> D.Decoder a -> Sub (Progress a)
httpPatchAsync s b d = httpCommonAsync d <| Http.patch s b

httpDelete : String -> D.Decoder a -> Cmd (Result Http.Error a)
httpDelete s d = httpCommon d <| Http.delete s

httpDeleteAsync : String -> D.Decoder a -> Sub (Progress a)
httpDeleteAsync s d = httpCommonAsync d <| Http.delete s

mkEntityDecoder : Maybe Int -> D.Decoder a -> D.Decoder (Entity a)
mkEntityDecoder maybe_id decoder =
    case maybe_id of
        Just id -> D.map2 Entity (D.succeed id)      decoder
        Nothing -> D.map2 Entity (D.at ["id"] D.int) decoder
    
    
rIndex : String -> String -> D.Decoder a -> Maybe Int -> Cmd (Result Http.Error (List (Entity a)))
rIndex domain path decoder mlimit = httpGet (join "/" [domain,path,""]) (D.list <| mkEntityDecoder Nothing decoder)

rIndexAsync : String -> String -> D.Decoder a -> Maybe Int -> Sub (Progress (List (Entity a)))
rIndexAsync domain path decoder mlimit = httpGetAsync (join "/" [domain,path,""]) (D.list <| mkEntityDecoder Nothing decoder)
                                           
rGet : String -> String -> D.Decoder a -> Int -> Cmd (Result Http.Error a)
rGet domain path decoder id  = httpGet (join "/" [domain,path,(toString id)]) decoder

rGetAsync : String -> String -> D.Decoder a -> Int -> Sub (Progress a)
rGetAsync domain path decoder id = httpGetAsync (join "/" [domain,path,(toString id)]) decoder
                               
rCreate : String -> String -> D.Decoder a -> Encoder a -> a -> Cmd (Result Http.Error (Entity a))
rCreate domain path decoder encoder val = httpPost (join "/" [domain,path]) (jsonBody <| E.object[("record",encoder val)]) (mkEntityDecoder Nothing decoder)

rCreateAsync : String -> String -> D.Decoder a -> Encoder a -> a -> Sub (Progress (Entity a))
rCreateAsync domain path decoder encoder val = httpPostAsync (join "/" [domain,path]) (jsonBody <| E.object[("record",encoder val)]) (mkEntityDecoder Nothing decoder)
                                          
rModify : String -> String -> D.Decoder a -> Encoder a -> Entity a -> Cmd (Result Http.Error a)
rModify domain path decoder encoder e = httpPatch (join "/" [domain,path,(toString e.id)]) (jsonBody <| E.object[("record", encoder e.val)]) decoder

rModifyAsync : String -> String -> D.Decoder a -> Encoder a -> Entity a -> Sub (Progress a)
rModifyAsync domain path decoder encoder e = httpPatchAsync (join "/" [domain,path,(toString e.id)]) (jsonBody <| E.object[("record", encoder e.val)]) decoder                                                                               
rDestroy : String -> String -> D.Decoder a -> Entity a -> Cmd (Result Http.Error Bool)
rDestroy domain path decoder e = httpDelete (join "/" [domain,path,(toString e.id)]) D.bool

rDestroyAsync : String -> String -> D.Decoder a -> Entity a -> Sub (Progress Bool)
rDestroyAsync domain path decoder e = httpDeleteAsync (join "/" [domain,path,(toString e.id)]) D.bool

rInfo : String -> String -> D.Decoder ColumnInfo -> Cmd (Result Http.Error (Dict String ColumnInfo))
rInfo domain path decoder = httpGet (join "/" [domain,path,"params_info"]) (D.dict columnInfoDecoder)

rInfoAsync : String -> String -> D.Decoder ColumnInfo -> Sub (Progress (Dict String ColumnInfo))
rInfoAsync domain path decoder = httpGetAsync (join "/" [domain,path,"params_info"]) (D.dict columnInfoDecoder)
           
mkResourcesI : String -> String -> D.Decoder a -> Encoder a -> T.ResourcesI a
mkResourcesI domain path decoder encoder =
    { index   = rIndex    domain path decoder
    , get     = rGet      domain path decoder
    , create  = rCreate   domain path decoder encoder
    , modify  = rModify   domain path decoder encoder
    , destroy = rDestroy  domain path decoder
    , info    = rInfo     domain path columnInfoDecoder
    , indexAsync   = rIndexAsync    domain path decoder
    , getAsync     = rGetAsync      domain path decoder
    , createAsync  = rCreateAsync   domain path decoder encoder
    , modifyAsync  = rModifyAsync   domain path decoder encoder
    , destroyAsync = rDestroyAsync  domain path decoder
    , infoAsync    = rInfoAsync     domain path columnInfoDecoder
    }

mkAllEntityDecoder : D.Decoder T.Cache
mkAllEntityDecoder =
    decode T.Cache
    |> optional "system"                 (D.map Just <|                               mkEntityDecoder Nothing systemDecoder             ) Nothing
    |> optional "areas"                  (D.map (Just << entitiesToDict) <| D.list <| mkEntityDecoder Nothing areaDecoder               ) Nothing
    |> optional "channels"               (D.map (Just << entitiesToDict) <| D.list <| mkEntityDecoder Nothing channelDecoder            ) Nothing
    |> optional "epg_programs"           (D.map (Just << entitiesToDict) <| D.list <| mkEntityDecoder Nothing epgProgramDecoder         ) Nothing
    |> optional "epg_program_categories" (D.map (Just << entitiesToDict) <| D.list <| mkEntityDecoder Nothing epgProgramCategoryDecoder ) Nothing
    |> optional "epg_program_medium_categories" (D.map (Just << entitiesToDict) <| D.list <| mkEntityDecoder Nothing epgProgramCategoryDecoder ) Nothing
    |> optional "program_serieses"       (D.map (Just << entitiesToDict) <| D.list <| mkEntityDecoder Nothing programSeriesDecoder       ) Nothing
    |> optional "reservations"           (D.map (Just << entitiesToDict) <| D.list <| mkEntityDecoder Nothing reservationDecoder        ) Nothing
        
mkSystemI : String -> T.SystemI
mkSystemI domain =
    let path = "systems"
    in { get    = httpGet (join "/" [domain,path]) <| mkEntityDecoder Nothing systemDecoder
       , modify = rModify domain path systemDecoder systemEncoder
       , info   = rInfo   domain path columnInfoDecoder
       , all    = httpGet (join "/" [domain,path,"all"]) mkAllEntityDecoder
       , getAsync    = httpGetAsync (join "/" [domain,path]) (mkEntityDecoder Nothing systemDecoder)
       , modifyAsync = rModifyAsync domain path systemDecoder systemEncoder
       , infoAsync   = rInfoAsync   domain path columnInfoDecoder
       , allAsync    = httpGetAsync (join "/" [domain,path,"all"]) mkAllEntityDecoder
       }

mkAreasI : String -> T.AreasI
mkAreasI domain = mkResourcesI domain "areas" areaDecoder areaEncoder
        
mkChannelsI : String -> T.ChannelsI                   
mkChannelsI domain = mkResourcesI domain "channels" channelDecoder channelEncoder

-- httpPost (join "/" [domain,path]) (jsonBody <| E.object[("record",encoder val)]) (mkEntityDecoder Nothing decoder)
-- String -> Http.Body -> D.Decoder a -> Cmd (Result Http.Error a)
mkEpgProgramsExI : String -> T.EpgProgramsExI
mkEpgProgramsExI domain =
    let path = join "/" [domain,"epg_programs"]
    in { epgdump        = httpPost (join "/" [path,"epgdump"]) Http.emptyBody D.bool
       , newReservation = (\ep -> httpPost (join "/" [path,toString ep.id,"new_reservation"]) Http.emptyBody (mkEntityDecoder Nothing reservationDecoder))
       }

mkEpgProgramsI : String -> T.EpgProgramsI
mkEpgProgramsI domain = mkResourcesI domain "epg_programs" epgProgramDecoder epgProgramEncoder                        

mkEpgProgramCategoriesI : String -> T.EpgProgramCategoriesI
mkEpgProgramCategoriesI domain = mkResourcesI domain "epg_program_categories" epgProgramCategoryDecoder epgProgramCategoryEncoder

mkProgramSeriesI : String -> T.ProgramSeriesI
mkProgramSeriesI domain = mkResourcesI domain "program_series" programSeriesDecoder programSeriesEncoder

mkReservationsI : String -> T.ReservationsI
mkReservationsI domain = mkResourcesI domain "reservations" reservationDecoder reservationEncoder
                          
getImpl : String -> T.API
getImpl domain = { system   = mkSystemI   domain
                 , areas    = mkAreasI    domain
                 , channels = mkChannelsI domain
                 , epgPrograms = mkEpgProgramsI domain
                 , epgProgramsEx = mkEpgProgramsExI domain
                 , epgProgramCategories = mkEpgProgramCategoriesI domain
                 , programSeries = mkProgramSeriesI domain
                 , reservations  = mkReservationsI domain
                 }
