module Records.Channel exposing (Channel,ChannelId,channelDecoder,channelTypeDecoder,channelEncoder)
import Records.Types exposing (ChannelType(BS,GR))
import Time exposing (Time)
import Json.Decode as D
import Json.Encode as E
import Utils.Json exposing (map9,Encoder)

toType : String -> ChannelType
toType s =
    case s of
        "bs" -> BS
        _    -> GR -- "gr"

typeToString : ChannelType -> String
typeToString t =
    case t of
        GR -> "gr"
        BS -> "bs"
                
type ChannelId = ChannelId Int
type alias Channel =
    { number : Int
    , area_id : Int
    , ctype : ChannelType
    , display_name : String
    , order : Int
    , created_at : Time
    , updated_at : Time
    }

channelTypeDecoder : D.Decoder ChannelType
channelTypeDecoder = D.string |> D.andThen (D.succeed << toType)
    
channelDecoder : D.Decoder Channel
channelDecoder =
    D.map7
        Channel
        (D.field "number"        D.int)
        (D.field "area_id"       D.int)
        (D.field "ctype"         channelTypeDecoder)
        (D.field "display_name"  D.string)
        (D.field "order"         D.int)
        (D.field "created_at"    D.float)
        (D.field "updated_at"    D.float)

channelEncoder : Encoder Channel
channelEncoder x =
    E.object
        [ ("number"       , E.int x.number)
        , ("area_id"      , E.int x.area_id)
        , ("ctype"        , E.string <| typeToString x.ctype)
        , ("display_name" , E.string x.display_name)
        , ("order"        , E.int x.order)
        , ("created_at"   , E.float x.created_at)
        , ("updated_at"   , E.float x.updated_at)
        ]
