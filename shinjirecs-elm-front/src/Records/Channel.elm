module Records.Channel exposing (Channel,ChannelId,channelDecoder,channelTypeDecoder,channelEncoder)
import Time exposing (Time)
import Json.Decode as D
import Json.Encode as E
import Utils.Json exposing (map9,Encoder)

type ChannelType = GR | BS

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
        (D.at ["number"] D.int)
        (D.at ["area_id"] D.int)
        (D.at ["ctype"] channelTypeDecoder)
        (D.at ["display_name"] D.string)
        (D.at ["order"] D.int)
        (D.at ["created_at"] D.float)
        (D.at ["updated_at"] D.float)

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
