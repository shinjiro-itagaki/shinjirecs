module Records.Channel exposing (Channel,ChannelId,channelDecoder,channelTypeDecoder,channelEncoder,enables)
import Records.Types exposing (ChannelType(BS,GR),Entity)
import Time exposing (Time)
import Json.Decode as D
import Json.Encode as E
import Json.Decode.Pipeline exposing (decode,required,optional)
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
    { number : String
    , area_id : Int
    , ctype : ChannelType
    , display_name : String
    , order : Int
    , enable : Bool
    , exist : Bool
    , scaned : Bool
    , created_at : Time
    , updated_at : Time
    }

channelTypeDecoder : D.Decoder ChannelType
channelTypeDecoder = D.string |> D.andThen (D.succeed << toType)
    
channelDecoder : D.Decoder Channel
channelDecoder =
    decode Channel
        |> required "number"        D.string
        |> required "area_id"       D.int
        |> required "ctype"         channelTypeDecoder
        |> required "display_name"  D.string
        |> required "order"         D.int
        |> required "enable"        D.bool
        |> required "exist"         D.bool
        |> required "scaned"        D.bool
        |> required "created_at"    D.float
        |> required "updated_at"    D.float

channelEncoder : Encoder Channel
channelEncoder x =
    E.object
        [ ("number"       , E.string x.number)
        , ("area_id"      , E.int x.area_id)
        , ("ctype"        , E.string <| typeToString x.ctype)
        , ("display_name" , E.string x.display_name)
        , ("order"        , E.int x.order)
        , ("enable"       , E.bool x.enable)
        , ("created_at"   , E.float x.created_at)
        , ("updated_at"   , E.float x.updated_at)
        ]

enables : List (Entity Channel) -> List (Entity Channel)
-- enables = List.filter (\x -> x.val.enable && x.val.exist)
enables = List.filter (\x -> x.val.exist)

gr : List (Entity Channel) -> List (Entity Channel)
gr = List.filter (\x -> x.val.ctype == GR)

bs : List (Entity Channel) -> List (Entity Channel)
bs = List.filter (\x -> x.val.ctype == BS)
