module Models.Channel exposing (Channel)
import Time exposing (Time)

type ChannelType = GR | BS

toType : String -> ChannelType
toType "bs" = BS
toType _    = GR
    
type Channel =
    { id : Int
    , number : Int
    , area_id : Int
    , ctype : ChannelType
    , display_name : String
    , order : Int
    , created_at : Time
    , updated_at : Time
    }
