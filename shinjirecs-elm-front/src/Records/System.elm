module Records.System exposing (System, SystemId,systemDecoder,systemEncoder,new,ColumnTarget(..))
import Time exposing (Time)
import Json.Decode as D
import Json.Encode as E
import Utils.Json exposing (map9,Encoder)
import Records.Types exposing (ChannelType(GR,BS))
import Result exposing (map,mapError)

type ColumnTarget = AreaId | Active | Setup | TunerCount ChannelType | RestTunerCount ChannelType

toBool : String -> Result String Bool
toBool = Result.map (\x -> x > 0) << String.toInt
    
updateSystem : System -> ColumnTarget -> String -> Result (String,ColumnTarget) System
updateSystem rec target valstr =
    let common : (a -> System) -> (String -> Result String a) -> Result (String,ColumnTarget) System
        common f1 f2 = Result.map f1 <| mapError (\s -> (s,target)) <| f2 valstr
    in case target of
           AreaId            -> common (\v -> {rec | area_id             = v} ) String.toInt
           Active            -> common (\v -> {rec | active              = v} ) toBool
           Setup             -> common (\v -> {rec | setup               = v} ) toBool
           TunerCount GR     -> common (\v -> {rec | gr_tuner_count      = v} ) String.toInt
           TunerCount BS     -> common (\v -> {rec | bs_tuner_count      = v} ) String.toInt
           RestTunerCount GR -> common (\v -> {rec | rest_gr_tuner_count = v} ) String.toInt
           RestTunerCount BS -> common (\v -> {rec | rest_bs_tuner_count = v} ) String.toInt
    
type SystemId = SystemId Int
type alias System =
    { area_id : Int
    , active : Bool
    , setup  : Bool
    , gr_tuner_count : Int
    , bs_tuner_count : Int
    , rest_gr_tuner_count : Int
    , rest_bs_tuner_count : Int
    , created_at : Time
    , updated_at : Time
    }
    
new : System
new = { area_id = 0
      , active = False
      , setup  = False
      , gr_tuner_count = 0
      , bs_tuner_count = 0
      , rest_gr_tuner_count = 0
      , rest_bs_tuner_count = 0
      , created_at = 0
      , updated_at = 0
      }
    
systemDecoder : D.Decoder System
systemDecoder =
    map9
        System
        (D.at ["area_id"] D.int)
        (D.at ["active"] D.bool)
        (D.at ["setup"] D.bool)
        (D.at ["gr_tuner_count"] D.int)
        (D.at ["bs_tuner_count"] D.int)
        (D.at ["rest_gr_tuner_count"] D.int)
        (D.at ["rest_bs_tuner_count"] D.int)
        (D.at ["created_at"] D.float)
        (D.at ["updated_at"] D.float)

systemEncoder : Encoder System
systemEncoder x = E.object
                  [ ("area_id", E.int  x.area_id)
                  , ("active" , E.bool x.active )
                  , ("setup"  , E.bool x.setup  )
                  , ("gr_tuner_count", E.int x.gr_tuner_count)
                  , ("bs_tuner_count", E.int x.bs_tuner_count)
                  , ("rest_gr_tuner_count", E.int x.rest_gr_tuner_count)
                  , ("rest_bs_tuner_count", E.int x.rest_bs_tuner_count)
                  , ("created_at", E.float x.created_at)
                  , ("updated_at", E.float x.updated_at)
                  ]
