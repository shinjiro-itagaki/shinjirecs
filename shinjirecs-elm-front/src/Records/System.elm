module Records.System exposing (System, SystemId,systemDecoder,systemEncoder,new,ColumnTarget(..),stringToTarget,updateSystem)
import Time exposing (Time)
import Json.Decode as D
import Json.Encode as E
import Utils.Json exposing (map9,Encoder)
import Records.Types exposing (ChannelType(GR,BS))
import Result exposing (map,mapError)

type ColumnTarget = AreaId | Active | Setup | TunerCount ChannelType | RestTunerCount ChannelType

stringToTarget : String -> Maybe ColumnTarget
stringToTarget str =
    case str of
        "area_id"             -> Just AreaId
        "active"              -> Just Active
        "setup"               -> Just Setup
        "gr_tuner_count"      -> Just (TunerCount GR)
        "bs_tuner_count"      -> Just (TunerCount BS)
        "rest_gr_tuner_count" -> Just (RestTunerCount GR)
        "rest_bs_tuner_count" -> Just (RestTunerCount BS)
        _                     -> Nothing
    
toBool : String -> Result String Bool
toBool str =
    case String.toLower str of
        "on"    -> Ok True
        "t"     -> Ok True
        "true"  -> Ok True
        "yes"   -> Ok True
        "off"   -> Ok False
        "f"     -> Ok False
        "false" -> Ok False
        "no"    -> Ok False
        x       -> Result.map (\x -> x > 0) <| String.toInt x

common : String -> tgt -> rec -> (rec -> a -> rec) -> (String -> Result String a) -> Result (String,tgt) rec
common valstr target rec replacer caster = Result.map (replacer rec) <| mapError (\s -> (s,target)) <| caster valstr

updateSystem : System -> ColumnTarget -> String -> Result (String,ColumnTarget) System
updateSystem rec target valstr =
    let common_ = common valstr target rec
    in case target of
           AreaId            -> common_ replace_area_id             String.toInt
           Active            -> common_ replace_active              toBool
           Setup             -> common_ replace_setup               toBool
           TunerCount GR     -> common_ replace_gr_tuner_count      String.toInt
           TunerCount BS     -> common_ replace_bs_tuner_count      String.toInt
           RestTunerCount GR -> common_ replace_rest_gr_tuner_count String.toInt
           RestTunerCount BS -> common_ replace_rest_bs_tuner_count String.toInt

replace_area_id : System -> Int -> System
replace_area_id rec v = {rec | area_id = v }
replace_active : System -> Bool -> System
replace_active rec v = { rec | active = v }
replace_setup : System -> Bool -> System
replace_setup rec v = { rec | setup = v }
replace_gr_tuner_count : System -> Int -> System
replace_gr_tuner_count rec v = { rec | gr_tuner_count = v }
replace_bs_tuner_count : System -> Int -> System
replace_bs_tuner_count rec v = { rec | bs_tuner_count = v }
replace_rest_gr_tuner_count : System -> Int -> System
replace_rest_gr_tuner_count rec v = { rec | rest_gr_tuner_count = v }
replace_rest_bs_tuner_count : System -> Int -> System
replace_rest_bs_tuner_count rec v = { rec | rest_bs_tuner_count = v }

area_id : System -> Int
area_id rec = rec.area_id
active : System -> Bool
active rec = rec.active
setup : System -> Bool
setup rec = rec.setup
gr_tuner_count : System -> Int
gr_tuner_count rec = rec.gr_tuner_count
bs_tuner_count : System -> Int
bs_tuner_count rec = rec.bs_tuner_count
rest_gr_tuner_count : System -> Int
rest_gr_tuner_count rec = rec.rest_gr_tuner_count
rest_bs_tuner_count : System -> Int
rest_bs_tuner_count rec = rec.rest_bs_tuner_count
                                    
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
        (D.field "area_id"              D.int)
        (D.field "active"               D.bool)
        (D.field "setup"                D.bool)
        (D.field "gr_tuner_count"       D.int)
        (D.field "bs_tuner_count"       D.int)
        (D.field "rest_gr_tuner_count"  D.int)
        (D.field "rest_bs_tuner_count"  D.int)
        (D.field "created_at"           D.float)
        (D.field "updated_at"           D.float)

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
