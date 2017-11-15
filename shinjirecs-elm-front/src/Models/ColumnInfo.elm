module Models.ColumnInfo exposing (ColumnInfo,columnInfoDecoder,minimum,maximum)
import Json.Decode as D
import Utils.Maybe exposing (or)

type alias ColumnInfo = { nullable  : Bool
                        , default   : Maybe String
                        , limit     : Maybe Int
                        , precision : Maybe Int
                        , scale     : Maybe Int
                        , tipe      : String
                        , maximum   : Maybe Int
                        , minimum   : Maybe Int
                        }

columnInfoDecoder : D.Decoder ColumnInfo
columnInfoDecoder =
    D.map8
        ColumnInfo
        (D.at ["nullable"]  D.bool)
        (D.at ["default"]   (D.nullable D.string))
        (D.at ["limit"]     (D.nullable D.int))
        (D.at ["precision"] (D.nullable D.int))
        (D.at ["scale"]     (D.nullable D.int))
        (D.at ["type"]      D.string)
        (D.at ["maximum"]   (D.nullable D.int))
        (D.at ["minimum"]   (D.nullable D.int))
            

minimum : ColumnInfo -> Maybe Int
minimum info =
    case info.tipe of
        "string"   -> or info.minimum info.limit
        "text"     -> or info.minimum info.limit
        "integer"  -> or info.minimum <| Maybe.map (\byte -> if byte < 1 then 0 else negate <| 2^(byte * 8 - 1)) info.limit
        "float"    -> info.minimum 
        _          -> Nothing

maximum : ColumnInfo -> Maybe Int
maximum info =
    case info.tipe of
        "string"   -> or info.minimum info.limit
        "text"     -> or info.minimum info.limit
        "integer"  -> or info.minimum <| Maybe.map (\byte -> (\x -> x - 1) <| if byte < 1 then 1 else 2^(byte * 8 - 1)) info.limit
        "float"    -> info.minimum 
        _          -> Nothing
