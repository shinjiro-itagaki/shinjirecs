module Models.ColumnInfo exposing (ColumnInfo,columnInfoDecoder)
import Json.Decode as D

type alias ColumnInfo = { nullable  : Bool
                        , default   : String
                        , limit     : Maybe Int
                        , precision : Maybe Int
                        , scale     : Maybe Int
                        , tipe      : String
                        }

columnInfoDecoder : D.Decoder ColumnInfo
columnInfoDecoder =
    D.map6
        ColumnInfo
        (D.at ["nullable"]  D.bool)
        (D.at ["default"]   D.string)
        (D.at ["limit"]     (D.nullable D.int))
        (D.at ["precision"] (D.nullable D.int))
        (D.at ["scale"]     (D.nullable D.int))
        (D.at ["type"]      D.string)
            
