module Records.ColumnInfo exposing (ColumnInfo,columnInfoDecoder,minimum,maximum,Typ(..))
import Json.Decode as D
import Utils.Maybe exposing (or)
import Utils.Json exposing (map9)

type Typ = StringT | TextT | PasswordT | HiddenT | IntegerT | FloatT | DateT | TimeT | DateTimeT | BooleanT | BelongsTo | HasMany | Tel | URL | Email | Month | Week | Weeks | Range | Color
    
stringToTyp : String -> Typ
stringToTyp s =
    case s of
        "string"     -> StringT
        "text"       -> TextT
        "password"   -> PasswordT
        "hidden"     -> HiddenT
        "integer"    -> IntegerT
        "float"      -> FloatT
        "date"       -> DateT
        "time"       -> TimeT
        "datetime"   -> DateTimeT
        "boolean"    -> BooleanT
        "belongs_to" -> BelongsTo
        "has_many"   -> HasMany
        "tel"        -> Tel
        "url"        -> URL
        "email"      -> Email
        "month"      -> Month
        "week"       -> Week
        "weeks"      -> Weeks
        "range"      -> Range
        "color"      -> Color                        
        _            -> StringT

type alias ColumnInfo = { nullable  : Bool
                        , default   : Maybe String
                        , limit     : Maybe Int
                        , precision : Maybe Int
                        , scale     : Maybe Int
                        , tipe      : Typ -- String
                        , maximum   : Maybe Int
                        , minimum   : Maybe Int
                        , list      : Maybe (List String)
                        }

columnInfoDecoder : D.Decoder ColumnInfo
columnInfoDecoder =
    map9
        ColumnInfo
        (D.field "nullable"  (D.bool))
        (D.field "default"   (D.maybe D.string))
        (D.field "limit"     (D.maybe D.int))
        (D.field "precision" (D.maybe D.int))
        (D.field "scale"     (D.maybe D.int))
        (D.field "type"      (D.map stringToTyp D.string))
        (D.field "maximum"   (D.maybe D.int))
        (D.field "minimum"   (D.maybe D.int))
        (D.field "list"      (D.maybe <| D.list D.string))
            

minimum : ColumnInfo -> Maybe Int
minimum info =
    case info.tipe of
        StringT   -> or info.minimum info.limit
        TextT     -> or info.minimum info.limit
        IntegerT  -> or info.minimum <| Maybe.map (\byte -> if byte < 1 then 0 else negate <| 2^(byte * 8 - 1)) info.limit
        FloatT    -> info.minimum 
        _         -> Nothing

maximum : ColumnInfo -> Maybe Int
maximum info =
    case info.tipe of
        StringT   -> or info.maximum info.limit
        TextT     -> or info.maximum info.limit
        IntegerT  -> or info.maximum <| Maybe.map (\byte -> (\x -> x - 1) <| if byte < 1 then 1 else 2^(byte * 8 - 1)) info.limit
        FloatT    -> info.maximum 
        _         -> Nothing
