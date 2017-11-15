module Components.Partials exposing (..)
import Models.ColumnInfo as C exposing (..)
import Html as H exposing (..)
import Html.Attributes as A exposing (..)
import Utils.Maybe exposing (or,catMaybes)
import Utils.DateTime exposing (weekdayLabel,weekdayToInt,weekdays,monthToInt,months,weekdayFlagsToInt,intToWeekdayFlags)
    
{-
  nullable  : Bool
, default   : Maybe String
, limit     : Maybe Int
, precision : Maybe Int
, scale     : Maybe Int
, tipe      : String
, maximum   : Maybe Int
, minimum   : Maybe Int
-}

required : C.ColumnInfo -> Bool
required info = (not info.nullable) && info.default == Nothing

attr_required : C.ColumnInfo -> Maybe (H.Attribute msg)
attr_required = Just << A.required << required

attr_maximum : C.ColumnInfo -> Maybe (H.Attribute msg)
attr_maximum info =
    let m_mkAttr = case info.tipe of
                       StringT   -> Just <| A.maxlength
                       TextT     -> Just <| A.maxlength 
                       IntegerT  -> Just <| A.max << toString
                       FloatT    -> Just <| A.max << toString
                       _          -> Nothing
    in
        Maybe.map2 (\f v -> f v) m_mkAttr (C.maximum info)
                      
attr_minimum : C.ColumnInfo -> Maybe (H.Attribute msg)
attr_minimum info =
    let m_mkAttr = case info.tipe of
                       StringT   -> Just <| A.minlength
                       TextT     -> Just <| A.minlength 
                       IntegerT  -> Just <| A.min << toString
                       FloatT    -> Just <| A.min << toString
                       _          -> Nothing
    in
        Maybe.map2 (\f v -> f v) m_mkAttr (C.minimum info)

monthOptions   = List.map (\m -> H.option [A.value <| toString <| monthToInt   m] [H.text <| toString <| monthToInt m] ) months
weekdayOptions = List.map (\w -> H.option [A.value <| toString <| weekdayToInt w] [H.text <| (weekdayLabel w).ja     ] ) weekdays

weekdayChecks = List.map (\w -> H.input [A.type_ "checkbox", A.value <| toString <| weekdayToInt w] [H.text <| (weekdayLabel w).ja] ) weekdays
                 
getInputConstructor : C.ColumnInfo -> ((List (Attribute msg) -> List (Html msg) -> Html msg) , List (Attribute msg) ,  List (Html msg))
getInputConstructor info =
    case info.tipe of
        C.StringT   -> (H.input    , [A.type_ "text"],[])
        C.TextT     -> (H.textarea , [A.type_ "text"],[])
        C.IntegerT  -> (H.input    , [A.type_ "number"],[])
        C.FloatT    -> (H.input    , [A.type_ "number"],[])
        C.DateT     -> (H.input    , [A.type_ "date"],[])
        C.TimeT     -> (H.input    , [A.type_ "time"],[])
        C.DateTimeT -> (H.input    , [A.type_ "datetime-local"],[])
        C.PasswordT -> (H.input    , [A.type_ "password"],[])
        C.HiddenT   -> (H.input    , [A.type_ "hidden"],[])
        C.BooleanT  -> (H.input    , [A.type_ "checkbox"],[])
        C.BelongsTo -> (H.select   , [],[])
        C.HasMany   -> (H.input    , [A.type_ "checkbox"],[])
        C.Tel       -> (H.input    , [A.type_ "tel"],[])
        C.URL       -> (H.input    , [A.type_ "url"],[])
        C.Email     -> (H.input    , [A.type_ "email"],[])
        C.Month     -> (H.select   , [], monthOptions)
        C.Week      -> (H.input    , [A.type_ "week"],[])
        C.Weeks     -> (H.div      , []              ,weekdayChecks)
        C.Range     -> (H.input    , [A.type_ "range"],[])
        C.Color     -> (H.input    , [A.type_ "color"],[])
            
mkInput : C.ColumnInfo -> H.Html msg
mkInput info =
    let
        attrs = catMaybes <| List.map (\f -> f info) [attr_maximum, attr_minimum, attr_required]
    in
        H.input attrs []

formByColumns : List (String, C.ColumnInfo) -> H.Html msg
formByColumns colmap =
    H.dl [] <| List.concat <| List.map (\(colname,info) -> [ H.dt [] [H.text colname], H.dd [] [mkInput info]] ) colmap
