module Components.Partials exposing (..)
import Records.ColumnInfo as C exposing (..)
import Html as H exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events as E exposing (..)
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
                 
defaultInput : (String,C.ColumnInfo) -> ((List (Attribute msg) -> List (Html msg) -> Html msg) , List (Attribute msg) ,  List (Html msg))
defaultInput (name,info) =
    let aname = A.name name
        attrs = (++) [aname] <| catMaybes <| List.map (\f -> f info) [attr_maximum, attr_minimum, attr_required]
    in case info.tipe of
           C.StringT   -> (H.input    , attrs ++ [A.type_ "text"]          ,[])
           C.TextT     -> (H.textarea , attrs ++ [A.type_ "text"]          ,[])
           C.IntegerT  -> (H.input    , attrs ++ [A.type_ "number"]        ,[])
           C.FloatT    -> (H.input    , attrs ++ [A.type_ "number"]        ,[])
           C.DateT     -> (H.input    , attrs ++ [A.type_ "date"]          ,[])
           C.TimeT     -> (H.input    , attrs ++ [A.type_ "time"]          ,[])
           C.DateTimeT -> (H.input    , attrs ++ [A.type_ "datetime-local"],[])
           C.PasswordT -> (H.input    , attrs ++ [A.type_ "password"]      ,[])
           C.HiddenT   -> (H.input    , attrs ++ [A.type_ "hidden"]        ,[])
           C.BooleanT  -> (H.input    , attrs ++ [A.type_ "checkbox"]      ,[])
           C.BelongsTo -> (H.select   , attrs                              ,[])
           C.HasMany   -> (H.input    , attrs ++ [A.type_ "checkbox"]      ,[])
           C.Tel       -> (H.input    , attrs ++ [A.type_ "tel"]           ,[])
           C.URL       -> (H.input    , attrs ++ [A.type_ "url"]           ,[])
           C.Email     -> (H.input    , attrs ++ [A.type_ "email"]         ,[])
           C.Month     -> (H.select   , attrs                              ,monthOptions)
           C.Week      -> (H.input    , attrs ++ [A.type_ "week"]          ,[])
           C.Weeks     -> (H.div      , attrs                              ,weekdayChecks)
           C.Range     -> (H.input    , attrs ++ [A.type_ "range"]         ,[])
           C.Color     -> (H.input    , attrs ++ [A.type_ "color"]         ,[])
            
mkInput : String -> C.ColumnInfo -> (String -> msg) -> H.Html msg
mkInput name info to_msg = (\(f,attrs,children) -> f ([E.onInput to_msg, E.onCheck <| to_msg << (\b -> if b then "1" else "0" )] ++ attrs) children ) <| defaultInput (name,info)

formByColumns : List (String, C.ColumnInfo,(String -> msg)) -> H.Html msg
formByColumns colmap =
    H.form [] <| List.singleton <|
        H.dl [] <| List.concat <| List.map (\(nm,info,f) -> [ H.dt [] [H.text nm]
                                                  , H.dd [] [mkInput nm info f]
                                                  ]
                                           ) colmap
