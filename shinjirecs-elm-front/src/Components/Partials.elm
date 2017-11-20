module Components.Partials exposing (..)
import Records.ColumnInfo as C exposing (..)
import Html as H exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events as E exposing (..)
import Utils.Maybe exposing (or,catMaybes)
import Utils.DateTime exposing (weekdayLabel,weekdayToInt,weekdays,monthToInt,months,weekdayFlagsToInt,intToWeekdayFlags)
import Dict exposing (Dict)
import Utils.Either exposing (Either(Left,Right))

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
                 
defaultInput : (String,C.ColumnInfo,Maybe String) -> ((List (Attribute msg) -> List (Html msg) -> Html msg) , List (Attribute msg) ,  List (Html msg))
defaultInput (name,info,mval) =
    let aname = A.name name
        attr_value = case mval of
                         Nothing -> []
                         Just v  -> [A.value v]                
        attrs = (++) ([aname] ++ attr_value) <| catMaybes <| List.map (\f -> f info) [attr_maximum, attr_minimum, attr_required]
        stringsToOption s = H.option [A.value <| s] [H.text <| s]
        integersToOptions = List.map (stringsToOption << toString)
        maybe_options = Maybe.map integersToOptions <| Maybe.map2 List.range info.minimum info.maximum
    in case info.tipe of
           C.StringT   -> (H.input    , attrs ++ [A.type_ "text"]          ,[])
           C.TextT     -> (H.textarea , attrs ++ [A.type_ "text"]          ,[])
           C.IntegerT  ->
               case maybe_options of
                   Just options -> (H.select   , attrs                       ,options)
                   Nothing      -> (H.input    , attrs ++ [A.type_ "number"] ,[])
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
            
mkInput : Maybe String -> String -> C.ColumnInfo -> (String -> msg) -> H.Html msg
mkInput mval name info to_msg = (\(f,attrs,children) -> f ([E.onInput to_msg, E.onCheck <| to_msg << (\b -> if b then "1" else "0" )] ++ attrs) children ) <| defaultInput (name,info,mval)

formByColumns : Dict String (C.ColumnInfo,(String -> msg)) -> Dict String String -> Maybe Int -> H.Html msg
formByColumns colmap valmap mid =
    H.form [] [ 
         H.dl [] <| List.concat <| List.map (\(nm,(info,f)) -> [ H.dt [] [H.text nm]
                                                               , H.dd [] [mkInput (Dict.get nm valmap) nm info f]
                                                               ]
                                            ) <| Dict.toList colmap
        ,H.button [] [H.text "保存"]
        ]

-- type Partial = SetValue String String | Submited

type Request = SetValue String String | Submit (Dict String (Either String (List String)))
type Response msg = Succeed | BadRequest (Dict String String) | RedirectTo msg

type Input = StringInput   String String
           | IntInput      String Int
           | DateInput     String (Int,Int,Int)
           | TimeInput     String (Int,Int,Int)
           | DateTimeInput String (Int,Int,Int,Int,Int,Int)
    
--mkForm : List (String,(C.ColumnInfo,Maybe String)) -> (Request -> Response msg) -> () -> H.Html msg
--mkForm info = 
--    let f = 1
--    in 

type SampleMsg = Multi String
type alias SampleModel = {val : String}

sample : SampleModel -> (SampleMsg -> msg) -> H.Html msg
sample m f = H.map f <|
             H.div [] [H.input [ A.type_ "number"
                               , E.onInput (\v -> Multi v)
                               ] []
                      ,H.div [] [text <| m.val]
                      ]

