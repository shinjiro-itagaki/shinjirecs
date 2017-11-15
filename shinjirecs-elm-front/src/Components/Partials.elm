module Components.Partials exposing (..)
import Models.ColumnInfo as C exposing (..)
import Html as H exposing (..)
import Html.Attributes as A exposing (..)
import Utils.Maybe exposing (or,catMaybes)
    
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
                       "string"   -> Just <| A.maxlength
                       "text"     -> Just <| A.maxlength 
                       "integer"  -> Just <| A.max << toString
                       "float"    -> Just <| A.max << toString
                       _          -> Nothing
    in
        Maybe.map2 (\f v -> f v) m_mkAttr (C.maximum info)
                      
attr_minimum : C.ColumnInfo -> Maybe (H.Attribute msg)
attr_minimum info =
    let m_mkAttr = case info.tipe of
                       "string"   -> Just <| A.minlength
                       "text"     -> Just <| A.minlength 
                       "integer"  -> Just <| A.min << toString
                       "float"    -> Just <| A.min << toString
                       _          -> Nothing
    in
        Maybe.map2 (\f v -> f v) m_mkAttr (C.minimum info)

mkInput : C.ColumnInfo -> H.Html msg
mkInput info =
    let
        attrs = catMaybes <| List.map (\f -> f info) [attr_maximum, attr_minimum, attr_required]
    in
        H.input attrs []

formByColumns : List (String, C.ColumnInfo) -> H.Html msg
formByColumns colmap =
    H.dl [] <| List.concat <| List.map (\(colname,info) -> [ H.dt [] [H.text colname], H.dd [] [mkInput info]] ) colmap
