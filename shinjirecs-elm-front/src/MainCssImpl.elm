module MainCssImpl exposing (root)
import Css exposing (Style,property,property,hex,color,important,inlineBlock,display,zero,padding,margin,px,borderBox,boxSizing,pct,height,width,rgb,backgroundColor,minWidth,overflowX,auto,hover)
import MainCssInterface exposing (CssClasses(NavBar,EpgHovered),CssIds(Page))
import Css.Foreign exposing (global,body, li, id, class, children)
import Html exposing (Html)
import Html.Styled exposing (toUnstyled)

zIndex : Int -> Style
zIndex i =
    property "z-index" <| toString i

primaryAccentColor =
    hex "ccffaa"

root : Html msg
root = toUnstyled <| global css_root
      
css_root = 
    [ body
          [ overflowX auto
          , minWidth (px 1280)
          ]
    , id Page
        [ backgroundColor (rgb 200 128 64)
        , color (hex "CCFFFF")
        , width (pct 100)
        , height (pct 100)
        , boxSizing borderBox
        , padding (px 8)
        , margin zero
        ]
    , class NavBar
        [ margin zero
        , padding zero
        , children
              [ li
                [ (display inlineBlock) |> important
                , color primaryAccentColor
                ]
              ]
        ]
    , class EpgHovered
        [ hover
              [
               margin zero
              ,backgroundColor <| hex "ffcc00"
              ]
        ]
    ]
