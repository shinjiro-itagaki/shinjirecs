port module MainCss exposing (main)
import Css exposing (Style,property,property,hex,color,important,inlineBlock,display,children,zero,padding,margin,class,px,borderBox,boxSizing,pct,height,width,rgb,backgroundColor,minWidth,id,overflowX,auto,stylesheet)
import Css.Namespace exposing (namespace)
import Css.Elements exposing (body, li)
import Css.File exposing (CssFileStructure, CssCompilerProgram)

import MainCssInterface exposing (CssClasses(NavBar),CssIds(Page),cssFileName)

port files : CssFileStructure -> Cmd msg

fileStructure : CssFileStructure
fileStructure =
    Css.File.toFileStructure
        [ ( cssFileName, Css.File.compile [ css ] ) ]

zIndex : Int -> Style
zIndex i =
    property "z-index" <| toString i

primaryAccentColor =
    hex "ccffaa"

main : CssCompilerProgram
main =
    Css.File.compiler files fileStructure
      
css =
    (stylesheet << namespace "dreamwriter")
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
    ]        
