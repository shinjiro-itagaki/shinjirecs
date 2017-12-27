module MainCssInterface exposing (CssClasses(..),CssIds(..),cssFileName,mainCssLink)

import Html.CssHelpers exposing (stylesheetLink)

cssFileName : String
cssFileName = "main.css"

-- mainCssLink : Html msg
mainCssLink = stylesheetLink cssFileName
              
type CssClasses
    = NavBar | EpgHovered

type CssIds
    = Page

