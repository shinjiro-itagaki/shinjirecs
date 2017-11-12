port module MainCss exposing (main)
import Css.File exposing (CssFileStructure, CssCompilerProgram)
import MainCssInterface exposing (cssFileName)
import MainCssImpl exposing (css)

port files : CssFileStructure -> Cmd msg

fileStructure : CssFileStructure
fileStructure =
    Css.File.toFileStructure
        [ ( cssFileName, Css.File.compile [ css ] ) ]
main : CssCompilerProgram
main =
    Css.File.compiler files fileStructure
