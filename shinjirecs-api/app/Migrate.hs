module Main where
import App
import Config
import System.Environment(getArgs)
import Class.Castable

main :: IO ()
main = do
  args <- getArgs
  App.migrate $ from $ if length args > 0 then head args else ""
  

