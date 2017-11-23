module Main where
import Helper
import Config
import App
import Class.Castable(from)
import System.Environment(getArgs)

main :: IO ()
main = do
  args <- getArgs
  App.listen 3000 $ from $ if length args > 0 then head args else ""
