
module Main where

-- import Lib
import App

main :: IO ()
-- main = someFunc
main = do
  -- scotty 3000 $ do
  App.app 3000
