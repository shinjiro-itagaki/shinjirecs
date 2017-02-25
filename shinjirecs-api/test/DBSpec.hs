module DBSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Controllers.Channels

spec :: Spec
spec = do
  describe "DB.hs test" $ do
    it "INT associative law" $ do
      putStrLn "not yet implemented"
    it "INT identity element" $ do
      putStrLn "not yet implemented"
    it "Int Inverse Element" $ do
      (+) 5 6 `shouldBe` 11
