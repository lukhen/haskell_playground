module DoubleSpec (spec) where

import Test.Hspec

-- Produce 2 times the given number
double :: (Num a) => a -> a
double x = x*2

spec :: Spec
spec = do
  describe "Double" $ do
    it "should double any number" $ do
       (double 2::Int)  `shouldBe` 4
       (double 2.2::Double) `shouldBe` 4.4
