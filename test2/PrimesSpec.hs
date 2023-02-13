{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module PrimesSpec (spec) where

import Test.Hspec

divides :: Integer -> Integer -> Bool
divides d n = rem n d == 0

half :: Integer -> Integer
half n = n `div `2
-- Look for: how to divide integers,
-- How to convert integer to CInt

spec :: Spec
spec = do
  describe "Double" $ do
    it "" $ do
       divides 2 3  `shouldBe` False
       half 100 `shouldBe` 50
       half 101 `shouldBe` 50
       half 103 `shouldBe` 52
