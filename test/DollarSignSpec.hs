{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module DollarSignSpec where

import Test.Hspec


pow2 :: Integer -> Integer
pow2 = (^2)

add2 :: Integer -> Integer
add2 = (+2)

mul2 :: Integer -> Integer
mul2 = (*2)

spec :: Spec
spec = do
  describe "infix" $ do
    it "" $ do
      2^2^3 `shouldBe` 2^(2^3)  -- right assocativity means that the left side is transformed to right side (/w parens)
      2^2^3 `shouldNotBe` (2^2)^3
      2+2+2+2 `shouldBe` ((2+2)+2)+2 -- left associativity
      
      (^3) 2 `shouldBe` 8
      (3^) 2 `shouldBe` 9
      (/100) 10 `shouldBe` 0.1
      (100/) 10 `shouldBe` 10
      pow2 (add2 3) `shouldBe` (pow2 $ add2 3)
      pow2 (add2 (mul2 1)) `shouldBe` (pow2 $ add2 $ mul2 1) 
