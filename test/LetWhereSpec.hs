module LetWhereSpec where

import LetWhere
import Test.Hspec

x :: Integer
x = let y = 5
        z = 10
        in
      y + z

a :: Integer
a = b + c where
  b = 100
  c = 200


d :: Integer
d = 2 + let func :: Integer -> Integer -> Integer
            func p q = p ^ q
            in
          func 2 3

-- let is an expression
-- where is a declaration

spec :: Spec
spec = do
  describe "let and where" $ do
    it "" $ do
      add2_1 2 `shouldBe` 4
      x `shouldBe` 15
      a `shouldBe` 300
      d `shouldBe` 10
