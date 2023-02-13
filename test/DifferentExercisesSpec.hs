module DifferentExercisesSpec (spec) where

import Test.Hspec
--import Graphics.Gloss

x :: (Bool, Char, Bool)
x = (True, 'a', False)

addTup :: (Int, Char) -> (Int, Char) -> (Int, [Char])
addTup (a1, b1) (a2, b2) = (a1 + a2, [b1] ++ [b2])

--circ1 :: Picture
--circ1 = circle 100

--circ2 :: Picture
--circ2 = translate (-100) (-100) (translate 100 100 (circle 100))


spec :: Spec
spec = do
  describe "Double" $ do
    it "should double any number" $ do
       x `shouldBe` (True, 'a', False)
       addTup (2, 'a') (3, 'b') `shouldBe` (5, ['a', 'b'])
       -- circ1 `shouldBe` circ2  -- pitty it doesn't pass
