module PluralizeSpec (spec) where

import Test.Hspec

-- Produce the given string with 's' added at the end.
pluralize :: String -> String
pluralize "" = ""
pluralize x = x ++ "s"

spec :: Spec
spec = do
  describe "Double" $ do
    it "should double any number" $ do
       (pluralize "")  `shouldBe` ""
       pluralize "apple" `shouldBe` "apples"
       pluralize "car" `shouldBe` "cars"
