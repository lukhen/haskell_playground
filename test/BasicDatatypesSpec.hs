
module BasicDatatypesSpec (spec) where

import Test.Hspec
import BasicDatatypes

spec :: Spec
spec = do
  describe "Double" $ do
    it "" $ do
       (Blah :: Mood) `shouldBe` Blah
       show (Town 100 100) `shouldBe` "Town {women = 100, men = 100}"
       Town 100 100 `shouldBe` Town 100 100
       Town 100 200 `shouldNotBe` Town 100 100
       inhabitants (Town 100 200) `shouldBe` 300
       addInhabitants (Town 10 20) (Town 20 30) `shouldBe` 80
       addInhabitants (Town 10 20) (GirlDormitory 5 "") `shouldBe` 35
