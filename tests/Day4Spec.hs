module Day4Spec where

import PreludeAoC
import Day4
import Test.Hspec

spec :: Spec
spec = do
  describe "solutions" $ do
    it "part 1" $
      solution1 `shouldBe` 1178
    it "part 2" $
      solution2 `shouldBe` 763
