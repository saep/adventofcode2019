module Day2Spec where

import PreludeAoC
import Day2
import Test.Hspec

spec :: Spec
spec = do
  describe "solutions" $ do
    it "part 1" $
      solution1 `shouldReturn` 4714701
    it "part 2" $
      solution2 `shouldReturn` 5121
