module Day3Spec where

import PreludeAoC
import Day3
import Test.Hspec

spec :: Spec
spec = do
  describe "solutions" $ do
    it "part 1" $
      solution1 `shouldReturn` Just 2180
    it "part 2" $
      solution2 `shouldReturn` Just 112316
