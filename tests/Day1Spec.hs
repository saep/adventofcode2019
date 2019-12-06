module Day1Spec where

import PreludeAoC
import Day1
import Test.Hspec

spec :: Spec
spec = do
  describe "solutions" $ do
    it "part 1" $
      fmap fst solutions `shouldReturn` 3361976
    it "part 2" $
      fmap snd solutions `shouldReturn` 5040085
