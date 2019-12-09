module Day8Spec where

import PreludeAoC
import Test.Hspec

import Day8

spec :: Spec
spec = do
  describe "solutions" $ do
    it "part 1" $ do
      solution1 `shouldReturn` Just 2760
