module Day9Spec where

import PreludeAoC
import Test.Hspec

import Day9

spec :: Spec
spec = do
  describe "solutions" $ do
    it "solution 1" $ do
      solution1 `shouldReturn` [2204990589]
    it "solution 2" $ do
      solution2 `shouldReturn` [50008]
