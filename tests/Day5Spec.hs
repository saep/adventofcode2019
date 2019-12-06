module Day5Spec where

import PreludeAoC
import IntCodeInterpreter
import Day5
import Test.Hspec
import RIO.List.Partial (last)

spec :: Spec
spec = do
  pure ()
  describe "solutions" $ do
    it "part 1" $ 
      fmap (last . fst) solution1 `shouldReturn` InterpreterOutput 15097178
    it "part 2" $
      fmap (last . fst) solution2 `shouldReturn` InterpreterOutput 1558663
